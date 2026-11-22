#!/usr/bin/env python3
"""Refit frozen audit datasets and goldens, checking the public log-density seam.

Reserve one CPU slot per job. --family accepts comma-separated family IDs.
Completed probes are cached independently of the JSONL output.
The timeout is an audit guard, never a fitter status or a claim of nonexistence.
"""
import argparse, collections, gzip, hashlib, json, math, os, re, shutil, signal
import subprocess, sys, tempfile
from concurrent.futures import ThreadPoolExecutor
from functools import lru_cache
from fractions import Fraction
from pathlib import Path
from strict_common import ROOT, HERE, ARCHIVE, cached_map, file_fingerprint, fingerprint, pin_threads, positive_jobs

pin_threads()
import mpmath as mp
import sympy as sp
import yaml
from quality.truth import DISTS, read
read = lru_cache(maxsize=None)(read)
from seam_reference import reference
WORK = ROOT/'build/scratch/seam-check'
CACHE = ROOT/'build/strict/seam.sqlite3'

def error(a,b):
    if a == b: return 0.
    if not math.isfinite(a) or not math.isfinite(b): return math.inf
    return abs(a-b)/max(1,abs(b))

def adapters():
    out={}
    for folder in sorted(DISTS.iterdir()):
        if not (folder/'golden.json').exists(): continue
        text=(folder/'test.cpp').read_text()
        part=text.split('mltest::check_dpqr_truth("'+folder.name+'",')
        if len(part)<2: raise ValueError(('missing density adapter',folder.name))
        sig,call=re.findall(r'\[\]\(([^\n]*?)\)\s*\{\s*return ([^;]+);\s*\}',part[1])[0]
        names=[re.search(r'(\w+)\s*$',a)[1] for a in sig.split(',')]
        mapping=dict(zip(names,['x','p','log']))
        out[folder.name]=re.sub(r'\b\w+\b',lambda m:mapping.get(m[0],m[0]),call)
    return out

def support_violations(meta, parameters, data):
    """Closed-support comparisons using exact binary64 rational arithmetic."""
    names=[p['name'] for p in meta['params']]
    exact=lambda v: Fraction(float(v)) if math.isfinite(float(v)) else float(v)
    values=dict(zip(names,map(exact,parameters)))
    def bound(text):
        text=str(text)
        match=re.fullmatch(r'if\s*\(([^()]*)\)\s*(.*?)\s+else\s+(.*)',text)
        if match:
            condition,yes,no=match.groups()
            return bound(yes if bound(condition) else no)
        for name in sorted(names,key=len,reverse=True):
            text=re.sub(r'\b'+re.escape(name)+r'\b', '_p['+repr(name)+']',text)
        return eval(text,{'__builtins__':{}},dict(_p=values,min=min,max=max,
            floor=lambda v:math.floor(v) if math.isfinite(v) else v,
            Inf=math.inf,inf=math.inf))
    lo,hi=bound(meta['support']['lower']),bound(meta['support']['upper'])
    return [i for i,x in enumerate(data) if exact(x)<lo or exact(x)>hi or
        (meta['support']['type']=='integer' and x!=math.floor(x))]

def probe_paths(families, double, source_id):
    tag = hashlib.sha256(','.join(families).encode()).hexdigest()[:12]+'-'+source_id[:12]
    return WORK/('probe-'+tag+'.cpp'), WORK/('probe-'+tag+('-double' if double else ''))


def boost_include():
    return subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()


def compiler_flags(double, boost):
    return ['-std=c++17', '-O2', *(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []),
            '-I'+str(ROOT), '-I'+str(ROOT/'univariateML-cpp/include'), '-I'+boost]


def probe_source(families, ads):
    blocks=[]
    src='#include <iostream>\n#include <iomanip>\n#include <algorithm>\n#include <cstdlib>\n'
    for ident in families:
        src+=f'#include "bindings/common/distributions/{ident}/fit.hpp"\n#include "cppunivariate/distributions/{ident}/dpqr.hpp"\n'
        meta,_=read(DISTS/ident)
        header=(ROOT/f'bindings/common/distributions/{ident}/fit.hpp').read_text()
        integer=bool(re.search(r'fit_'+ident+r'\(cppunivariate::ispan',header))
        typ,span=('int','ispan') if integer else ('double','span')
        args=','.join(f'umvl_binding::Held{{bool(mask&{1<<i}),holds[{i}]}}' for i in range(len(meta['params'])))
        # Retain the adapter defaults, including controls: this audit changes no fit.
        blocks.append(f'''if(id=="{ident}"){{std::vector<{typ}> data;for(double v:input)data.push_back(v);
        auto r=umvl_binding::fit_{ident}(cppunivariate::{span}{{data.data(),data.size()}},{args});
        if(!r.error.empty()){{std::cout<<"stop\\t"<<r.error<<std::endl;continue;}}
        std::vector<double>p;for(const char* name:{{{','.join('"'+p['name']+'"' for p in meta['params'])}}}){{
        auto it=std::find_if(r.estimates.begin(),r.estimates.end(),[&](const auto&v){{return v.first==name;}});p.push_back(it==r.estimates.end()?NAN:it->second);}}
        std::cout<<"ok\\t"<<r.logLik;for(double v:p)std::cout<<'\\t'<<v;
        for(double x:input){{bool log=true;std::cout<<'\\t'<<({ads[ident]});}}std::cout<<std::endl;continue;}}''')
    src+='''int main(){std::cout<<std::setprecision(17);std::string id,s;unsigned mask;int n,k;
    while(std::cin>>id>>mask>>k>>n){std::vector<double>holds(k),input(n);
    for(double&v:holds){std::cin>>s;v=std::strtod(s.c_str(),nullptr);}for(double&v:input){std::cin>>s;v=std::strtod(s.c_str(),nullptr);}
    '''+''.join(blocks)+'}}\n'
    return src


def write_source(source, text):
    with tempfile.TemporaryDirectory(dir=WORK) as staging:
        temporary = Path(staging)/source.name
        temporary.write_text(text)
        temporary.replace(source)


def compile_probe(source, exe, flags):
    with tempfile.TemporaryDirectory(dir=WORK) as staging:
        staged_exe = Path(staging)/exe.name
        subprocess.run(['ccache', 'c++', *flags,
                        str(source), '-o', str(staged_exe)], check=True)
        staged_exe.replace(exe)


def build(families, double=False, source_id="", boost=None):
    """Build the requested family set for the archived evaluation drivers."""
    WORK.mkdir(parents=True, exist_ok=True)
    source, exe = probe_paths(families, double, source_id)
    write_source(source, probe_source(families, adapters()))
    compile_probe(source, exe, compiler_flags(double, boost_include() if boost is None else boost))
    return exe


def cases(families, archive_bytes=None):
    if archive_bytes is None:
        archive_bytes = ARCHIVE.read_bytes()
    for c in json.loads(gzip.decompress(archive_bytes)):
        if c['family'] in families: yield dict(c,source='archive')
    for ident in families:
        meta,g=read(DISTS/ident)
        held=g.get('fit_args',{})
        # fit_args holds required/explicit coefficients; omit control arguments.
        held={p['name']:held[p['name']] for p in meta['params'] if p['name'] in held and held[p['name']] is not None}
        yield dict(family=ident,index=-1,source='golden',data=g['data'],held=held)

def timeout(*_): raise TimeoutError('independent reference wall-clock guard')

def probe_task(task):
    c, exe, guard = task
    signal.signal(signal.SIGALRM, timeout)
    ident=c['family'];meta,g=read(DISTS/ident);names=[p['name'] for p in meta['params']]
    holds=[float(c['held'].get(n,0)) for n in names];mask=sum(1<<i for i,n in enumerate(names) if n in c['held'])
    line=' '.join(map(str,[ident,mask,len(names),len(c['data']),*holds,*c['data']]))+'\n'
    r=dict(c)
    try:
        result=subprocess.run([str(exe)],input=line,text=True,capture_output=True,check=True,timeout=guard)
        cells=result.stdout.strip().split('\t')
        if cells[0]!='ok':r.update(outcome='stop',reason='\t'.join(cells[1:]))
        else:
            vals=list(map(float,cells[1:]));reported=vals[0];pars=vals[1:1+len(names)];densities=vals[1+len(names):]
            assert len(densities)==len(c['data'])
            density=math.fsum(densities)
            outside=support_violations(meta,pars,c['data'])
            r.update(outcome='pass',parameters=pars,reported=reported,dpqr=density,error=error(reported,density),outside_support=outside)
            if r['error']>1e-10 or outside:
                r['outcome']='mismatch';signal.alarm(120)
                try:
                    exp=max([abs(math.log10(abs(v))) for v in pars if v and math.isfinite(v)]+[0])
                    mp.mp.dps=min(1600,max(100,int(4*exp)+100))
                    ref=reference(DISTS/ident,meta,dict(zip(names,pars)),c['data'])
                    mp.mp.dps+=50
                    ref2=reference(DISTS/ident,meta,dict(zip(names,pars)),c['data'])
                    assert error(ref,ref2)<1e-20,(ref,ref2)
                    r.update(reference=ref,fitter_error=error(reported,ref),dpqr_error=error(density,ref))
                    r['attribution']='support' if ref==-math.inf else 'both' if r['fitter_error']>1e-10 and r['dpqr_error']>1e-10 else 'fitter' if r['fitter_error']>1e-10 else 'dpqr' if r['dpqr_error']>1e-10 else 'summation margin'
                except Exception as e:r.update(attribution='open reference',reason=f'{type(e).__name__}: {e}')
                finally:signal.alarm(0)
    except subprocess.TimeoutExpired:r.update(outcome='open fit',reason=f'no terminal result within {guard}s')
    except Exception as e:r.update(outcome='open fit',reason=f'{type(e).__name__}: {e}')
    return r


def probe_cacheable(row):
    return row['outcome'] != 'open fit' and row.get('attribution') != 'open reference'


def reference_fingerprint():
    sources = sorted((HERE/'quality').rglob('*.py'))
    sources += [HERE/name for name in ('seam-check.py', 'seam_reference.py', 'strict_common.py')]
    return file_fingerprint(sources)


def probe_groups(families):
    # A fixed partition lets a subset or a guard retry reuse the same binaries.
    groups = collections.defaultdict(list)
    for family in sorted(families):
        bucket = hashlib.sha256(family.encode()).digest()[0] % 8
        groups[bucket].append(family)
    return groups.values()


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--family', help='comma-separated family IDs (default: all)')
    ap.add_argument('--output', type=Path, required=True, help='JSONL; .summary.json is also written')
    ap.add_argument('--timeout', type=float, default=30)
    ap.add_argument('--no-long-double', action='store_true')
    ap.add_argument('--case', type=int, action='append', help='restrict to archived case index; repeatable')
    ap.add_argument('--jobs', type=positive_jobs, default=1, help='parallel probe workers (default: 1)')
    ap.add_argument('--no-cache', action='store_true', help='refit the selected probes without reading or writing the cache')
    ap.add_argument('--cache', type=Path, default=CACHE, help='result cache path (default: build/strict/seam.sqlite3)')
    args = ap.parse_args()
    ads = adapters()
    families = sorted(args.family.split(',') if args.family else ads)
    if unknown := set(families) - ads.keys():
        ap.error('unknown families: ' + ', '.join(sorted(unknown)))
    archive_bytes = ARCHIVE.read_bytes()
    selected = [c for c in cases(families, archive_bytes)
                if args.case is None or (c['source'] == 'archive' and c['index'] in args.case)]

    boost = boost_include()
    compiler = subprocess.check_output(['c++', '-v'], stderr=subprocess.STDOUT, text=True)
    compiler_path = Path(shutil.which('c++')).resolve()
    include_environment = {name: os.environ.get(name) for name in (
        'CPATH', 'CPLUS_INCLUDE_PATH', 'COMPILER_PATH', 'GCC_EXEC_PREFIX', 'LIBRARY_PATH')}
    flags = compiler_flags(args.no_long_double, boost)
    toolchain = fingerprint(compiler, str(compiler_path),
                            hashlib.sha256(compiler_path.read_bytes()).hexdigest(),
                            flags, include_environment)
    source_id = reference_fingerprint()
    cache_id = fingerprint(source_id, args.timeout, sys.version, mp.__version__,
                           sp.__version__, yaml.__version__)
    active = {c['family'] for c in selected}
    groups = [group for group in probe_groups(ads) if active.intersection(group)]
    WORK.mkdir(parents=True, exist_ok=True)

    def describe(group):
        text = probe_source(group, ads)
        source, _ = probe_paths(group, False, fingerprint(text))
        if not source.is_file():
            write_source(source, text)
        # Hash the actual translation unit, including transitive, conditional
        # and system includes. A header outside this unit cannot invalidate it.
        expanded = subprocess.check_output(['c++', *flags, '-E', str(source)])
        probe_id = fingerprint(toolchain, hashlib.sha256(expanded).hexdigest())
        _, exe = probe_paths(group, args.no_long_double, probe_id)
        return group, probe_id, (source, exe, flags)

    with ThreadPoolExecutor(args.jobs) as pool:
        probes = list(pool.map(describe, groups))
    binaries = {}
    keys = {}
    for group, probe_id, build_args in probes:
        for ident in sorted(active.intersection(group)):
            folder = DISTS/ident
            sources = [folder/'README.md', folder/'golden.json', *folder.glob('*.py')]
            keys[ident] = fingerprint(cache_id, probe_id, file_fingerprint(sources))
            binaries[ident] = build_args[1]

    def prepare():
        missing = [build_args for _, _, build_args in probes if not build_args[1].is_file()]
        with ThreadPoolExecutor(args.jobs) as pool:
            list(pool.map(lambda build_args: compile_probe(*build_args), missing))
        print('replaying frozen inputs', flush=True)

    items = [(fingerprint(keys[c['family']], c), (c, binaries[c['family']], args.timeout))
             for c in selected]
    results = cached_map(probe_task, items, args.jobs, None if args.no_cache else args.cache,
                         cacheable=probe_cacheable, prepare=prepare)
    outcomes = collections.defaultdict(collections.Counter)
    counts = collections.Counter()
    attributions = collections.Counter()
    record_count = 0
    cache_hits = 0
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(dir=args.output.parent) as staging:
        staged_output = Path(staging)/args.output.name
        with staged_output.open('w') as dest:
            for record_count, (hit, row) in enumerate(results, 1):
                cache_hits += hit
                ident = row['family']
                outcome = row['outcome']
                outcomes[ident][outcome] += 1
                counts[outcome] += 1
                if outcome == 'mismatch':
                    attributions[row.get('attribution')] += 1
                dest.write(json.dumps(row)+'\n')
                if record_count % 100 == 0:
                    print(record_count, ident, counts, flush=True)
        staged_output.replace(args.output)
    provenance = dict(source_sha256=source_id, probe_sha256=fingerprint([probe[1] for probe in probes]),
                      cache_sha256=fingerprint(keys), families=families,
                      no_long_double=args.no_long_double, timeout=args.timeout, case=args.case)
    args.output.with_suffix('.provenance.json').write_text(json.dumps(provenance, indent=2)+'\n')
    summary = dict(records=record_count, by_family={i: dict(outcomes[i]) for i in families},
                   attributions=dict(attributions),
                   head=subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
                   compiler=subprocess.check_output(['c++', '--version'], text=True).splitlines()[0])
    args.output.with_suffix('.summary.json').write_text(json.dumps(summary, indent=2)+'\n')
    print(summary, flush=True)
    print('seam-check: %d/%d probes cached' % (cache_hits, record_count), flush=True)


if __name__=='__main__': main()
