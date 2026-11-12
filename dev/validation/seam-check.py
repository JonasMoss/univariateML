#!/usr/bin/env python3
"""Refit frozen audit datasets and goldens, checking the public log-density seam.

Run through /tmp/umvl-cpu/run. --family accepts comma-separated family IDs.
Results are resumable, but use a NEW output after changing production sources.
The timeout is an audit guard, never a fitter status or a claim of nonexistence.
"""
import argparse, collections, gzip, hashlib, json, math, os, re, signal
import subprocess, sys, time
from functools import lru_cache
from fractions import Fraction
from pathlib import Path
import mpmath as mp
from quality.truth import DISTS, read
read = lru_cache(maxsize=None)(read)
from seam_reference import reference
ROOT = Path(__file__).resolve().parents[2]
WORK = ROOT/'build/scratch/seam-check'

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

def build(families, double=False, source_id=""):
    WORK.mkdir(parents=True,exist_ok=True)
    ads=adapters(); blocks=[]
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
    tag=hashlib.sha256(','.join(families).encode()).hexdigest()[:12]+'-'+source_id[:12]
    source=WORK/('probe-'+tag+'.cpp');source.write_text(src)
    boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
    exe=WORK/('probe-'+tag+('-double' if double else ''))
    subprocess.run(['ccache','c++','-std=c++17','-O1',*(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []),'-I'+str(ROOT),'-I'+str(ROOT/'univariateML-cpp/include'),'-I'+boost,str(source),'-o',str(exe)],check=True)
    return exe

def cases(families):
    path=ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
    for c in json.loads(gzip.decompress(path.read_bytes())):
        if c['family'] in families: yield dict(c,source='archive')
    for ident in families:
        meta,g=read(DISTS/ident)
        held=g.get('fit_args',{})
        # fit_args holds required/explicit coefficients; omit control arguments.
        held={p['name']:held[p['name']] for p in meta['params'] if p['name'] in held and held[p['name']] is not None}
        yield dict(family=ident,index=-1,source='golden',data=g['data'],held=held)

def timeout(*_): raise TimeoutError('independent reference wall-clock guard')

def main():
    ap=argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--family',help='comma-separated family IDs (default all 99)')
    ap.add_argument('--output',type=Path,required=True,help='resumable JSONL; .summary.json is also written')
    ap.add_argument('--timeout',type=float,default=30)
    ap.add_argument('--no-long-double',action='store_true')
    ap.add_argument('--case',type=int,action='append',help='restrict to archived case index; repeatable')
    args=ap.parse_args(); families=sorted(args.family.split(',') if args.family else adapters())
    args.output.parent.mkdir(parents=True,exist_ok=True)
    sources=sorted((ROOT/'univariateML-cpp/include').rglob('*.hpp'))+sorted((ROOT/'bindings/common').rglob('*.hpp'))
    sources += sorted(DISTS.glob('*/README.md'))+sorted(DISTS.glob('*/truth.py'))+sorted(DISTS.glob('*/golden.json'))
    sources += sorted((ROOT/'dev/validation/quality').rglob('*.py'))
    sources += [Path(__file__),Path(__file__).with_name('seam_reference.py')]
    fingerprint=hashlib.sha256(b''.join(str(p.relative_to(ROOT)).encode()+p.read_bytes() for p in sources)).hexdigest()
    provenance=args.output.with_suffix('.provenance.json')
    if provenance.exists():
        saved=json.loads(provenance.read_text())
        if saved['source_sha256']!=fingerprint or saved['families']!=families or saved['no_long_double']!=args.no_long_double or saved['case']!=args.case or saved['timeout']!=args.timeout:
            raise SystemExit('source/selection changed: use a new --output, do not reuse stale fits')
    elif args.output.exists():
        raise SystemExit('existing output lacks source provenance: use a new --output')
    else:
        provenance.write_text(json.dumps(dict(source_sha256=fingerprint,families=families,
            no_long_double=args.no_long_double,timeout=args.timeout,case=args.case),indent=2)+'\n')
    print('building',len(families),'families',flush=True)
    exe=build(families,args.no_long_double,fingerprint)
    print('replaying frozen inputs',flush=True)
    previous=[json.loads(l) for l in args.output.read_text().splitlines()] if args.output.exists() else []
    done={(r['family'],r['source'],r['index']) for r in previous}; records=previous
    signal.signal(signal.SIGALRM,timeout)
    with args.output.open('a') as dest:
        for c in cases(families):
            if args.case is not None and (c['source']!='archive' or c['index'] not in args.case): continue
            key=c['family'],c['source'],c['index']
            if key in done: continue
            ident=c['family'];meta,g=read(DISTS/ident);names=[p['name'] for p in meta['params']]
            holds=[float(c['held'].get(n,0)) for n in names];mask=sum(1<<i for i,n in enumerate(names) if n in c['held'])
            line=' '.join(map(str,[ident,mask,len(names),len(c['data']),*holds,*c['data']]))+'\n'
            r=dict(c)
            try:
                result=subprocess.run([str(exe)],input=line,text=True,capture_output=True,check=True,timeout=args.timeout)
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
            except subprocess.TimeoutExpired:r.update(outcome='open fit',reason=f'no terminal result within {args.timeout}s')
            except Exception as e:r.update(outcome='open fit',reason=f'{type(e).__name__}: {e}')
            dest.write(json.dumps(r)+'\n');dest.flush();records.append(r)
            if len(records)%100==0:print(len(records),ident,collections.Counter(r['outcome'] for r in records),flush=True)
    summary=dict(records=len(records),by_family={i:dict(collections.Counter(r['outcome'] for r in records if r['family']==i)) for i in families},attributions=dict(collections.Counter(r.get('attribution') for r in records if r['outcome']=='mismatch')),head=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0])
    args.output.with_suffix('.summary.json').write_text(json.dumps(summary,indent=2)+'\n');print(summary,flush=True)
if __name__=='__main__': main()
