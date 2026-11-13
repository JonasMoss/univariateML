#!/usr/bin/env python3
"""Record what every fitter returns on a fixed set of hard inputs.

A snapshot is a behaviour record, not a test: it judges nothing. Two snapshots
are compared with --diff. It is the safety net for refactors that must not
change any fit (dev/design/consolidation.md, steps 3 and 4).

Inputs, all deterministic:
  archive   the frozen fit-audit cases (small samples, every hold subset)
  golden    each family's golden sample with its frozen fit arguments
  class     magnitude and tie classes derived from the golden sample, default
            fit only: subnormal, tiny, huge, near-constant, wide spread,
            large offset, ties, half ties, one, two and constant samples, and
            invalid ones (empty, negative, zeros, NaN, infinite)
  resample  seeded draws with replacement from the golden sample (n = 3, 7)

Every case goes through the binding-neutral adapter umvl_binding::fit_<id> with
its default controls, so a record is what R and JS would see: estimates, log
likelihood, flags, or the stop with its kind, model, bounds and message.
Doubles are stored as C hexadecimal floats; any NaN is "nan".

  fit-snapshot.py --output build/snapshots/base [--family a,b] [--jobs N]
  fit-snapshot.py --diff build/snapshots/base build/snapshots/new [--ignore message]

Run builds and fits through /tmp/umvl-cpu/run (or run4 with --jobs 4). A case
that uses more than --cpu-limit CPU seconds is recorded as kind "timeout";
that is an audit guard, not a fitter status. Finished families are kept in
<output>/<family>.jsonl and skipped on a rerun while the sources are unchanged.
"""
import argparse, gzip, hashlib, json, math, os, random, re, select, shutil
import subprocess, sys, threading, time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import yaml

ROOT = Path(__file__).resolve().parents[2]
DISTS = ROOT/'univariateML-cpp/include/cppunivariate/distributions'
COMMON = ROOT/'bindings/common'
ARCHIVE = ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz'
ROUTES = json.loads((ROOT/'dev/research/evaluations/2026-11-13-retire-16-data/route-inputs.json').read_text())
ROUTE_MODE = False

FLAGS = ['-std=gnu++17', '-O2', '-DNDEBUG']   # as shipped (R, WASM, cpp-test)
TICK = os.sysconf('SC_CLK_TCK')
DENORM = 5e-324

# ---------------------------------------------------------------- inputs

def families():
    return sorted(f.name for f in DISTS.iterdir()
                  if (f/'golden.json').exists() and (COMMON/'distributions'/f.name/'fit.hpp').exists())

def read(ident):
    if ROUTE_MODE and ident in ROUTES: return ROUTES[ident]['meta'], ROUTES[ident]['golden']
    meta = yaml.safe_load((DISTS/ident/'README.md').read_text().split('---', 2)[1])
    return meta, json.loads((DISTS/ident/'golden.json').read_text())

def golden_held(meta, golden):
    args = golden.get('fit_args', {})
    return {p['name']: float(args[p['name']]) for p in meta['params']
            if args.get(p['name']) is not None}

def domain(meta, data):
    s = meta['support']
    if s['type'] == 'integer': return 'integer'
    if str(s['lower']) in ('0', '0.0') and str(s['upper']) in ('1', '1.0'): return 'unit'
    return 'positive' if min(data) > 0 and str(s['lower']) != '-inf' else 'real'

def classes(meta, golden):
    """Deterministic hard samples from fixed order statistics of the golden."""
    data = sorted(golden['data']); n = len(data)
    q10, q25, q50, q60, q75, q90 = (data[int(f*(n-1))] for f in (.1, .25, .5, .6, .75, .9))
    base = [q10, q25, q50, q60, q75, q90]
    kind = domain(meta, data)
    out = {'base6': base, 'one': [q50], 'two': [q25, q75], 'constant': [q50]*4,
           'ties': [q50]*4+[q25, q75], 'half-ties': [q50]*3+[q10, q25, q75],
           'empty': [], 'negative': [-abs(v)-1 for v in base]}
    if kind == 'integer':
        c = int(q50); lo = int(data[0])
        out.update({'near-constant': [c, c, c+1], 'zeros': [0]*4,
            'large': [int(v)+10**6 for v in base],
            'huge': [2**31-1-i for i in (0, 1, 3, 8, 11)],
            'wide': [lo, lo+1, lo+1000, lo+10**6]})
        return out
    c = q50 if q50 != 0 else 1.0
    out['near-constant'] = [c, c, math.nextafter(c, math.inf)]
    out.update({'zeros': [0.0]*4, 'nan': [math.nan, q50, q75], 'inf': [math.inf, q50, q75],
                'neginf': [-math.inf, q50, q75]})
    out['subnormal'] = [DENORM*k for k in (1, 2, 3)]
    if kind == 'unit':
        out.update({'tiny': [v*1e-300 for v in base],
            'near-one': [1-2.0**-53*k for k in (1, 2, 3, 5, 8, 13)],
            'wide': [1e-300, .5, 1-2.0**-53]})
        return out
    big = max(abs(v) for v in base) or 1.0
    spread = (q75-q25) or abs(c)
    out.update({'tiny': [v*(1e-300/big) for v in base],
        'huge': [v*(1.7e308/big) for v in base],
        'offset': [1e15*spread+v for v in base]})
    if kind == 'positive':
        out['wide'] = [1e-8*c, c, 1e8*c]
    else:
        m = abs(c)
        out.update({'wide': [-1e8*m, 1e-8*m, m, 1e8*m],
            'subnormal-signed': [-DENORM, DENORM, 2*DENORM]})
    return out

def cases(ident, archive):
    meta, golden = read(ident)
    names = [p['name'] for p in meta['params']]
    held = golden_held(meta, golden)
    def case(source, label, data, h):
        mask = sum(1 << i for i, name in enumerate(names) if name in h)
        return dict(family=ident, source=source, case=label, mask=mask,
                    held={k: float(h[k]) for k in names if k in h}, data=data)
    for c in archive.get(ident, []):
        yield case('archive', '%06d' % c['index'], c['data'], c['held'])
    yield case('golden', 'golden', golden['data'], held)
    for label, data in classes(meta, golden).items():
        yield case('class', label, data, held)
    integer = meta['support']['type'] == 'integer'
    for size in (3, 7):
        for seed in range(4):
            rng = random.Random(1000*size+seed)   # only .random(): stable across versions
            draw = [golden['data'][int(rng.random()*len(golden['data']))] for _ in range(size)]
            yield case('resample', 'n%d-s%d' % (size, seed), [int(v) if integer else v for v in draw], held)

def line(case, names):
    holds = [case['held'].get(name, 0.0) for name in names]
    return ' '.join([str(case['mask']), str(len(case['data'])),
                     *(float(v).hex() for v in holds), *(float(v).hex() for v in case['data'])])+'\n'

# ---------------------------------------------------------------- probe

PROBE = r'''#include "bindings/common/distributions/@ID@/fit.hpp"
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
static std::string num(double v){if(v!=v)return "\"nan\"";if(v==INFINITY)return "\"inf\"";if(v==-INFINITY)return "\"-inf\"";
  char b[64];std::snprintf(b,sizeof b,"\"%a\"",v);return b;}
static std::string str(const std::string&s){std::string o="\"";for(unsigned char c:s){
  if(c=='"'||c=='\\'){o+='\\';o+=char(c);}else if(c<0x20){char b[8];std::snprintf(b,sizeof b,"\\u%04x",c);o+=b;}else o+=char(c);}return o+"\"";}
static std::string named(const std::vector<std::pair<std::string,double>>&v){std::string o="{";
  for(std::size_t i=0;i<v.size();++i)o+=(i?",":"")+str(v[i].first)+":"+num(v[i].second);return o+"}";}
int main(){
  std::string token;unsigned mask;std::size_t n;
  while(std::cin>>mask>>n){
    std::vector<double> holds(@K@);std::vector<@TYPE@> data;
    for(double&v:holds){std::cin>>token;v=std::strtod(token.c_str(),nullptr);}
    for(std::size_t i=0;i<n;++i){std::cin>>token;data.push_back(static_cast<@TYPE@>(std::strtod(token.c_str(),nullptr)));}
    const auto r=umvl_binding::fit_@ID@(cppunivariate::@SPAN@{data.data(),data.size()},@ARGS@);
    std::string o="{\"kind\":"+str(r.error.empty()?"ok":r.error_kind.empty()?"error":r.error_kind);
    o+=",\"estimates\":"+named(r.estimates)+",\"logLik\":"+num(r.logLik);
    o+=",\"iterations\":"+std::string(r.has_iterations?std::to_string(r.iterations):"null");
    o+=",\"fixed\":[";for(std::size_t i=0;i<r.fixed.size();++i)o+=(i?",":"")+str(r.fixed[i]);
    o+="],\"restricted\":{";for(std::size_t i=0;i<r.restricted.size();++i)o+=(i?",":"")+str(r.restricted[i].first)+":"+str(r.restricted[i].second);
    o+="},\"extra\":"+named(r.extra)+",\"extra_vectors\":{";
    for(std::size_t i=0;i<r.extra_vectors.size();++i){o+=(i?",":"")+str(r.extra_vectors[i].first)+":[";
      for(std::size_t j=0;j<r.extra_vectors[i].second.size();++j)o+=(j?",":"")+num(r.extra_vectors[i].second[j]);o+="]";}
    o+="},\"warnings\":[";for(std::size_t i=0;i<r.warnings.size();++i)o+=(i?",":"")+str(r.warnings[i]);
    o+="],\"message\":"+str(r.error)+",\"model\":"+str(r.model)+",\"attainable_tol\":"+num(r.attainable_tol);
    o+=",\"limit_estimates\":"+named(r.limit_estimates)+",\"limit_loglik\":"+num(r.limit_loglik);
    o+=",\"loglik_lower\":"+num(r.loglik_lower)+",\"loglik_upper\":"+num(r.loglik_upper);
    o+=",\"segment_loglik_upper\":"+num(r.segment_loglik_upper)+",\"candidates\":[";
    for(std::size_t i=0;i<r.candidates.size();++i){const auto&c=r.candidates[i];
      o+=(i?",":"")+std::string("{\"model\":")+str(c.model)+",\"estimates\":"+named(c.estimates)+
         ",\"loglik_lower\":"+num(c.loglik_lower)+",\"loglik_upper\":"+num(c.loglik_upper)+"}";}
    o+="]";
#if __has_include("cppunivariate/core/outcome.hpp")
    o+=",\"outcome\":";
    if(r.outcome_set)o+=std::string("{\"kind\":")+str(cppunivariate::outcome_kind_name(r.outcome.kind))+",\"reason\":"+
      str(cppunivariate::outcome_reason_name(r.outcome.reason))+",\"certified\":"+(r.outcome.certified?"true":"false")+
      ",\"unique\":"+(r.outcome.unique?"true":"false")+"}";
    else o+="null";
    o+=",\"core_status\":"+std::to_string(r.core_status)+",\"outcome_override\":"+(r.outcome_override?"true":"false");
#endif
    std::cout<<o<<"}"<<std::endl;
  }
}
'''

def boost_include():
    if os.environ.get('UMVL_BOOST_INCLUDE'): return os.environ['UMVL_BOOST_INCLUDE']
    return subprocess.check_output(['Rscript', '-e', 'cat(system.file("include",package="BH"))'], text=True).strip()

def build(ident, work, double, boost):
    meta, _ = read(ident)
    header = (COMMON/'distributions'/ident/'fit.hpp').read_text() if not ROUTE_MODE else ''
    integer = bool(re.search(r'fit_'+ident+r'\(cppunivariate::ispan', header))
    if ROUTE_MODE: integer = False
    k = len(meta['params'])
    args = ','.join('umvl_binding::Held{bool(mask&%du),holds[%d]}' % (1 << i, i) for i in range(k))
    text = (PROBE.replace('@ID@', ident).replace('@K@', str(k)).replace('@ARGS@', args)
            .replace('@TYPE@', 'int' if integer else 'double').replace('@SPAN@', 'ispan' if integer else 'span'))
    if ROUTE_MODE:
        route = ROUTES[ident]
        names = [p['name'] for p in meta['params']]
        h = lambda name: 'umvl_binding::Held{bool(mask&%du),holds[%d]}' % (1 << names.index(name), names.index(name))
        entries = []
        for name in names:
            if name in ('a','b','N'): continue
            entries.append('{"%s",%s}' % ('theta' if ident=='zmlogarithmic' and name=='prob' else name,h(name)))
        endpoint = lambda name: '(mask&%du?holds[%d]:NAN)' % (1 << names.index(name),names.index(name))
        a,b = 'NAN','NAN'
        if ident in ('tpois','tbinom'):
            a='(mask&%du?std::max(0.,std::floor(holds[%d])+1):NAN)' % (1 << names.index('a'),names.index('a'))
            b='(mask&%du?std::floor(holds[%d]):NAN)' % (1 << names.index('b'),names.index('b'))
        elif ident=='tnorm': a,b=endpoint('a'),endpoint('b')
        elif ident=='zipf': a,b='1',endpoint('N')
        call='umvl_binding::fit_modifier("%s",cppunivariate::span{data.data(),data.size()},{%s},"%s",%s,%s,%s)' % (route['base'],','.join(entries),route['zero'],'true' if route['zero']=='none' else 'false',a,b)
        text=text.replace('#include "bindings/common/distributions/'+ident+'/fit.hpp"','#include "bindings/common/modifiers.hpp"')
        text=re.sub(r'umvl_binding::fit_'+ident+r'\(.*?;',''+call+';',text,count=1)
    source = work/(ident+'.cpp'); exe = work/ident
    source.write_text(text)
    env = dict(os.environ)
    env.setdefault('CCACHE_DIR', str(Path.home()/'.cache/ccache/univariateML'))
    env.setdefault('CCACHE_BASEDIR', str(ROOT)); env.setdefault('CCACHE_NOHASHDIR', '1')
    compiler = (['ccache'] if shutil.which('ccache') else [])+[os.environ.get('CXX', 'c++')]
    subprocess.run([*compiler, *FLAGS, *(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []),
        '-I'+str(ROOT), '-I'+str(ROOT/'univariateML-cpp/include'), '-I'+boost,
        str(source), '-o', str(exe)], check=True, env=env, cwd=ROOT)
    return exe

def cpu_seconds(pid):
    fields = Path('/proc/%d/stat' % pid).read_text().rsplit(')', 1)[1].split()
    return (int(fields[11])+int(fields[12]))/TICK   # utime + stime

class Probe:
    def __init__(self, exe): self.exe = exe; self.start()
    def start(self):
        self.child = subprocess.Popen([str(self.exe)], stdin=subprocess.PIPE, stdout=subprocess.PIPE)
        self.buffer = b''
    def close(self):
        try: self.child.stdin.close()
        except OSError: pass
        self.child.kill(); self.child.wait()
    def ask(self, text, limit):
        """One case in, one JSON line out; None after a CPU-time overrun or a crash."""
        try:
            before = cpu_seconds(self.child.pid)
            self.child.stdin.write(text.encode()); self.child.stdin.flush()
            fd = self.child.stdout.fileno()
            while b'\n' not in self.buffer:
                if select.select([fd], [], [], .25)[0]:
                    chunk = os.read(fd, 65536)
                    if not chunk: raise BrokenPipeError
                    self.buffer += chunk
                elif cpu_seconds(self.child.pid)-before > limit:
                    self.close(); self.start(); return 'timeout'
            answer, self.buffer = self.buffer.split(b'\n', 1)
            return json.loads(answer)
        except (BrokenPipeError, FileNotFoundError, ProcessLookupError, ValueError):
            code = self.child.poll()
            self.close(); self.start(); return 'crash:%s' % code

def run_family(ident, args, archive, work, boost, tag):
    part = args.output/(ident+'.jsonl')
    if part.exists() and part.read_text().startswith(tag): return ident, 'kept'
    meta, _ = read(ident)
    names = [p['name'] for p in meta['params']]
    probe = Probe(build(ident, work, args.no_long_double, boost))
    rows = []
    for case in cases(ident, archive):
        text = line(case, names)
        result = probe.ask(text, args.cpu_limit)
        if isinstance(result, str):
            result = dict(kind=result.split(':')[0], exit=result.partition(':')[2] or None)
        record = dict(family=ROUTES[ident]['id'] if ROUTE_MODE else ident, source=case['source'], case=case['case'], mask=case['mask'],
                      held=case['held'], n=len(case['data']),
                      input_sha256=hashlib.sha256(text.encode()).hexdigest()[:16])
        if case['source'] in ('class', 'resample'): record['data'] = case['data']
        record['result'] = result
        rows.append(json.dumps(record, sort_keys=True))
    probe.close()
    part.write_text(tag+'\n'+'\n'.join(rows)+'\n')
    return ident, len(rows)

# ---------------------------------------------------------------- commands

def fingerprint(args):
    sources = sorted((ROOT/'univariateML-cpp/include').rglob('*.hpp'))+sorted(COMMON.rglob('*.hpp'))
    sources += sorted(DISTS.glob('*/README.md'))+sorted(DISTS.glob('*/golden.json'))+[ARCHIVE, Path(__file__)]
    digest = hashlib.sha256()
    for path in sources:
        digest.update(str(path.relative_to(ROOT)).encode()); digest.update(path.read_bytes())
    git = lambda *a: subprocess.run(['git', *a], cwd=ROOT, capture_output=True, text=True).stdout.strip()
    return dict(source_sha256=digest.hexdigest(), commit=git('rev-parse', 'HEAD'),
        dirty=bool(git('status', '--porcelain', '--', 'univariateML-cpp/include', 'bindings/common')),
        compiler=subprocess.run([os.environ.get('CXX', 'c++'), '--version'], capture_output=True, text=True).stdout.splitlines()[0],
        flags=FLAGS, no_long_double=args.no_long_double, cpu_limit=args.cpu_limit)

def snapshot(args):
    selected = sorted(args.family.split(',')) if args.family else sorted(ROUTES) if ROUTE_MODE else families()
    args.output.mkdir(parents=True, exist_ok=True)
    work = args.output/'build'; work.mkdir(exist_ok=True)
    provenance = fingerprint(args)
    tag = '# '+provenance['source_sha256'][:16]+(' double' if args.no_long_double else ' native')
    archive = {}
    for c in json.loads(gzip.decompress(ARCHIVE.read_bytes())):
        archive.setdefault(c['family'], []).append(c)
    boost = boost_include()
    started = time.time(); lock = threading.Lock()
    def one(ident):
        result = run_family(ident, args, archive, work, boost, tag)
        with lock: print('%-14s %s  (%.0f s)' % (*result, time.time()-started), flush=True)
    with ThreadPoolExecutor(args.jobs) as pool: list(pool.map(one, selected))
    if not args.family:   # a full run also writes the merged, sorted snapshot
        with gzip.open(args.output/'snapshot.jsonl.gz', 'wt') as dest:
            for ident in selected:
                dest.write((args.output/(ident+'.jsonl')).read_text().split('\n', 1)[1])
    (args.output/'provenance.json').write_text(json.dumps(dict(provenance, families=selected), indent=2)+'\n')

def load(folder):
    rows = {}
    if Path(folder).is_file():  # a stored snapshot.jsonl.gz
        with gzip.open(folder, 'rt') as source:
            for text in source:
                r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
        return rows
    for part in sorted(Path(folder).glob('*.jsonl')):
        for text in part.read_text().splitlines()[1:]:
            r = json.loads(text); rows[(r['family'], r['source'], r['case'])] = r
    return rows

def flatten(value, prefix=''):
    if isinstance(value, dict):
        for k, v in value.items(): yield from flatten(v, prefix+'.'+k if prefix else k)
    else: yield prefix, value

def show(value):
    if isinstance(value, str) and re.fullmatch(r'-?0x[0-9a-f.]+p[+-]\d+', value): return '%.17g' % float.fromhex(value)
    return json.dumps(value)

def diff(args):
    a, b = load(args.diff[0]), load(args.diff[1])
    if args.family:   # compare only these families, e.g. a partial rerun against a full baseline
        keep = set(args.family.split(','))
        a = {k: v for k, v in a.items() if k[0] in keep}; b = {k: v for k, v in b.items() if k[0] in keep}
    ignore = set(args.ignore.split(',')) if args.ignore else set()
    changed = 0; by_family = {}; by_field = {}
    for key in sorted(set(a) | set(b)):
        if key not in a or key not in b:
            print('%s %s %s: only in %s' % (*key, args.diff[1] if key not in a else args.diff[0])); changed += 1
            by_family[key[0]] = by_family.get(key[0], 0)+1; continue
        if a[key]['input_sha256'] != b[key]['input_sha256']:
            print('%s %s %s: inputs differ' % key); changed += 1; continue
        x, y = dict(flatten(a[key]['result'])), dict(flatten(b[key]['result']))
        fields = [f for f in sorted(set(x) | set(y)) if x.get(f) != y.get(f) and f.split('.')[0] not in ignore]
        if not fields: continue
        changed += 1; by_family[key[0]] = by_family.get(key[0], 0)+1
        for f in fields: by_field[f.split('.')[0]] = by_field.get(f.split('.')[0], 0)+1
        print('%s %s %s:' % key, '; '.join('%s %s -> %s' % (f, show(x.get(f)), show(y.get(f))) for f in fields[:6]),
              '(+%d more)' % (len(fields)-6) if len(fields) > 6 else '')
    print('\n%d of %d cases differ' % (changed, len(set(a) | set(b))))
    if changed:
        print('by family:', dict(sorted(by_family.items(), key=lambda kv: -kv[1])))
        print('by field:', dict(sorted(by_field.items(), key=lambda kv: -kv[1])))
    sys.exit(1 if changed else 0)

def outcomes(args):
    """Which adapters report the shared outcome, and whether it agrees with the kind they emit."""
    rows = load(args.outcomes)
    expected = lambda o: {'estimate': 'ok', 'limit': 'limit', 'nonexistence': 'nomle', 'budget': 'budget'}.get(o['kind']) or (
        'error' if o['reason'] in ('invalid-input', 'invalid-argument', 'unsupported') else
        'unresolved' if o['reason'] == 'unresolved' else 'numerical')
    families = {}; table = {}; disagree = []; plain = {}; overrides = {}; statuses = {}
    for (family, source, case), r in sorted(rows.items()):
        res = r['result']; o = res.get('outcome')
        state = families.setdefault(family, [0, 0]); state[o is not None] += 1
        if res['kind'] in ('timeout', 'crash') or o is None: continue
        statuses.setdefault(family, set()).add(res['core_status'])
        key = (o['kind'], o['reason'], res['kind']); table[key] = table.get(key, 0)+1
        if res.get('outcome_override'): overrides[family] = overrides.get(family, 0)+1
        want = expected(o); got = 'numerical' if res['kind'] in ('tolerance', 'conditioning') else res['kind']
        # Until step 5 a tie stop is how an estimate that is not unique, or not certified, is rendered.
        if got == 'tie' and o['kind'] == 'estimate' and not (o['unique'] and o['certified']): continue
        flag = res['extra'].get('certified') if got == 'ok' else None
        if flag is not None and (float.fromhex(flag) != 0) != o['certified']:
            disagree.append((family, source, case, o, 'certified flag %s' % flag)); continue
        if got == want: continue
        if want == 'numerical' and got == 'error': plain[family] = plain.get(family, 0)+1
        else: disagree.append((family, source, case, o, res['kind']))
    done = sorted(f for f, (unset, has) in families.items() if has and not unset)
    partial = sorted(f for f, (unset, has) in families.items() if has and unset)
    print('%d of %d families report an outcome on every case: %s' % (len(done), len(families), ' '.join(done)))
    if partial: print('partly:', {f: families[f] for f in partial})
    print('\noutcome kind, reason, emitted kind: cases')
    for key, count in sorted(table.items()): print('  %-13s %-17s %-10s %d' % (*key, count))
    print('\nraw statuses reached:', {f: sorted(s) for f, s in sorted(statuses.items())})
    print('plain errors that become numerical stops in step 5:', plain)
    print('adapter overrides of a core success (core defects):', overrides)
    print('\n%d disagreements between outcome and emitted kind' % len(disagree))
    for d in disagree[:40]: print(' ', *d)
    sys.exit(1 if disagree else 0)

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--output', type=Path, help='snapshot directory')
    ap.add_argument('--family', help='comma-separated family ids (default: all); also restricts --diff')
    ap.add_argument('--jobs', type=int, default=1, help='families fitted in parallel (one core each)')
    ap.add_argument('--cpu-limit', type=float, default=300, help='CPU seconds per case before "timeout"')
    ap.add_argument('--no-long-double', action='store_true', help='double-only intermediates')
    ap.add_argument('--diff', nargs=2, metavar=('OLD', 'NEW'), help='compare two snapshots (directories or stored .jsonl.gz files)')
    ap.add_argument('--outcomes', metavar='DIR', help='report shared-outcome coverage and consistency of a snapshot')
    ap.add_argument('--ignore', help='comma-separated result fields to ignore in --diff, e.g. message')
    ap.add_argument('--modifier-routes', action='store_true', help='replay frozen retirement inputs through the modifier seam')
    args = ap.parse_args()
    global ROUTE_MODE
    ROUTE_MODE = args.modifier_routes
    if args.outcomes: outcomes(args)
    elif args.diff: diff(args)
    elif args.output: snapshot(args)
    else: ap.error('give --output, --diff or --outcomes')

if __name__ == '__main__':
    main()
