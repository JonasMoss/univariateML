"""Audit every active continuous truth quantile; keep inputs/tables untouched.

Run from repository root through /tmp/umvl-cpu/run:
  python3 .../audit.py build [double]
  python3 .../audit.py measure native|double
  python3 .../audit.py verify native|double|js|r
The native dispatch expressions are extracted from the existing family tests.
"""
import hashlib
import inspect
import json
import math
import platform
import re
import subprocess
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[3]
sys.path.insert(0, str(ROOT / 'dev/validation'))
from quality import truth
import mpmath as mp

WORK = ROOT / 'build/scratch/quantile-rule'
WORK.mkdir(parents=True, exist_ok=True)


def save(name, value):
    (HERE / name).write_text(json.dumps(value, indent=2, allow_nan=False) + '\n')


def tables():
    for folder in sorted(truth.DISTS.iterdir()):
        path = folder / 'truth.json'
        if path.exists() and 'type: integer' not in (folder / 'README.md').read_text():
            yield folder.name, json.loads(path.read_text())


def build(double):
    ads = {}
    for folder in sorted(truth.DISTS.iterdir()):
        path = folder / 'test.cpp'
        if not path.exists(): continue
        parts = path.read_text().split('mltest::check_dpqr_truth("' + folder.name + '",')
        if len(parts) < 2: continue
        matches = re.findall(r'\[\]\(([^\n]*?)\)\s*\{\s*return ([^;]+);\s*\}', parts[1])[:3]
        calls = []
        for index, (signature, call) in enumerate(matches):
            names = [re.search(r'(\w+)\s*$', arg).group(1) for arg in signature.split(',')]
            canonical = ['x', 'p', 'log'] if index == 0 else ['x', 'p', 'lower', 'log']
            mapping = dict(zip(names, canonical))
            calls.append(re.sub(r'\b\w+\b', lambda m: mapping.get(m[0], m[0]), call))
        if len(calls) == 3: ads[folder.name] = calls
    assert {f for f, _ in tables()} <= set(ads), 'Missing continuous dispatch'
    source = '#include <iostream>\n#include <iomanip>\n#include <vector>\n#include <string>\n#include <cmath>\n'
    source += ''.join('#include "cppunivariate/distributions/' + i + '/dpqr.hpp"\n' for i in ads)
    source += 'double eval(const std::string&id,int mode,double x,const std::vector<double>&p,bool lower,bool log){\n'
    for ident, calls in ads.items():
        source += 'if(id=="' + ident + '") {switch(mode){' + ''.join('case ' + str(i) + ':return ' + call + ';' for i, call in enumerate(calls)) + '}}\n'
    source += 'return NAN;}\nint main(){std::cout<<std::setprecision(17);std::string id,xs;int mode,lo,lg,n;while(std::cin>>id>>mode>>xs>>lo>>lg>>n){double x=std::strtod(xs.c_str(),nullptr);std::vector<double>p(n);for(double&v:p){std::cin>>xs;v=std::strtod(xs.c_str(),nullptr);}std::cout<<eval(id,mode,x,p,lo,lg)<<"\\n";}}\n'
    src = WORK / 'probe.cpp'
    src.write_text(source)
    flags = ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []
    boost = subprocess.check_output(['Rscript', '-e', 'cat(system.file("include",package="BH"))'], text=True).strip()
    subprocess.run(['c++', '-std=c++17', '-O1', *flags, '-I' + str(ROOT / 'univariateML-cpp/include'), '-I' + boost, str(src), '-o', str(WORK / ('double' if double else 'native'))], check=True)


def forward(a, r):
    b, lo, hi, lattice, floor = map(float, r[4:])
    return a == b or (math.isfinite(a) and math.isfinite(b) and abs(a-b) <= max(1e-9*abs(b), floor)) or ((a == lo or a == hi) if lattice else lo <= a <= hi)


def measure(label):
    def evaluate(jobs):
        lines = [' '.join(map(str, [f, mode, x, int(lo), int(lg), len(p), *p])) for f, mode, x, lo, lg, p in jobs]
        out = subprocess.run([str(WORK / label)], input='\n'.join(lines)+'\n', text=True, capture_output=True, check=True)
        vals = list(map(float, out.stdout.splitlines()))
        assert len(vals) == len(jobs)
        return vals
    rows = [(f, t, n, r) for f, t in tables() for n, r in enumerate(t['quantiles'])]
    actual = evaluate([(f, 2, r[3], r[1], r[2], t['parameters'][r[0]]) for f, t, n, r in rows])
    pending = []
    for (f, t, n, r), a in zip(rows, actual):
        x = float(r[3])
        if not forward(a, r) and math.isfinite(a) and math.isfinite(float(r[4])) and (math.isfinite(x) and x < 0 if r[2] else 0 < x < 1):
            pending.append(dict(family=f, index=n, row=r, parameters=t['parameters'][r[0]], actual=a))
    at = evaluate([(v['family'], 1, v['actual'], v['row'][1], v['row'][2], v['parameters']) for v in pending])
    accepted = []
    rejected = []
    for v, p in zip(pending, at):
        r = v['row']; x = float(r[3]); scale = max(1, abs(x)) if r[2] else abs(x)
        v['cdf'] = p if math.isfinite(p) else str(p)
        (accepted if math.isfinite(p) and abs(p-x) <= 1e-12*scale else rejected).append(v)
    save(label + '-decisions.json', accepted)
    save(label + '-rejected.json', rejected)
    save(label + '-counts.json', dict(total_rows=len(rows), families={f: sum(v['family']==f for v in accepted) for f, t in tables()}, forward_failures=sum(not forward(a, r) for (_, _, _, r), a in zip(rows, actual)), backward_candidates=len(pending), backward_accepted=len(accepted)))
    probe(label)
    print(label, 'rows', len(rows), 'newly accepted', len(accepted), 'still rejected', len(rejected), flush=True)


def probe(label):
    def evaluate(mode, x):
        out = subprocess.check_output([str(WORK / label)], input=f'frechet {mode} {x} 1 1 3 1000000000 -999999998 1000000000\n', text=True)
        return float(out)
    r = [0, 1, 1, -7.3890561137087625, -1.9991476033510408592152e-18,
         math.nextafter(-1.9991476033510408592152e-18, -math.inf),
         math.nextafter(-1.9991476033510408592152e-18, math.inf), 0, 0]
    p = [1e9, -999999998., 1e9]
    a = evaluate(2, r[3])
    at = evaluate(1, a)
    save(label + '-probe-decisions.json', [dict(family='frechet', index='regression probe (not a truth row)',
         row=r, parameters=p, actual=a, cdf=at)])


def verify(label):
    rows = json.loads((HERE / (label + '-decisions.json')).read_text())
    results = []
    for v in rows:
        f = v['family']; folder = truth.DISTS / f
        meta, _ = truth.read(folder)
        route = truth.module_at(folder / 'truth.py')
        theta = dict(zip([p['name'] for p in meta['params']], v['parameters']))
        probes = []
        for digits in (80, 160):
            mp.mp.dps = digits
            t = {k: mp.mpf(float(x)) for k, x in theta.items()}
            logd = truth.declarative(folder, meta, theta, route)
            ref = route.build(t, logd) if len(inspect.signature(route.build).parameters) == 2 else route.build(t)
            r = v['row']; x = mp.mpf(float(r[3])); a = mp.mpf(v['actual'])
            tail = ref.tails(a)[0 if r[1] else 1]
            at = tail if r[2] else mp.exp(tail)
            scale = max(1, abs(x)) if r[2] else abs(x)
            oracle_error = abs(at-x)/scale
            cdf_error = abs(mp.mpf(float(v['cdf']))-at)/scale
            # Independent probability at the stored nearest-double root too.
            b = mp.mpf(float(r[4])); bt = ref.tails(b)[0 if r[1] else 1]
            bp = bt if r[2] else mp.exp(bt)
            probes.append((at, oracle_error, cdf_error, abs(bp-x)/scale))
            ld = ref.log_density if ref.log_density is not None else logd
            slope = mp.exp(ld(b) - (bt if r[2] else 0))
            condition = scale / (abs(b)*slope) if b and slope else mp.inf
        p0, p1 = probes
        result = dict(v, forward_relative_error=abs(v['actual']-float(r[4]))/abs(float(r[4])) if float(r[4]) else None,
                      oracle_probability_error=float(p1[1]), cdf_error=float(p1[2]), stored_root_probability_error=float(p1[3]), precision_delta=float(abs(p0[0]-p1[0])/scale),
                      relative_condition_number=float(condition) if mp.isfinite(condition) and condition < 1e308 else str(condition),
                      ill_conditioning_amplification=float(abs(a-b)/max(abs(a),abs(b))/p1[1]) if p1[1] else None)
        if f == 'frechet' and isinstance(v['index'], str):
            root = t['mu'] + t['sigma'] * mp.exp(-mp.log(-x)/t['lambda'])
            result['independent_quantile'] = mp.nstr(root, 60)
            result['stored_root_error_ulps'] = float(abs(root-b)/mp.mpf(math.ulp(float(b))))
            result['center'] = float(t['mu']+t['sigma'])
            result['absolute_quantile_error'] = float(abs(root-a))
            result['center_error_bound'] = 4*sys.float_info.epsilon*max(1,abs(result['center']))
            assert result['stored_root_error_ulps'] <= .5
            assert result['absolute_quantile_error'] <= result['center_error_bound']
            result['forward_accepted'] = forward(v['actual'], r)
            result['backward_accepted'] = abs(mp.mpf(float(v['cdf']))-x)/scale <= mp.mpf('1e-12')
            assert result['backward_accepted']
        results.append(result)
        print(label, f, v['index'], 'oracle', mp.nstr(p1[1], 5), 'cdf', mp.nstr(p1[2], 5), flush=True)
    save(label + '-verified.json', results)
    assert all(v['oracle_probability_error'] <= 1e-12 and v['cdf_error'] < 1e-13 and v['precision_delta'] < 1e-30 for v in results)


def provenance():
    protected = sorted(p for p in truth.DISTS.rglob('*') if p.name in ('dpqr.hpp', 'truth.json', 'truth.py', 'golden.json'))
    hashes = {str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in protected}
    changed = subprocess.check_output(['git', 'diff', '--name-only', 'HEAD'], cwd=ROOT, text=True).splitlines()
    assert not set(changed) & set(hashes), 'Settled source or fixture changed'
    sources = [ROOT / p for p in changed] + [p for p in HERE.iterdir() if p.suffix in ('.py', '.R', '.mjs', '.sh')]
    binaries = [WORK / 'native', WORK / 'double', ROOT / 'build/js/univariateML.wasm', ROOT / 'univariateML-r/src/univariateML.so']
    save('provenance.json', dict(baseline=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
        python=sys.version, mpmath=mp.__version__, platform=platform.platform(),
        compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
        R=subprocess.check_output(['Rscript','-e','cat(R.version.string)'],text=True),
        node=subprocess.check_output(['node','--version'],text=True).strip(),
        source_hashes={str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in sources},
        binary_hashes={str(p.relative_to(ROOT)): hashlib.sha256(p.read_bytes()).hexdigest() for p in binaries if p.exists()},
        changed_tracked_files=changed, protected_files=hashes,
        continuous_families=len(list(tables())), continuous_quantile_rows=sum(len(t['quantiles']) for _, t in tables())))


if __name__ == '__main__':
    if sys.argv[1] == 'build': build(len(sys.argv) > 2 and sys.argv[2] == 'double')
    elif sys.argv[1] == 'measure': measure(sys.argv[2])
    elif sys.argv[1] == 'verify': verify(sys.argv[2])
    elif sys.argv[1] == 'probe': probe(sys.argv[2])
    elif sys.argv[1] == 'provenance': provenance()
