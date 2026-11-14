#!/usr/bin/env python3
"""Strict layer: a reported log likelihood equals the summed C++ log density.

For every snapshot case whose result is an estimate, every limit stop that
states limit_loglik with limit_estimates for a model whose density is in the
C++ library (a family of ours or a modifier law), and every candidate of a
tie stop, this recomputes sum_i d<id>(x_i; theta, log = TRUE) with the
family's dpqr.hpp at the returned coefficients, on the same input (rebuilt
with fit-snapshot.py's own cases()), compiled in the snapshot's precision
mode with the shipped flags. Terms are summed exactly (math.fsum).

Comparison, with S the summed density, R the reported value and
A = sum_i |log f(x_i)|:

  |R - S| <= TOL * max(1, |R|) + SUM_EPS * n * A

TOL = 1e-10 is the accuracy contract's logLik tolerance
(dev/design/accuracy-contract.md); the second term is a bound on the
rounding of a naive n-term sum of terms of total magnitude A. A tie
candidate passes when S lies in its stated [lower, upper] widened by the
same margin. A non-finite S where R is finite fails ("support" when S is
-Inf: the returned coefficients put an observation outside the support).

The modifier routes (fit-snapshot.py --modifier-routes) are included: each
route's law has a C++ density (dtbinom, dtnorm, dtpois, dzipf, dz*), and
window bounds that the caller did not hold are rebuilt as the route does
(lower and upper counts from the data).

Usage:
  fit-density.py NATIVE [DOUBLE] [--records FILE] [--prune | --add REF [--match RE]]
NATIVE and DOUBLE are as for fit-judge.py; the precision mode of each is read
from its provenance. Failures are keyed 'density|mode|family|source|case'
(a limit stop uses 'density:limit', a tie candidate 'density:candidate#<i>')
and compared with strict-known.json. Each failure of a dossier family is
attributed with an independent mpmath value at the exact binary64 inputs
(seam_reference.py, as in seam-check.py): reported off, density off, or both.
--records writes one JSON line per evaluated item.
"""
import argparse, collections, hashlib, json, math, os, re, shutil, subprocess, sys
from pathlib import Path
from strict_common import ROOT, HERE, add_arguments, load, mode_of, number, ratchet, relative, snapshot_module

TOL = 1e-10
SUM_EPS = 2.0**-52          # n * eps * sum|terms| bounds the error of a recursive n-term sum
CPU_LIMIT = 120             # CPU seconds per density request (an audit guard)
CHECK = 'density'
WORK = ROOT/'build/strict/density'

fs = snapshot_module()
DISTS = fs.DISTS

def adapters():
    """Density call per family, from the d<id> lambda the family's test.cpp passes to check_dpqr_truth."""
    out = {}
    for folder in sorted(DISTS.iterdir()):
        if not (folder/'golden.json').exists() or not (folder/'test.cpp').exists(): continue
        part = (folder/'test.cpp').read_text().split('mltest::check_dpqr_truth("'+folder.name+'",')
        if len(part) < 2: continue
        sig, call = re.findall(r'\[\]\(([^\n]*?)\)\s*\{\s*return ([^;]+);\s*\}', part[1])[0]
        names = [re.search(r'(\w+)\s*$', a)[1] for a in sig.split(',')]
        mapping = dict(zip(names, ['x', 'p', 'log']))
        out[folder.name] = re.sub(r'\b\w+\b', lambda m: mapping.get(m[0], m[0]), call)
    return out

def laws():
    """Every C++ log density reachable here: id -> (header, call, parameter names, integer support)."""
    out = {}
    for ident, call in adapters().items():
        meta, _ = fs.read(ident)
        out[ident] = ('cppunivariate/distributions/%s/dpqr.hpp' % ident, call,
                      [p['name'] for p in meta['params']], meta['support']['type'] == 'integer')
    for ident, route in fs.ROUTES.items():
        meta = route['meta']; names = [p['name'] for p in meta['params']]
        call = 'cppunivariate::distributions::d%s(x, %s, true)' % (ident, ', '.join('p[%d]' % i for i in range(len(names))))
        header = 'cppunivariate/distributions/%s/%s_dpqr.hpp' % (route['base'], ident)
        if ident == 'zipf':   # the zeta window admits every real exponent; dzipf is the s >= 0 law
            call = "cppunivariate::zeta_window_eval('d', x, static_cast<int>(p[0]), p[1], true, true)"
            header = 'cppunivariate/distributions/zeta/truncation.hpp'
        out[ident] = (header, call, names, meta['support']['type'] == 'integer')
    return out

def attribute(law, values, data):
    """Independent value of the summed log density at exact binary64 inputs (seam_reference.py), or a reason."""
    import signal
    import mpmath as mp
    from quality.truth import read as truth_read
    from seam_reference import reference
    if not (DISTS/law/'README.md').exists(): return None, 'no dossier (modifier law): no independent reference'
    meta, _ = truth_read(DISTS/law)
    def guard(*_): raise TimeoutError('reference guard')
    old = signal.signal(signal.SIGALRM, guard); signal.alarm(120)
    try:
        scale = max([abs(math.log10(abs(v))) for v in values.values() if v and math.isfinite(v)] +
                    [abs(math.log10(abs(v))) for v in data if v and math.isfinite(v)]+[0])
        values = {p['name']: values[p['name']] for p in meta['params']}
        with mp.workdps(min(1600, max(100, int(4*scale)+100))):
            a = reference(DISTS/law, meta, values, data)
        with mp.workdps(min(1650, max(150, int(4*scale)+150))):
            b = reference(DISTS/law, meta, values, data)
        if relative(a, b) > 1e-20: return None, 'reference unstable across precision: %r, %r' % (a, b)
        return a, None
    except Exception as e:
        return None, '%s: %s' % (type(e).__name__, str(e)[:120])
    finally:
        signal.alarm(0); signal.signal(signal.SIGALRM, old)

PROBE = r'''#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <string>
#include <vector>
@INCLUDES@
static std::string num(double v){if(v!=v)return "\"nan\"";if(v==INFINITY)return "\"inf\"";if(v==-INFINITY)return "\"-inf\"";
  char b[64];std::snprintf(b,sizeof b,"\"%a\"",v);return b;}
static double density(int law,double x,const std::vector<double>&p){
  const bool log=true;(void)log;
  switch(law){
@CASES@
  }
  return NAN;
}
int main(){
  std::string token;int law;std::size_t k,n;
  while(std::cin>>law>>k>>n){
    std::vector<double> p(k),xs(n);
    for(double&v:p){std::cin>>token;v=std::strtod(token.c_str(),nullptr);}
    for(double&v:xs){std::cin>>token;v=std::strtod(token.c_str(),nullptr);}
    std::string o="{\"terms\":[";
    for(std::size_t i=0;i<n;++i)o+=(i?",":"")+num(density(law,xs[i],p));
    std::cout<<o<<"]}"<<std::endl;
  }
}
'''

def build(table, double):
    order = sorted(table)
    includes = '\n'.join(sorted({'#include "%s"' % table[i][0] for i in order}))
    cases = '\n'.join('  case %d: return %s;' % (j, table[i][1]) for j, i in enumerate(order))
    text = PROBE.replace('@INCLUDES@', includes).replace('@CASES@', cases)
    tag = hashlib.sha256((text+str(double)).encode()).hexdigest()[:12]
    WORK.mkdir(parents=True, exist_ok=True)
    source = WORK/('probe-%s.cpp' % tag); exe = WORK/('probe-%s%s' % (tag, '-double' if double else ''))
    source.write_text(text)
    env = dict(os.environ)
    env.setdefault('CCACHE_DIR', str(Path.home()/'.cache/ccache/univariateML'))
    env.setdefault('CCACHE_BASEDIR', str(ROOT)); env.setdefault('CCACHE_NOHASHDIR', '1')
    compiler = (['ccache'] if shutil.which('ccache') else [])+[os.environ.get('CXX', 'c++')]
    subprocess.run([*compiler, *fs.FLAGS, *(['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'] if double else []),
        '-I'+str(ROOT/'univariateML-cpp/include'), '-I'+fs.boost_include(), str(source), '-o', str(exe)],
        check=True, env=env, cwd=ROOT)
    return exe, {i: j for j, i in enumerate(order)}

def route_ids():
    return {route['id']: ident for ident, route in fs.ROUTES.items()}

def inputs(family, archive):
    """(source, case) -> data, rebuilt by fit-snapshot.py and checked against the record's input hash."""
    routes = route_ids()
    ident = routes.get(family, family)
    fs.ROUTE_MODE = family in routes
    try:
        meta, _ = fs.read(ident)
        names = [p['name'] for p in meta['params']]
        return ident, {(c['source'], c['case']): (c, hashlib.sha256(fs.line(c, names).encode()).hexdigest()[:16])
                       for c in fs.cases(ident, archive)}
    finally:
        fs.ROUTE_MODE = False

def window(ident, held, data, names):
    """Window coefficients of a modifier route as the route computes them (fit-snapshot.py, modifiers.hpp)."""
    out = {}
    if ident in ('tbinom', 'tpois'):
        lower = max(0., math.floor(held['a'])+1) if 'a' in held else min(data)
        upper = math.floor(held['b']) if 'b' in held else max(data)
        out = dict(a=max(-1., math.ceil(lower)-1), b=math.floor(upper))
    elif ident == 'tnorm':
        out = dict(a=held.get('a', min(data)), b=held.get('b', max(data)))
    elif ident == 'zipf':
        out = dict(N=held.get('N', max(data)))
    return {k: v for k, v in out.items() if k in names}

def coefficients(law, table, values, held, data, own):
    """Parameter vector of a law in its declared order, or the names that are missing."""
    names = table[law][2]
    values = dict(values)
    if law == 'zmlogarithmic' and 'theta' in values: values['prob'] = values.pop('theta')
    if own:   # held window bounds and design values are inputs of the fit, not estimates
        for k, v in held.items():
            values.setdefault('prob' if (law == 'zmlogarithmic' and k == 'theta') else k, v)
        if law in fs.ROUTES and data:
            for k, v in window(law, held, data, names).items(): values.setdefault(k, v)
    missing = [n for n in names if n not in values]
    return ([values[n] for n in names] if not missing else None), missing

def items(r, family_law, table):
    """What to evaluate for one record: (label, law, values, reported, bounds, own)."""
    res = r['result']; out = []
    num = lambda d: {k: number(v) for k, v in d.items()}
    if res['kind'] == 'ok':
        out.append(('estimate', family_law, num(res['estimates']), number(res['logLik']), None, True))
    elif res['kind'] == 'limit' and res['limit_loglik'] != 'nan' and res['limit_estimates']:
        model = res['model']
        out.append(('limit', model if model in table else None, num(res['limit_estimates']),
                    number(res['limit_loglik']), None, False))
    if res['kind'] == 'tie' and (res.get('outcome') or {}).get('kind', 'estimate') == 'estimate':
        for i, c in enumerate(res['candidates']):
            model = c['model'] or family_law
            out.append(('candidate#%d' % i, model if model in table else None, num(c['estimates']), None,
                        (number(c['loglik_lower']), number(c['loglik_upper'])), model == family_law))
    return out

def evaluate(rows, mode, table, archive, records, attributions=True):
    exe, index = build(table, mode == 'double')
    probe = fs.Probe(exe)
    failures = {}; stats = collections.defaultdict(list); unchecked = collections.Counter(); judged = set()
    by_family = collections.defaultdict(list)
    for key, r in rows.items(): by_family[key[0]].append((key, r))
    try:
        for family in sorted(by_family):
            ident, rebuilt = inputs(family, archive)
            integer = table[ident][3] if ident in table else False
            for key, r in sorted(by_family[family]):
                todo = items(r, ident, table)
                if not todo: continue
                judged.add((mode,)+key)
                case, digest = rebuilt.get((r['source'], r['case']), (None, None))
                if case is None or digest != r['input_sha256']:
                    raise SystemExit('%s %s %s: snapshot input differs from fit-snapshot.py\'s cases()' % key)
                data = [float(int(v)) if integer and math.isfinite(v) else float(v) for v in case['data']]
                for label, law, values, reported, bounds, own in todo:
                    if law is None:
                        unchecked['%s: %s model has no C++ density' % (family, label)] += 1; continue
                    params, missing = coefficients(law, table, values, r['held'], data, own)
                    if params is None:
                        unchecked['%s: %s %s lacks %s' % (family, label, law, ','.join(missing))] += 1; continue
                    line = '%d %d %d %s\n' % (index[law], len(params), len(data),
                                              ' '.join(float(v).hex() for v in params+data))
                    answer = probe.ask(line, CPU_LIMIT)
                    if not isinstance(answer, dict):
                        failures['|'.join((CHECK+('' if label == 'estimate' else ':'+label), mode)+key)] = \
                            'density probe %s for %s' % (answer, law)
                        continue
                    terms = [number(t) for t in answer['terms']]
                    total = math.fsum(terms) if all(math.isfinite(t) for t in terms) else sum(terms)
                    magnitude = math.fsum(abs(t) for t in terms) if all(math.isfinite(t) for t in terms) else math.inf
                    rec = dict(mode=mode, family=family, source=key[1], case=key[2], item=label, law=law,
                               n=len(data), reported=reported, bounds=bounds, summed=total, magnitude=magnitude)
                    if bounds is None:
                        rel = relative(total, reported)
                        allowed = TOL*max(1, abs(reported))+SUM_EPS*len(data)*magnitude if math.isfinite(reported) else 0
                        ok = total == reported or (math.isfinite(total) and math.isfinite(reported) and abs(total-reported) <= allowed)
                        rec.update(relative=rel, ratio=abs(total-reported)/allowed if allowed and math.isfinite(total) else None)
                        if not ok:
                            why = ('summed density %s, reported %r' % (total, reported) if not math.isfinite(total) else
                                   'reported %.17g, summed density %.17g, relative %.3g' % (reported, total, rel))
                            if total == -math.inf: why = 'support: '+why
                            if attributions:
                                ref, reason = attribute(law, dict(zip(table[law][2], params)), data)
                                if ref is None: why += '; independent: '+reason
                                else:
                                    a, b = relative(reported, ref), relative(total, ref)
                                    side = ('both' if a > TOL and b > TOL else 'fitter' if a > TOL else
                                            'density' if b > TOL else 'neither')
                                    why += '; independent %.17g: reported off %.2g, density off %.2g (%s)' % (ref, a, b, side)
                                    rec.update(reference=ref, side=side)
                            failures['|'.join((CHECK+('' if label == 'estimate' else ':'+label), mode)+key)] = why
                            rec['detail'] = why
                        stats[family].append(rel)
                    else:
                        lo, hi = bounds
                        margin = TOL*max(1, abs(lo), abs(hi))+SUM_EPS*len(data)*magnitude if math.isfinite(magnitude) else 0
                        ok = math.isfinite(total) and lo-margin <= total <= hi+margin
                        rec.update(relative=0.0 if ok else math.inf)
                        if not ok:
                            failures['|'.join((CHECK+':'+label, mode)+key)] = \
                                '%s summed density %.17g outside stated [%.17g, %.17g]' % (law, total, lo, hi)
                    rec['pass'] = ok
                    if records is not None: records.write(json.dumps(rec)+'\n')
    finally:
        probe.close()
    return failures, judged, stats, unchecked

def summary(stats, mode):
    print('\n%s: relative difference |reported - summed| / max(1, |reported|) per family' % mode)
    print('  %-26s %6s %10s %10s %10s %10s' % ('family', 'items', 'median', 'p99', 'max', '>1e-10'))
    for family in sorted(stats):
        v = sorted(stats[family]); k = len(v)
        q = lambda f: v[min(k-1, int(f*(k-1)+.5))]
        print('  %-26s %6d %10.2g %10.2g %10.2g %10d' % (family, k, q(.5), q(.99), v[-1], sum(x > TOL for x in v)))

def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('native'); ap.add_argument('double', nargs='?')
    ap.add_argument('--records', type=Path, help='write every evaluated item as JSON lines')
    ap.add_argument('--quiet', action='store_true', help='omit the per-family distribution')
    ap.add_argument('--no-attribution', action='store_true', help='skip the independent mpmath value for failures')
    add_arguments(ap)
    args = ap.parse_args()
    import gzip
    archive = {}
    for c in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())): archive.setdefault(c['family'], []).append(c)
    table = laws()
    records = args.records.open('w') if args.records else None
    failures = {}; judged = set(); unchecked = collections.Counter()
    for paths in filter(None, (args.native, args.double)):
        mode = mode_of(paths)
        f, j, stats, u = evaluate(load(paths), mode, table, archive, records, not args.no_attribution)
        failures.update(f); judged |= j; unchecked.update({'%s %s' % (mode, k): v for k, v in u.items()})
        if not args.quiet: summary(stats, mode)
    if records: records.close()
    if unchecked:
        print('\nnot checked (no C++ density for the model, or coefficients missing):')
        for k, v in sorted(unchecked.items()): print('  %s: %d' % (k, v))
    print()
    sys.exit(ratchet(failures, judged, lambda check: check.split(':')[0] == CHECK, args, 'fit-density'))

if __name__ == '__main__': main()
