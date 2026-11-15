"""50-digit log likelihood at the returned coefficients for three subnormal cases whose logLik moved by about 1.

The 2026-11-15 values equal the exact sum; the 2026-11-14 values did not."""
import sys, mpmath as mp
from pathlib import Path
R = Path(__file__).resolve().parents[4]
sys.path.insert(0, str(R/'dev/validation'))
from strict_common import load, number
mp.mp.dps = 50
E = R/'dev/research/evaluations'
new = load(E/'2026-11-15-fit-snapshot-data/native.jsonl.gz'); old = load(E/'2026-11-14-fit-snapshot-data/native.jsonl.gz')
num = lambda v: mp.mpf(v) if isinstance(v, (int, float)) else mp.mpf(number(v))
DENSITY = {
    'laplace': lambda x, p: -mp.log(2*p['sigma'])-abs(x-p['mu'])/p['sigma'],
    'rayleigh': lambda x, p: mp.log(x)-2*mp.log(p['sigma'])-x**2/(2*p['sigma']**2),
    'hnorm': lambda x, p: mp.log(2/mp.pi)/2-mp.log(p['sigma'])-(x-p['mu'])**2/(2*p['sigma']**2) if x >= p['mu'] else -mp.inf,
}
for family, f in DENSITY.items():
    k = (family, 'class', 'subnormal'); r = new[k]
    x = [num(v) for v in r['data']]; p = {n: num(v) for n, v in r['result']['estimates'].items()}
    print('%-8s exact %s  2026-11-15 %r  2026-11-14 %r' % (family, mp.nstr(sum(f(v, p) for v in x), 17),
          number(r['result']['logLik']), number(old[k]['result']['logLik'])))
