"""Independent 70-digit root/likelihood checks. Run under one CPU slot.
Build probe.cpp against univariateML-cpp/include, then pass its binary path.
No fixtures or goldens are rewritten.
"""
import json, math, random, subprocess, sys, time
from collections import Counter
from pathlib import Path
import mpmath as mp
mp.mp.dps = 70
def log_survival(t):
    return mp.log(-mp.expm1(-t)) if t < 1 else mp.log1p(-mp.exp(-t))
rng = random.Random(1163)
cases = []
for family in ('nbinom', 'kumar'):
    golden = json.loads(Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json').read_text())
    cases.append((family, golden['data']))
    for j in range(24):
        if family == 'kumar':
            a, b = math.exp(rng.uniform(-3, 3)), math.exp(rng.uniform(-3, 3))
            x = [(-math.expm1(math.log(rng.random()) / b)) ** (1/a) for _ in range(20)]
            x = [max(math.nextafter(0., 1.), min(math.nextafter(1., 0.), v)) for v in x]
        else:
            size, p = math.exp(rng.uniform(-2, 2)), rng.uniform(.1, .9)
            x = []
            for _ in range(20):
                lam = rng.gammavariate(size, (1-p)/p)
                threshold, product, k = math.exp(-lam), 1., 0
                while product > threshold:
                    product *= rng.random()
                    k += 1
                x.append(max(k-1, 0))
        cases.append((family, x))
cases += [('nbinom', [9000,11000]), ('nbinom', [0]*9+[1000]), ('nbinom', [0,1,2,3,4]*999+[0,4]),
          ('kumar', [.0000000001,.1,.3,.9,.9999999999])]
payload = ''.join(f'{f} {len(x)} ' + ' '.join(map(str,x))+'\n' for f,x in cases)
start = time.monotonic()
run = subprocess.run([sys.argv[1]], input=payload, text=True, capture_output=True, check=True)
rows = []
for (family, x), line in zip(cases, run.stdout.splitlines(), strict=True):
    status, a, b, ll, work = line.split()
    a, b, ll = map(float, (a,b,ll))
    n = len(x)
    z = list(map(mp.mpf, x))
    if family == 'nbinom':
        mean = sum(z)/n
        var = sum((v-mean)**2 for v in z)/n
        if not mean or var <= mean:
            assert int(status) == (0 if not mean else 1)
            rows.append(dict(family=family,n=n,status=int(status),kind='attained zero or Poisson'))
            continue
        counts = Counter(x)
        def score(r):
            return sum(k*(mp.digamma(r+v)-mp.digamma(r)) for v,k in counts.items())/n-mp.log1p(mean/r)
        def likelihood(r,p):
            return sum(k*(mp.loggamma(r+v)-mp.loggamma(r)-mp.loggamma(v+1)+r*mp.log(p)+v*mp.log1p(-p)) for v,k in counts.items())
    else:
        logs = [-mp.log(v) for v in z]
        T = sum(logs)/n
        def score(r):
            G = sum(-log_survival(r*t) for t in logs)/n
            D = sum(r*t/mp.expm1(r*t) for t in logs)/n
            return 1-r*T+(1/G-1)*D
        def likelihood(r,p):
            return n*(mp.log(r)+mp.log(p))-(r-1)*sum(logs)+(p-1)*sum(log_survival(r*t) for t in logs)
    lo, hi = mp.mpf(1), mp.mpf(1)
    while score(lo) <= 0: lo /= 2
    while score(hi) >= 0: hi *= 2
    for _ in range(180):
        mid = (lo+hi)/2
        if score(mid) > 0: lo = mid
        else: hi = mid
    root = (lo+hi)/2
    prob = root/(root+mean) if family == 'nbinom' else -n/sum(log_survival(root*t) for t in logs)
    if int(status) != 0:
        rows.append(dict(family=family,n=n,status=int(status),kind='numerical stop',oracle_a=float(root)))
        continue
    ea, eb = float(abs(mp.mpf(a)/root-1)), float(abs(mp.mpf(b)/prob-1))
    ell = float(abs(mp.mpf(ll)-likelihood(root,prob)))
    assert max(ea,eb) < 1e-6, (family, ea,eb)
    assert ell < 1e-7*n, (family, ell)
    rows.append(dict(family=family,n=n,status=0,relative_a_error=ea,relative_b_error=eb,loglik_error=ell,certificate_evaluations=int(work)))
result = dict(seed=1163,decimal_digits=70,seconds=time.monotonic()-start,binary=sys.argv[1],cases=rows)
Path(sys.argv[2]).write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:v for k,v in result.items() if k!='cases'}))
print('cases',len(rows),'numerical stops',sum(r.get('kind')=='numerical stop' for r in rows))
