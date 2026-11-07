"""Independent factorial-polynomial oracle, 90 decimal digits; no fitted code."""
import json
from pathlib import Path
from mpmath import mp
mp.dps = 90
out = []
for x in ([1], [0, 0, 0, 0, 10], [1, 2, 3, 5], [0, 1, 2, 3]):
    for shape in ('0.00000000000000000001', '.01', '3', '100000000000000000000'):
        lam = mp.mpf(shape)
        def degree(z, k):
            if k <= 1:
                return mp.mpf(0)
            terms = [mp.factorial(k-1+j)/mp.factorial(j)/mp.factorial(k-1-j)/(2*z)**j for j in range(k)]
            return mp.fsum(j*v for j, v in enumerate(terms))/mp.fsum(terms)
        def score(mu):
            z = mp.sqrt(2*lam+(lam/mu)**2)
            q = lam/mu/z
            D = mp.fsum(k+degree(z,k) for k in x)/len(x)
            return D*(1+q)/2/mu-1
        average = mp.mpf(sum(x))/len(x)
        lo, hi = average/2, 2*average
        for _ in range(220):
            mid = (lo+hi)/2
            if score(mid) > 0:
                lo = mid
            else:
                hi = mid
        mu = (lo+hi)/2
        # Separate Bessel-K likelihood, independent of the polynomial solve.
        A = 2+lam/mu**2
        z = mp.sqrt(lam*A)
        ll = mp.fsum(mp.log(2*lam/mp.pi)/2+lam/mu+(mp.mpf(k)-mp.mpf('.5'))/2*mp.log(lam/A)
                    +mp.log(mp.besselk(mp.mpf(k)-mp.mpf('.5'),z))-mp.loggamma(k+1) for k in x)
        out.append(dict(x=x, shape=shape, mean=str(mu), logLik=str(ll), residual=str(score(mu))))
Path(__file__).with_name('oracle.json').write_text(json.dumps(out, indent=2)+'\n')
