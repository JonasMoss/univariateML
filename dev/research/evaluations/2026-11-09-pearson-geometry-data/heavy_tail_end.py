# Heavy-tail end of the Pearson system (type VII, tail exponent -> 1).
# f(x) = k (1 + z^2)^(-m), z = (x - lam)/alpha, d = 2m - 1 -> 0.
# H^2(f, g) = int (sqrt f - sqrt g)^2 (range [0, 2]); g = f shifted by Delta.
# Claims: (1) location information I = d(d+1)/((d+3) alpha^2), so H^2 ~ I Delta^2 / 4
# for small Delta; (2) at a fixed gap H^2 -> 0 like d; (3) for large gaps
# H^2 = 2 d log(Delta/alpha) + O(d) as d -> 0 (each tail contributes d log Delta to
# the lost overlap): Hellinger-close members with locations far apart.
from mpmath import mp, mpf, beta, sqrt, quad, log, inf
mp.dps = 30

def h2(d, Delta, alpha=mpf(1)):
    m = (1 + d)/2
    k = 1/(alpha*beta(d/2, mpf(1)/2))
    sf = lambda x: sqrt(k*(1 + (x/alpha)**2)**(-m))
    pts = {mpf(0), Delta}
    for e in range(-3, int(mp.log10(Delta + 1)) + 6):
        for c in (mpf(0), Delta):
            pts.add(c + mpf(10)**e); pts.add(c - mpf(10)**e)
    pts = [-inf] + sorted(pts) + [inf]
    return quad(lambda x: (sf(x) - sf(x - Delta))**2, pts)

print("small gap Delta = 1e-3, alpha = 1: H^2/(Delta^2/4) vs I = d(d+1)/(d+3)")
for d in (mpf('0.5'), mpf('0.1'), mpf('0.01')):
    Dl = mpf('1e-3')
    print(f"  d={float(d):<5} H^2/(Delta^2/4)={float(h2(d, Dl)/(Dl**2/4)):.6f}  I={float(d*(d+1)/(d+3)):.6f}")

print("gaps Delta, alpha = 1: H^2, H^2/d, and H^2/d - 2 log Delta (should settle as d -> 0, Delta -> inf)")
for d in (mpf('0.1'), mpf('0.01'), mpf('0.001')):
    for Dl in (mpf(1), mpf(10), mpf(100), mpf(10)**4, mpf(10)**6):
        v = h2(d, Dl)
        print(f"  d={float(d):<6} Delta={float(Dl):<8g} H^2={float(v):.5f}  H^2/d={float(v/d):8.4f}  H^2/d-2logDelta={float(v/d - 2*log(Dl)):8.4f}")
