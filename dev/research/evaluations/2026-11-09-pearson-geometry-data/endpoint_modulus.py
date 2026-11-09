# Free endpoint with exponent p: gamma with shape a = p + 1, rate 1, threshold tau.
# Threshold shifts form a location family, so H^2(tau, tau + h) = H2(h) with
# H^2 = int (sqrt f - sqrt g)^2. Claims:
#   p < 1: H2(h) ~ C h^(p+1);  p = 1: H2(h) ~ h^2 log(1/h) / 4;  p > 1: H2(h) ~ I h^2 / 4
#   with threshold information I = 1/(a - 2) = 1/(p - 1).
# Chord geometry: the threshold curve behaves like fractional Brownian motion with
# Hurst index s = (p+1)/2 (capped at 1): the cosine between chords over [0,h] and
# [2h,3h] is (H2(h) + H2(3h) - 2 H2(2h)) / (2 H2(h)) -> (1 + 3^(2s) - 2^(2s+1))/2.
# p = 0 gives orthogonal chords (Halmos's crinkled arc); p > 1 gives parallel chords.
from mpmath import mp, mpf, sqrt, quad, exp, gamma, gammainc, log, inf
mp.dps = 50

def H2(p, h):
    c = 1/gamma(p + 1)
    sf = lambda y: sqrt(c*y**p*exp(-y)) if y > 0 else mpf(0)
    near = gammainc(p + 1, 0, h, regularized=True)          # mass of f on (0, h)
    pts = [h] + [h*mpf(2)**j for j in range(1, 80) if h*mpf(2)**j < 60] + [mpf(60), inf]
    return near + quad(lambda x: (sf(x) - sf(x - h))**2, pts)

for p in (mpf('-0.5'), mpf(0), mpf('0.5'), mpf(1), mpf('1.5')):
    s = min((p + 1)/2, mpf(1))
    pred_cos = (1 + 3**(2*s) - 2**(2*s + 1))/2
    print(f"p={float(p):+.1f}  Hurst s={float(s):.2f}  predicted chord cosine {float(pred_cos):+.3f}")
    prev = None
    for e in range(2, 8):
        h = mpf(10)**(-e)
        v1, v2, v3 = H2(p, h), H2(p, 2*h), H2(p, 3*h)
        cosc = (v1 + v3 - 2*v2)/(2*v1)
        if p < 1:   norm, lab = v1/h**(p + 1), "H2/h^(p+1)"
        elif p == 1: norm, lab = v1/(h**2*log(1/h)), "H2/(h^2 log(1/h))"
        else:        norm, lab = v1/h**2, "H2/h^2"
        slope = "" if prev is None else f"slope {float(log(prev/v1)/log(10)):.4f}"
        print(f"   h=1e-{e}: {lab}={float(norm):.5f}  chord cos {float(cosc):+.4f}  {slope}")
        prev = v1
    if p > 1: print(f"   target I/4 = {float(1/(4*(p - 1))):.5f}")
    if p == 1: print("   target 1/4 (approached slowly: H2 = h^2 (log(1/h) + O(1))/4)")
