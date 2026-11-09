# Spectral form of the endpoint geometry, gamma with shape p + 1 and moving threshold.
# With ghat(w) = int g(x) exp(-i w x) dx and g = sqrt f:
#   phi(h) = H^2(h) = (1/pi) int (1 - cos w h) |ghat|^2 dw,   I = 4 int (g')^2 = (2/pi) int w^2 |ghat|^2 dw.
# For the gamma, |ghat(w)|^2 = Gamma(p/2+1)^2 / (Gamma(p+1) (1/4 + w^2)^(p/2+1)) ~ |w|^-(p+2),
# fBm's spectral exponent 2H + 1 with H = (p+1)/2. Compare phi(h) with the direct
# integral of endpoint_modulus.py, and I with 1/(p - 1) for p > 1.
from mpmath import mp, mpf, gamma, quadosc, quad, cos, pi, inf
import importlib.util, sys
mp.dps = 30
spec = importlib.util.spec_from_file_location("em", "endpoint_modulus.py")
src = open("endpoint_modulus.py").read().split("\nfor p in")[0]      # reuse H2() only
ns = {}; exec(src, ns); H2 = ns["H2"]

def S(p, w):
    return gamma(p/2 + 1)**2/(gamma(p + 1)*(mpf(1)/4 + w*w)**(p/2 + 1))

for p in (mpf('0.5'), mpf('1.5')):
    for h in (mpf('0.1'), mpf('0.01')):
        spec_phi = 2/pi*quadosc(lambda w: (1 - cos(w*h))*S(p, w), [0, inf], omega=h)
        print(f"p={float(p)} h={float(h)}: spectral phi={float(spec_phi):.10e}  direct H2={float(H2(p, h)):.10e}")
p = mpf('1.5')
I = 4/pi*quad(lambda w: w*w*S(p, w), [0, 1, 10, 100, inf])
print(f"p=1.5: spectral I={float(I):.10f}  target 1/(p-1)={float(1/(p - 1)):.10f}")
