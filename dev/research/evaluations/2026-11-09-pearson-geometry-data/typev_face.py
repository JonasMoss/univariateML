# Check: Pearson IV and VI near the type V face share one expansion in delta.
# IV: nu = 1/eps, alpha = beta*eps, delta = eps^2.  VI: roots lambda -+ gamma, delta = -gamma^2/beta^2.
# Prediction: log f - log f_V = delta*(s(y) - E_V s) + O(delta^2), y = lambda - x,
#   s(y) = -m beta^2/y^2 + beta^3/(3 y^3),  E_V s = (2m-1)(2m)(1-m)/3.
from mpmath import mp, mpf, loggamma, log, atan, beta as B, re, sqrt
mp.dps = 60
lam = mpf(0)

def logf_V(x, m, b):
    a, y = 2*m - 1, lam - x
    return a*log(b) - loggamma(a) - (a + 1)*log(y) - b/y

def logf_IV(x, m, nu, al):
    z = (x - lam)/al
    logk = 2*re(loggamma(m + 1j*nu/2)) - 2*loggamma(m) - log(al) - log(B(m - mpf(1)/2, mpf(1)/2))
    return logk - m*log(1 + z*z) - nu*atan(z)

def logf_VI(x, m, g, b):
    y = lam - x
    p = (b/g - 2*m)/2; q = (-b/g - 2*m)/2
    logZ = (1 - 2*m)*log(2*g) + log(B(p + 1, 2*m - 1))
    return p*log(y - g) + q*log(y + g) - logZ

for m in (mpf('1.7'), mpf('0.8')):
    b = mpf('1.3')
    Es = (2*m - 1)*(2*m)*(1 - m)/3
    print(f"m={float(m)} beta={float(b)}")
    for y in (mpf('0.4'), mpf(1), mpf(3)):
        x = lam - y
        pred = -m*b**2/y**2 + b**3/(3*y**3) - Es
        row = []
        for d in (mpf('1e-3'), mpf('5e-4'), mpf('2.5e-4')):
            eps = sqrt(d)
            iv = (logf_IV(x, m, 1/eps, b*eps) - logf_V(x, m, b))/d - pred
            vi = (logf_VI(x, m, b*eps, b) - logf_V(x, m, b))/(-d) - pred
            row.append((float(iv), float(vi)))
        print(f"  y={float(y):4}: pred={float(pred):+.6f}  resid IV {[f'{r[0]:+.2e}' for r in row]}  VI {[f'{r[1]:+.2e}' for r in row]}")
