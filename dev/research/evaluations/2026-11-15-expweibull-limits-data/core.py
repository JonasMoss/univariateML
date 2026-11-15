"""Shared mpmath tools for the expweibull limit analysis (y = log x data, log-likelihood in the x scale)."""
import mpmath as mp

def mpset(d): mp.mp.dps = d

def faces(y):
    """Return (P, I, k_hat, logs_hat). y: list of mpf log observations. Likelihood of x (Jacobian -sum y included)."""
    n = len(y); y = list(y); sy = sum(y)
    yp = max(y); Dp = sum(yp - v for v in y)
    P = n*mp.log(n/Dp) - sy - n if Dp > 0 else mp.inf
    def Iof(k):
        S = sum(mp.e**(-k*(v-yp)) for v in y)   # shifted: sum e^{-k y} = e^{-k yp} S
        # I(k) = n log k - sy - k sy + n log n - n log sum e^{-k y} - n
        return n*mp.log(k) - sy - k*sy + n*mp.log(n) - n*(-k*yp + mp.log(S)) - n
    dI = lambda k: mp.diff(Iof, k)
    lo, hi = mp.mpf('1e-6'), mp.mpf(10)
    while dI(hi) > 0: hi *= 4        # bracket
    for _ in range(300):
        mid = (lo+hi)/2
        if dI(mid) > 0: lo = mid
        else: hi = mid
    k = (lo+hi)/2
    S = sum(mp.e**(-k*(v-yp)) for v in y)
    # scale: s^k = n/sum x^-k  -> log s = (log n - log sum e^{-k y})/k
    ls = (mp.log(n) - (-k*yp + mp.log(S)))/k
    return P, Iof(k), k, ls

def Acoef(y, k, ls):
    """First order (1/T) correction of the inverse-Weibull face: l = I + A/T + O(T^-2), T = log c."""
    u = [v - ls for v in y]
    return k*sum(u) + (k**2/2)*sum(ui**2*(mp.e**(-k*ui) - 1) for ui in u)

def h(t):
    return mp.log1p(-mp.e**(-t)) if t > 1 else mp.log(-mp.expm1(-t))

def lp(y, mu, b):
    n = len(y)
    t = [mp.e**(b*(v-mu)) for v in y]
    H = sum(h(ti) for ti in t)
    c = -n/H
    ll = n*(mp.log(b)+mp.log(c)) - sum(y) + sum(mp.log(ti)-ti for ti in t) + (c-1)*H
    return ll, c

def lbc(y, b, T):
    """max over mu of l(mu,b,c=e^T): unique root of sum Q_c(t_i)=0 (strictly increasing in mu). Returns (l, mu)."""
    n = len(y); c = mp.e**T
    def S(mu):
        t = [mp.e**(b*(v-mu)) for v in y]
        return sum(1 - ti + (c-1)*ti/mp.expm1(ti) for ti in t)
    lo = min(y) - (T + 50)/b - 1     # t huge -> S<0
    hi = max(y) + 50/b + 1           # t tiny -> S>0 (=nc)
    for _ in range(64):
        mid = (lo+hi)/2
        if S(mid) > 0: hi = mid
        else: lo = mid
    mu = (lo+hi)/2
    t = [mp.e**(b*(v-mu)) for v in y]
    ll = n*(mp.log(b)+T) - sum(y) + sum(mp.log(ti)-ti for ti in t) + (c-1)*sum(h(ti) for ti in t)
    return ll, mu
