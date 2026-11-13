import sympy as sp

def log_density(s, x):
    k = sp.Symbol('k', integer=True)
    lo = sp.Max(0, sp.floor(s['a'])+1)
    hi = sp.floor(s['b'])
    rate = s['lambda']
    kernel = lambda v: v*sp.log(rate)-sp.loggamma(v+1)
    return kernel(x)-sp.log(sp.Sum(sp.exp(kernel(k)), (k,lo,hi)))
