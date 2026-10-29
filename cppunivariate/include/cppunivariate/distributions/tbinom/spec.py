import sympy as sp

def log_density(s, x):
    k = sp.Symbol('k', integer=True)
    lo = sp.Max(0, sp.floor(s['a'])+1)
    hi = sp.floor(s['b'])
    size, prob = s['size'], s['prob']
    hi = sp.Min(hi, size)
    kernel = lambda v: v*sp.log(prob)+(size-v)*sp.log(1-prob)-sp.loggamma(v+1)-sp.loggamma(size-v+1)
    return kernel(x)-sp.log(sp.Sum(sp.exp(kernel(k)), (k,lo,hi)))
