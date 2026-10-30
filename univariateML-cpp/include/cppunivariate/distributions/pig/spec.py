"""Half-integer Bessel polynomial, independent of the fitter recurrence."""
import sympy as sp

def log_density(symbols, x):
    mean, shape = symbols['mean'], symbols['shape']
    z = sp.sqrt(shape*(2+shape/mean**2))
    # The golden has finite integer support; Piecewise unfolds its exact
    # half-integer K polynomials before symbolic differentiation.
    import json
    from pathlib import Path
    maxk = max(json.loads(Path(__file__).with_name('golden.json').read_text())['data'])
    branches=[]
    for k in range(int(maxk)+1):
        n=max(0,k-1)
        poly=sum(sp.factorial(n+j)/(sp.factorial(j)*sp.factorial(n-j))*(2*z)**(-j) for j in range(n+1))
        lp=-2*mean/(1+sp.sqrt(1+2*mean**2/shape))+k*sp.log(mean/sp.sqrt(1+2*mean**2/shape))-sp.loggamma(k+1)+sp.log(poly)
        branches.append((lp,sp.Eq(x,k)))
    return sp.Piecewise(*branches, (-sp.oo,True))
