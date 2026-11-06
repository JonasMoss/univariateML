"""Independent arbitrary-precision score roots for the original double datum."""
import math
import mpmath as m
m.mp.dps = 70
x = m.mpf(math.exp(-.001))
y = m.log(x)
def ll(a):
    return 2*m.log(a)+a*y-m.log(x)-(a+1)*m.log1p(m.exp(a*y))
def score(a):
    return 2/a+y-m.log1p(m.exp(a*y))-(a+1)*y/(1+m.exp(-a*y))
for bracket in [(2,4),(100,1000),(8000,10000)]:
    a = m.findroot(score,bracket)
    print('shape',m.nstr(a,60),'logLik',m.nstr(ll(a),60),'curvature',m.nstr(m.diff(score,a),30))
