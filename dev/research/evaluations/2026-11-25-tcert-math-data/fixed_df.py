"""Exact eliminant and numerical stationary locations for a subunit example."""
import json
import mpmath as mp
import sympy as sp

mp.mp.dps=80
m,t=sp.symbols('m t')
x=[-100,-99,0,99,100]
v=sp.Rational(1,2)
D=[(a-m)**2+t for a in x]
others=[sp.prod(D[:i]+D[i+1:]) for i in range(len(x))]
F=sp.expand(sum((a-m)*o for a,o in zip(x,others)))
G=sp.expand(3*sum((a-m)**2*o for a,o in zip(x,others))-10*sp.prod(D))
result=sp.resultant(F,G,t)
factors=sp.factor_list(result)[1]
stationary=[]
polynomial=sp.Poly(result,m)
roots=polynomial.intervals(eps=sp.Rational(1,10**40))
for (a,b),multiplicity in roots:
    if a>max(x) or b<min(x):
        continue
    loc=mp.mpf(str(sp.N((a+b)/2,80)))
    lo,hi=mp.mpf(0),mp.mpf(100000)
    for _ in range(230):
        mid=(lo+hi)/2
        S=3*mp.fsum((z-loc)**2/(mid+(z-loc)**2) for z in x)-10
        if S>0:
            lo=mid
        else:
            hi=mid
    tt=(lo+hi)/2
    val=mp.fsum((z-loc)/(tt+(z-loc)**2) for z in x)
    if abs(val)<mp.mpf('1e-35'):
        def ll(m,t):
            return -.5*len(x)*mp.log(t)-.75*mp.fsum(mp.log1p((z-m)**2/t) for z in x)
        mm,mt,ttt=[mp.diff(ll,(loc,tt),p) for p in [(2,0),(1,1),(0,2)]]
        determinant=mm*ttt-mt**2
        assert abs(determinant)>mp.mpf('1e-15')
        stationary.append({'location':mp.nstr(loc,28),'a2':mp.nstr(tt,28),
                           'type':'maximum' if determinant>0 and ttt<0 else 'saddle',
                           'loglik_up_to_const':mp.nstr(ll(loc,tt),26)})
print(json.dumps({'resultant_degree':polynomial.degree(),
                  'factors':[[str(f),int(c)] for f,c in factors],
                  'real_eliminant_intervals':len(roots),
                  'stationary':stationary},indent=2))
