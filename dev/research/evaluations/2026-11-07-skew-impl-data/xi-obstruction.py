"""Held-moment thin-endpoint prototype; finite numerical evidence only.
Shows why independent numerator/denominator intervals need an algebraic
endpoint treatment before they can certify the remaining xi-only route.
"""
import json, pathlib
import mpmath as mp
mp.mp.dps=60
nu=mp.mpf(3);h=mp.gamma((nu-1)/2)/(mp.sqrt(mp.pi)*mp.gamma(nu/2));v=1/(nu-2)-h*h
endpoint=-h/mp.sqrt(v);rho=h/(2*(nu-2)*v)
rows=[]
for xi in map(mp.mpf,(10,100,1000,10000,100000)):
    r=xi*xi
    a=1/mp.sqrt((1-r+r*r)/(nu-2)-h*h*(r-1)**2)
    b=r*a;c=-h*(b-a)
    thin=(endpoint-c)/a
    factor=(1+thin*thin)**(-(nu+1)/2)
    rows.append(dict(xi=float(xi),thin_ratio=float(thin),factor=float(factor)))
expected=(1+rho*rho)**(-(nu+1)/2)
assert abs(rows[-1]['factor']-float(expected))<mp.mpf('1e-9')
result=dict(nu=3,mean=0,sd=1,endpoint=float(endpoint),rho=float(rho),factor=float(expected),paths=rows,
    obligation='An independent interval for x-c and a loses their limiting ratio at a=0. A correlated algebraic enclosure or endpoint remainder is required; a closed half-t value overstates this held-moment face.')
path=pathlib.Path(__file__).with_suffix('.json');path.write_text(json.dumps(result,indent=2)+'\n');print(expected)
