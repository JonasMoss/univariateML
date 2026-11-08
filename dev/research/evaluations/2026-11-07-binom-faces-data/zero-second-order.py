#!/usr/bin/env python3
"""Exact rational boundary of the sufficient second-order TB criterion."""
from fractions import Fraction as F
from math import lcm,gcd
from functools import reduce
import json
q=[F(v,824) for v in [24,96,192,256,256]]
p=[F(v,824) for v in [104,0,0,608,112]]
def E(prob,f):return sum(prob[k]*f(k) for k in range(5))
a=lambda k:-F(k*(k-1),2)
b=lambda k:-F(k*(k-1)*(2*k-1),12)
c=lambda k:-F(k*k*(k-1)*(k-1),12)
m=E(q,lambda k:k);abar=E(q,a)
beta=(E(q,lambda k:k*a(k))-m*abar)/(E(q,lambda k:k*k)-m*m)
h=lambda k:a(k)-abar-beta*(k-m)
D=E(q,lambda k:h(k)**2)
t=D/2/(E(p,b)-E(q,b));assert 0<t<1
p0=[(1-t)*u+t*v for u,v in zip(q,p)]
assert sum(p0)==1
assert E(p0,lambda k:k)==m and E(p0,a)==abar
B=E(p0,b)-E(q,b)-D/2;assert B==0
C=E(p0,c)-E(q,c)-E(q,lambda k:h(k)*b(k))-E(q,lambda k:h(k)**3)/6
den=lcm(*(v.denominator for v in p0));counts=[int(v*den) for v in p0];g=reduce(gcd,counts);counts=[v//g for v in counts]
print(json.dumps(dict(rate=4,counts=counts,n=sum(counts),mix=str(t),first_exact='0',second_exact=str(B),third_profiled_exact=str(C),third_profiled=float(C),interpretation='Exact local expansion only. The sufficient second-order criterion deliberately refuses zero B. The positive formal third coefficient indicates eventual finite improvement; this expansion does not locate or certify the global finite maximizer.'),indent=2))
