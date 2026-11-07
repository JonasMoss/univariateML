"""Exact rational bounds: two finite maximizers must exist, without fitting.
Machin's identity bounds pi by alternating series; square roots use integer
arithmetic; log uses its positive atanh series with a geometric tail bound.
"""
from fractions import Fraction as F
from math import isqrt

def add(a,b): return (a[0]+b[0],a[1]+b[1])
def neg(a): return (-a[1],-a[0])
def mul(a,b):
    v=[x*y for x in a for y in b]
    return min(v),max(v)
def scalar(x): return F(x),F(x)
def div(a,b):
    assert b[0]>0
    return mul(a,(1/b[1],1/b[0]))
def sqrt(a):
    assert a[0]>=0
    scale=10**30
    def lower(x): return F(isqrt(x.numerator*scale*scale//x.denominator),scale)
    return lower(a[0]),lower(a[1])+F(1,scale)
def sq(a):
    assert a[0]>=0
    return a[0]**2,a[1]**2
def atan(x):
    z=sum((-1)**j*x**(2*j+1)/F(2*j+1) for j in range(60))
    return z,z+x**121/121
pi=add(mul(scalar(16),atan(F(1,5))),neg(mul(scalar(4),atan(F(1,239)))))
m=sqrt(div(scalar(2),pi));h2=add(scalar(1),neg(sq(m)))
su=sqrt(add(scalar(1),mul(h2,scalar(F(35,6)**2))))
sd=scalar(F(151,200));r=div(su,sd);e=mul(m,scalar(F(35,6)))
def log_bound(x):
    assert x>1
    z=(x-1)/(x+1)
    lo=2*sum(z**(2*j+1)/F(2*j+1) for j in range(40))
    return lo,lo+2*z**81/(81*(1-z*z))
x=div(mul(scalar(12),su),scalar(37))
logx=log_bound(x[0])[0],log_bound(x[1])[1]
penalty=mul(scalar(F(1,2)),add(mul(scalar(36),sq(add(r,neg(e)))),div(add(sq(e),sq(add(r,e))),scalar(36))))
gain=add(add(mul(scalar(3),logx),div(scalar(1),sq(sd))),neg(penalty))
# Both half-normal endpoints lie strictly inside the sample extremes.
endpoint2=div(mul(sq(m),sq(sd)),h2)
assert F(7685,100000)<gain[0]<=gain[1]<F(7686,100000) and endpoint2[1]<F(999,1000)
print('L(xi=6)-L(xi=1) in',*(float(v) for v in gain))
print('Squared absolute half-normal endpoint <',float(endpoint2[1]))
print('Reflection gives L(xi)=L(1/xi). Both tails tend to -infinity;')
print('a finite global maximum exists and cannot be xi=1. Thus at least two distinct finite maxima exist.')
