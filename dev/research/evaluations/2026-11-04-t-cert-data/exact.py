"""Exact rational counterexample: negative normal slope, finite nu=3 wins."""
from fractions import Fraction as Q
import json
from pathlib import Path

def atan_bounds(x,terms=48):
    s=sum(((-1)**j*x**(2*j+1)/Q(2*j+1) for j in range(terms)),Q(0))
    next_term=(-1)**terms*x**(2*terms+1)/Q(2*terms+1)
    return min(s,s+next_term),max(s,s+next_term)

def log_unit(x,terms=48):
    assert Q(1)<=x<=Q(2)
    t=(x-1)/(x+1)
    s=2*sum((t**(2*j+1)/Q(2*j+1) for j in range(terms)),Q(0))
    remainder=2*t**(2*terms+1)/(Q(2*terms+1)*(1-t*t))
    return s,s+remainder

def log_bounds(x):
    power=0
    while x>=2:x/=2;power+=1
    while x<1:x*=2;power-=1
    lo,hi=log_unit(x);l2,u2=log_unit(Q(2))
    return lo+min(power*l2,power*u2),hi+max(power*l2,power*u2)

al,au=atan_bounds(Q(1,5));bl,bu=atan_bounds(Q(1,239))
pl,pu=16*al-4*bu,16*au-4*bl
ints=[-4,-3,-2,-1,1,2,3,4,-101,-99,99,101]
x=[Q(v,100) for v in ints];n=len(x);v=sum(q*q for q in x)/n
m4=sum(q**4 for q in x)/n/(v*v);A=Q(n,4)*(m4-3);assert A<0
# At nu=3, mu=0, ordinary scale=1/3, delta = n/2 log(24v/pi)
# +n/2 -2 sum log(1+3x^2). No root solve appears in this proof.
l,_=log_bounds(24*v/pu);_,u=log_bounds(24*v/pl)
lo=Q(n,2)*(l+1);hi=Q(n,2)*(u+1)
for q in x:
    l,u=log_bounds(1+3*q*q);lo-=2*u;hi-=2*l
assert lo>Q(49,100) and hi-lo<Q(1,10**35)
precision=10**12
lower=Q((lo*precision).numerator//(lo*precision).denominator,precision)
upper=Q(-((-hi*precision).numerator//(-hi*precision).denominator),precision)
assert lower<=lo<=hi<=upper
# An algebraic zero-slope member of the same symmetric cluster family.
# t=e^2 is the positive root of 2304 t^2+1440.144 t-.0192=0.
tlo,thi=Q(1,100000),Q(1,50000)
def polynomial(t):return 2304*t*t+Q(180018,125)*t-Q(12,625)
assert polynomial(tlo)<0<polynomial(thi)
# Positive derivative gives exactly one root in this interval. At that
# root m4=3 exactly; symmetry gives m3=0, and B=21-2*m6.
outer6=2*(Q(99,100)**6+Q(101,100)**6)
m6_upper=144*(9780*thi**3+outer6)/(60*tlo+Q(10001,2500))**3
B_lower=21-2*m6_upper
assert B_lower>2
result={'arithmetic':'Python Fraction, exact rational operations','pi_identity':'16 atan(1/5)-4 atan(1/239)',
        'atan_terms':48,'log_terms':48,'A_exact':str(A),'nu':3,'mu':0,'ordinary_scale':'1/3',
        'finite_minus_normal_lower':str(lower),'finite_minus_normal_upper':str(upper),
        'claim':'finite witness exceeds the normal by more than 49/100; normal inward slope is strictly negative',
        'algebraic_zero_slope':{'t_lower':str(tlo),'t_upper':str(thi),'polynomial':'2304 t^2+1440.144 t-.0192',
                               'A_exact':0,'B_proved_greater_than':2,'B_rational_lower_bound':str(B_lower)}}
(Path(__file__).resolve().parent/'exact.json').write_text(json.dumps(result,indent=2)+'\n')
print('Exact rational negative-slope finite-winner counterexample passed.')
