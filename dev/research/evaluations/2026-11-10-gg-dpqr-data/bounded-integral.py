"""Exploratory Simpson enclosure for standardized gamma tails.

Analytic fourth-derivative bounds; floating point is not outward rounded.
"""
import math

def expm1_minus(z):
    if abs(z)>=0.1:
        return math.expm1(z)-z
    term=0.5
    total=term
    for k in range(3,25):
        term*=z/k
        total+=term
    return z*z*total

def tail(w,q,tol=1e-12):
    sgn=-1 if w<0 else 1
    z=q*w
    eq=math.exp(z)
    slope=math.expm1(z)/q if q else w
    rate=max(1,abs(slope))
    def delta(y):
        d=sgn*y/rate
        a=q*d
        extra=(expm1_minus(a)/(a*a)) if a else 0.5
        return sgn*y*slope/rate + eq*d*d*extra
    def f(y):
        return math.exp(-delta(y))
    def grad(y):
        a=q*sgn*y/rate
        diff=(math.expm1(a)/q) if q else sgn*y/rate
        return sgn*(slope+eq*diff)/rate
    def curvature(y):
        return math.exp(z+q*sgn*y/rate)/(rate*rate)
    T=1.
    while f(T)/grad(T)>tol/4:
        T*=2
    residual=f(T)/grad(T)
    evals=0
    def integral(a,b,fa,fm,fb,budget,depth):
        nonlocal evals
        evals+=1
        d=b-a
        m1=max(abs(grad(a)),abs(grad(b)))
        m2=max(curvature(a),curvature(b))
        c=abs(q)/rate
        bound=fa*(m1**4+6*m1*m1*m2+3*m2*m2+4*m1*m2*c+m2*c*c)*d**5/2880
        estimate=d*(fa+4*fm+fb)/6
        if bound<=budget:
            return estimate,bound
        if depth==0: raise ArithmeticError('depth')
        m=(a+b)/2
        left=integral(a,m,fa,f((a+m)/2),fm,budget/2,depth-1)
        right=integral(m,b,fm,f((m+b)/2),fb,budget/2,depth-1)
        return left[0]+right[0],left[1]+right[1]
    I,E=integral(0,T,1,f(T/2),f(T),tol/2,30)
    return I/rate,(E+residual)/rate,evals

if __name__=='__main__':
    for q in (0,1e-8,-.01,.01):
        for w in (0,1,-1,40,-40,100):
            print(q,w,tail(w,q))
