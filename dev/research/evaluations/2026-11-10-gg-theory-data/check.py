#!/usr/bin/env python3
"""High-precision spot checks; not a global certificate."""
import mpmath as m
m.mp.dps = 70

def close(a,b,tol):
    assert abs(a-b) < m.mpf(tol), (a,b,abs(a-b))

def ell(z,q):
    if q == 0:
        return -m.log(2*m.pi)/2-z*z/2
    a=1/(q*q)
    return m.log(abs(q)) + a*m.log(a) - m.loggamma(a) + z/q-a*m.exp(q*z)

def side(t, u):
    k=m.log(sum(m.exp(t*x) for x in u)/len(u))
    # solve in reciprocal gamma shape, avoiding convergence to infinity
    r=m.findroot(lambda r: m.log(1/r)-m.digamma(1/r)-k, (k,3*k), solver='anderson')
    a=1/r
    stir=m.loggamma(a)-(a-m.mpf('.5'))*m.log(a)+a-m.log(2*m.pi)/2
    return m.log(abs(t))+m.log(a)/2-stir-a*k+m.mpf('.5'),a,k

u0=[m.mpf(x) for x in [-2,-1,0,1,3]]
mean=sum(u0)/len(u0); sd=m.sqrt(sum((x-mean)**2 for x in u0)/len(u0)); u=[(x-mean)/sd for x in u0]
k3=sum(x**3 for x in u)/len(u)
for t in [m.mpf('1e-4'),m.mpf('-1e-4'),m.mpf('1e-6'),m.mpf('-1e-6')]:
    d,a,k=side(t,u)
    close(d/t,-k3/6,'0.00002')
    # Q=sign(t)/sqrt(a), sigma=sd/(abs(t) sqrt(a)), mu=mean + sd K/t
    q=m.sign(t)/m.sqrt(a); sigma=sd/(abs(t)*m.sqrt(a)); mu=mean+(sd/t)*k
    direct=sum(ell((x-mu)/sigma,q)-m.log(sigma) for x in u0)/len(u0)
    normal=-m.log(sd)-m.log(2*m.pi)/2-m.mpf('.5')
    close(d,direct-normal,'1e-48')
    print('profile',m.nstr(t,3),'delta/t',m.nstr(d/t,12),'Q',m.nstr(q,9))
for z in [m.mpf('-2'),m.mpf('0'),m.mpf('1.2')]:
    q=m.mpf('1e-8')
    derivative=(ell(z,q)-ell(z,-q))/(2*q)
    close(derivative,-z**3/6,'1e-14')
    print('Q score',str(z),m.nstr(derivative,14))
phi=lambda z:m.exp(-z*z/2)/m.sqrt(2*m.pi)
scores=[lambda z:z,lambda z:z*z-1,lambda z:-z**3/6]
info=m.matrix(3)
for i in range(3):
    for j in range(3):info[i,j]=m.quad(lambda z: scores[i](z)*scores[j](z)*phi(z),[-m.inf,m.inf])
close(m.det(info),m.mpf(1)/3,'1e-50')
print('Fisher matrix (sigma=1):',str(info).replace('\n','; '),'det=',m.nstr(m.det(info),12))
print('PASS: high-precision seam, reparameterization, score and information checks')
# Fixed-mu endpoint limits, including observations at the endpoint.
for sign in [1,-1]:
    q=sign*m.mpf('1000'); d=m.mpf('1.7'); sigma=d/abs(q)
    for y in [m.mpf('0'),-sign*m.mpf('0.8')]:
        observed=ell(y/sigma,q)-m.log(sigma)
        limit=-m.log(d)-abs(y)/d
        close(observed,limit,'0.00003')
    print('fixed-mu face sign',sign,'log-density error',m.nstr(observed-limit,8))
# Fixed sigma: the profiled mode is lower than the normal peak for nonzero Q.
for q in [m.mpf('.1'),m.mpf('1'),m.mpf('10')]:
    a=1/q**2
    remainder=m.loggamma(a)-(a-m.mpf('.5'))*m.log(a)+a-m.log(2*m.pi)/2
    assert remainder > 0
    close(ell(m.mpf('0'),q)+m.log(2*m.pi)/2,-remainder,'1e-60')
    print('fixed-sigma mode deficit',str(q),m.nstr(remainder,9))
print('PASS: fixed-mu endpoint and fixed-sigma mode checks')
