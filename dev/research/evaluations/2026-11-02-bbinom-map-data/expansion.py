#!/usr/bin/env python3
import sympy as s
import mpmath as mp
mp.mp.dps=60
x,a,c=s.symbols('x a c', positive=True)
# Euler--Maclaurin for -sum log(1+a/(c*N+j)), and the finite x products.
e,t=s.symbols('eps t', positive=True)
f=s.log(1+a*e/(c+t)).series(e,0,3).removeO().expand()
f1=f.coeff(e,1); f2=f.coeff(e,2)
total=-(f1.subs(t,0)-f1.subs(t,1))/2-s.integrate(f2,(t,0,1))
assert s.simplify(total-a*(a-1)/(2*c*(c+1))) == 0
term=s.expand(-x*(x-1)/2+x*(x+1)/(2*(c+1))+a*(a-1)/(2*c*(c+1)))
print('symbolic coefficient:',term)
mu,v=s.symbols('mu v', positive=True)
mean_coefficient=-(v+mu**2-mu)/2+mu*(v+mu**2+mu)/(2*(a+mu))+mu**2*(a-1)/(2*(a+mu))
assert s.simplify(mean_coefficient-(a*(mu-v)+mu**2)/(2*(a+mu)))==0
print('coefficient per observation:',s.factor(mean_coefficient))
f2,f3=s.symbols('F2 F3', real=True)
assert s.simplify((-2*f3-3*f2)/12+mu**3/2-mu**3/3-((mu**3-f3)/6-f2/4))==0
print('NB-to-Poisson 1/a^2 coefficient per observation:',(mu**3-f3)/6-f2/4)
print('sample: x=(0,1,2,4), a=5.203397940581812, mu=1.75')
xx=[0,1,2,4]; aa=mp.mpf('5.203397940581812'); mu=mp.mpf('1.75'); cc=aa/mu
coef=sum(-z*(z-1)/2+mp.mpf(z*(z+1))/(2*(cc+1))+aa*(aa-1)/(2*cc*(cc+1)) for z in xx)
def nb(z): return mp.loggamma(z+aa)-mp.loggamma(aa)-mp.loggamma(z+1)+aa*mp.log(cc/(cc+1))+z*mp.log(1/(cc+1))
def bb(z,n): return mp.loggamma(n+1)-mp.loggamma(z+1)-mp.loggamma(n-z+1)+mp.loggamma(z+aa)+mp.loggamma(n-z+cc*n)-mp.loggamma(n+aa+cc*n)-mp.loggamma(aa)-mp.loggamma(cc*n)+mp.loggamma(aa+cc*n)
print('coefficient',mp.nstr(coef,18))
for n in [100,1000,10000,100000]: print(n,mp.nstr(n*sum(bb(z,n)-nb(z) for z in xx),18))
print('binomial-face rho slope, x=(0,1,2), N=2, p=1/2: 1')
x2=[0,1,2]; nn=2; pp=mp.mpf('.5')
base=sum(mp.loggamma(nn+1)-mp.loggamma(z+1)-mp.loggamma(nn-z+1)+z*mp.log(pp)+(nn-z)*mp.log(1-pp) for z in x2)
for rho in [mp.mpf('0.01'),mp.mpf('0.001'),mp.mpf('0.0001')]:
 a2=pp*(1/rho-1);b2=(1-pp)*(1/rho-1)
 ll=sum(mp.loggamma(nn+1)-mp.loggamma(z+1)-mp.loggamma(nn-z+1)+mp.loggamma(z+a2)+mp.loggamma(nn-z+b2)-mp.loggamma(nn+a2+b2)-mp.loggamma(a2)-mp.loggamma(b2)+mp.loggamma(a2+b2) for z in x2)
 print('rho',rho,'slope estimate',mp.nstr((ll-base)/rho,15))
