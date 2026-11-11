# Poisson-lognormal: Lambert-W mode, 40-node Laplace-centred Gauss-Hermite log pmf
# versus adaptive quadrature, and an exact-likelihood fit of an 11-point sample.
# The k=5000 case overflows on purpose: Lambert W needs a log-form argument there.
# Run: python3 -I gauss_hermite_and_fit.py   (numpy, scipy; 2026-11-11)
import numpy as np
from scipy.special import lambertw, gammaln
from scipy.optimize import minimize
from scipy.integrate import quad

xg,wg=np.polynomial.hermite.hermgauss(40)
def mode(k,mu,s):
    return mu+k*s*s-lambertw(s*s*np.exp(mu+k*s*s)).real
def logp(k,mu,s):
    t=mode(k,mu,s); h=1/np.sqrt(np.exp(t)+1/s**2)
    tt=t+np.sqrt(2)*h*xg
    f=k*tt-np.exp(tt)-(tt-mu)**2/(2*s*s)-gammaln(k+1)
    m=f.max()
    return m+np.log(np.sum(wg*np.exp(f-m+xg**2)))+np.log(np.sqrt(2)*h/(s*np.sqrt(2*np.pi)))
def ref(k,mu,s):
    t=mode(k,mu,s); h=1/np.sqrt(np.exp(t)+1/s**2)
    f0=k*t-np.exp(t)-(t-mu)**2/(2*s*s)
    g=lambda u: np.exp(k*u-np.exp(u)-(u-mu)**2/(2*s*s)-f0)
    v=quad(g,t-60*h,t+60*h,points=[t],epsabs=0,epsrel=1e-13,limit=500)[0]
    return f0-gammaln(k+1)+np.log(v)-np.log(s*np.sqrt(2*np.pi))
mu,s,k=0.3,1.2,7; t=mode(k,mu,s)
print("score at Lambert-W mode:", k-np.exp(t)-(t-mu)/s**2)
for k,mu,s in [(0,0.3,1.2),(7,0.3,1.2),(200,1,0.05),(3,-2,3),(5000,2,2)]:
    print("k=%d mu=%g s=%g  log-pmf err %.2e"%(k,mu,s,logp(k,mu,s)-ref(k,mu,s)))
x=np.array([0,0,0,1,1,2,3,3,5,9,14])
nll=lambda p: -sum(logp(k,p[0],np.exp(p[1])) for k in x)
r=minimize(nll,[0.5,0],method="Nelder-Mead",options=dict(xatol=1e-11,fatol=1e-13,maxiter=4000))
mu,s=r.x[0],np.exp(r.x[1])
print("xbar",x.mean(),"var(n)",x.var(),"fitted PLN mean",np.exp(mu+s*s/2),"mu",mu,"sigma",s)
