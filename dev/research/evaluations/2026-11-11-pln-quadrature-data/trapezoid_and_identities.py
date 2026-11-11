# Poisson-lognormal: centred trapezoid log pmf versus adaptive quadrature, and the
# heat-equation derivative identities checked by central finite differences.
# Run: python3 -I trapezoid_and_identities.py   (numpy, scipy; 2026-11-11)
import numpy as np
from scipy.special import lambertw, gammaln
from scipy.integrate import quad
def mode(k,mu,s): return mu+k*s*s-lambertw(s*s*np.exp(mu+k*s*s)).real
def logf(t,k,mu,s): return k*t-np.exp(t)-(t-mu)**2/(2*s*s)-gammaln(k+1)-np.log(s*np.sqrt(2*np.pi))
def ref(k,mu,s):
    t=mode(k,mu,s); h=1/np.sqrt(np.exp(t)+1/s**2); f0=logf(t,k,mu,s)
    v=quad(lambda u: np.exp(logf(u,k,mu,s)-f0),t-60*h,t+60*h,points=[t],epsabs=0,epsrel=2e-14,limit=1000)[0]
    return f0+np.log(v)
def trap(k,mu,s,N):
    # centred trapezoid; left tail decays on scale s, right on scale h
    t=mode(k,mu,s); h=1/np.sqrt(np.exp(t)+1/s**2)
    L=t-9*s; R=t+9*h; step=(R-L)/N; u=np.linspace(L,R,N+1)
    f=logf(u,k,mu,s); m=f.max()
    return m+np.log(step*np.exp(f-m).sum())
print("trapezoid log-pmf error vs adaptive reference")
for k,mu,s in [(0,0.3,1.2),(3,-2,3),(7,0.3,1.2),(200,1,0.05),(0,-5,0.3)]:
    r=ref(k,mu,s)
    print(f"k={k:<3} mu={mu:<4} s={s:<4}", " ".join("N=%d:%.1e"%(N,trap(k,mu,s,N)-r) for N in (20,40,80,160)))
# derivative identities: d/dmu p_k = k p_k - (k+1) p_{k+1};  d/d(s^2) p_k = 1/2[k^2 p_k - (2k+1)(k+1) p_{k+1} + (k+2)(k+1) p_{k+2}]
P=lambda k,mu,s: np.exp(ref(k,mu,s))
k,mu,s,e=4,0.5,0.9,1e-5
dmu=(P(k,mu+e,s)-P(k,mu-e,s))/(2*e)
ds2=(P(k,mu,np.sqrt(s*s+e))-P(k,mu,np.sqrt(s*s-e)))/(2*e)
print("dmu  identity rel err %.1e"%((k*P(k,mu,s)-(k+1)*P(k+1,mu,s))/dmu-1))
print("ds2  identity rel err %.1e"%(0.5*(k*k*P(k,mu,s)-(2*k+1)*(k+1)*P(k+1,mu,s)+(k+2)*(k+1)*P(k+2,mu,s))/ds2-1))
