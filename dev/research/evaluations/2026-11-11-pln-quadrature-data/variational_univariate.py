# Univariate Poisson-lognormal variational (Gaussian q, ELBO) fit of the same sample,
# to show that a variational estimate is not the MLE.
# Run: python3 -I variational_univariate.py   (numpy, scipy; 2026-11-11)
import numpy as np
from scipy.optimize import minimize
from scipy.special import gammaln
x=np.array([0,0,0,1,1,2,3,3,5,9,14.]); n=len(x)
def negelbo(p):
    m,ls=p[:n],p[n:2*n]; mu,lsig=p[2*n],p[2*n+1]; s2=np.exp(2*ls); sig2=np.exp(2*lsig)
    e=x*m-np.exp(m+s2/2)-gammaln(x+1)-((m-mu)**2+s2)/(2*sig2)-lsig+ls+0.5
    return -e.sum()
p0=np.r_[np.log(x+0.5),np.full(n,-1.0),0.5,0.0]
r=minimize(negelbo,p0,method="L-BFGS-B",options=dict(maxiter=20000,ftol=1e-15,gtol=1e-10))
mu,sig=r.x[2*n],np.exp(r.x[2*n+1])
print("variational: mu %.4f sigma %.4f  ELBO %.4f  mean %.4f"%(mu,sig,-r.fun,np.exp(mu+sig**2/2)))
