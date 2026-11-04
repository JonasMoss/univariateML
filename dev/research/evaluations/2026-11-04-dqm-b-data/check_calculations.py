"""Independent density integrals illustrating (not proving) the report.
CPU: one thread; run through /tmp/umvl-cpu/run from repository root.
Analytic two-piece scores use a smooth raw chart (m,log a,log xi,nu).
No library implementation, golden data or fitted parameters are used.
"""
import json
import platform
from pathlib import Path
import numpy as np
import scipy
from scipy.integrate import quad_vec
from scipy.special import gammaln, digamma
from scipy.stats import t

OUT = Path(__file__).resolve().parent
records=[]

def two_piece(base, nu, xi):
    eta=np.log(xi)
    dim=3 if base=='normal' else 4
    def integrand(v, side):
        if v==0: return np.zeros(1+dim+dim*dim+1)
        r=side*v*xi**side
        if base=='ged':
            w=v**nu
            lg=np.log(nu)-np.log(2)-gammaln(1/nu)-w
            q=nu*w
            df=1/nu+digamma(1/nu)/nu**2-w*np.log(v)
            hmm=nu*(nu-1)*w/r**2
        elif base=='normal':
            lg=-.5*np.log(2*np.pi)-v*v/2
            q=v*v
            hmm=1/xi**(2*side)
        else:
            vv=v*v
            lg=gammaln((nu+1)/2)-gammaln(nu/2)-.5*np.log(nu*np.pi)-(nu+1)/2*np.log1p(vv/nu)
            q=(nu+1)*vv/(nu+vv)
            df=.5*digamma((nu+1)/2)-.5*digamma(nu/2)-.5/nu-.5*np.log1p(vv/nu)+(nu+1)*vv/(2*nu*(nu+vv))
            hmm=(nu+1)*(nu-vv)/(nu+vv)**2/xi**(2*side)
        s=np.array([q/r,-1+q,-np.tanh(eta)+side*q]+([] if dim==3 else [df]))
        mass=2/(xi+1/xi)*np.exp(lg)*xi**side
        # Do not numerically integrate a nonintegrable ordinary GED Hessian.
        if base=='ged' and nu<=1: hmm=0.
        return mass*np.r_[1,s,np.outer(s,s).ravel(),hmm]
    val=np.zeros(1+dim+dim*dim+1); error=0.
    for side in (-1,1):
        for lo,hi in ((0,1),(1,np.inf)):
            a,e=quad_vec(lambda v:integrand(v,side),lo,hi,epsabs=2e-8,epsrel=2e-8)
            val+=a;error+=e
    mass=val[0]; mean=val[1:1+dim]; gram=val[1+dim:-1].reshape(dim,dim)
    eigen=np.linalg.eigvalsh(gram)
    assert abs(mass-1)<2e-6 and max(abs(mean))<2e-6 and eigen[0]>1e-7
    if base!='ged' or nu>1: assert abs(val[-1]-gram[0,0])<3e-6
    records.append(dict(model=base,nu=nu,xi=xi,mass=float(mass),max_abs_mean_score=float(max(abs(mean))),eigenvalues=eigen.tolist(),location_opg=float(gram[0,0]),location_negative_hessian=float(val[-1]) if base!='ged' or nu>1 else 'not integrated: invalid information Hessian',quadrature_error_bound_estimate=float(error)))

for base, shapes in [('normal',[2]),('ged',[.7,1,1.2,2,3.5]),('t',[2.5,5,20])]:
    for nu in shapes:
        for xi in (.6,1,2): two_piece(base,nu,xi)

# Azzalini t, analytic location/scale/alpha scores, central difference in df.
def st_log(x,nu,alpha):
    v=alpha*x*np.sqrt((nu+1)/(nu+x*x))
    return np.log(2)+t.logpdf(x,nu)+t.logcdf(v,nu+1)
def st_integrand(x,nu,alpha):
    den=nu+x*x
    w=np.sqrt((nu+1)/den)
    v=alpha*x*w
    mills=np.exp(t.logpdf(v,nu+1)-t.logcdf(v,nu+1))
    dx=-(nu+1)*x/den+mills*alpha*w*nu/den
    h=2e-5*nu
    dnu=(st_log(x,nu+h,alpha)-st_log(x,nu-h,alpha))/(2*h)
    score=np.array([-dx,-1-x*dx,mills*x*w,dnu])
    f=np.exp(st_log(x,nu,alpha))
    return f*np.r_[1,score,np.outer(score,score).ravel()]
for nu in (.5,1,5,20):
    for alpha in (-2,0,2):
        val=np.zeros(21);err=0.
        for lo,hi in ((-np.inf,-1),(-1,0),(0,1),(1,np.inf)):
            v,e=quad_vec(lambda x: st_integrand(x,nu,alpha),lo,hi,epsabs=1e-7,epsrel=1e-7);val+=v;err+=e
        gram=val[5:].reshape(4,4); eigen=np.linalg.eigvalsh(gram)
        assert abs(val[0]-1)<2e-6 and max(abs(val[1:5]))<2e-6 and eigen[0]>1e-7
        records.append(dict(model='st',nu=nu,alpha=alpha,mass=float(val[0]),max_abs_mean_score=float(max(abs(val[1:5]))),eigenvalues=eigen.tolist(),quadrature_error_bound_estimate=float(err)))

# Uniform support examples: finite pointwise score moments can be misleading.
# Pareto at a=b=1: E ss'=diag(1,1), but -E ell''=[[1,-1],[-1,1]],
# and the endpoint score has mean 1, so there is no regular information identity.
# Power at alpha=beta=1: E ss'=[[1,0],[0,1]], but -E ell''=[[-1,1],[1,1]].
# Both remain non-DQM despite finite positive definite formal score moments.
metadata=dict(python=platform.python_version(),platform=platform.platform(),numpy=np.__version__,scipy=scipy.__version__,seed='none (deterministic quadrature)',tolerances=dict(integration_absolute_relative='2e-8 two-piece; 1e-7 st',mass_and_mean=2e-6,hessian_opg_absolute=3e-6,minimum_eigenvalue=1e-7,df_difference_relative_step=2e-5))
(OUT/'calculation-results.json').write_text(json.dumps(dict(environment=metadata,checks=len(records),records=records),indent=2)+'\n')
print(f'PASS: {len(records)} parameter points; normalized densities, zero score means, positive Gram eigenvalues, and applicable location Hessian identities.')
