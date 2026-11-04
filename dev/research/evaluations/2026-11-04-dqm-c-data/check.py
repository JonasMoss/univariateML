"""Finite numerical illustrations; no numerical claim of proof or tail certification."""
import csv
import json
import platform
from pathlib import Path
import sys
import numpy as np
import scipy
from scipy import special as sp, stats

HERE = Path(__file__).resolve().parent
IDS = 'bbinom binom bnbinom cmb cmp dgamma dlaplace dnorm dunif dweibull geom hyper lgser nbinom nhyper pig pois skellam tbinom tpois yulesimon zeta zib zinb zip zipf zmbinom zmgeom zmlogarithmic zmnbinom zmpois ztbinom ztgeom ztnbinom ztpois'.split()
COLUMNS = 'id free held_or_integer dqm_everywhere dqm_fail_set info_nonsingular_everywhere info_singular_set k_every_x k_ae support_moves cramer observed_hessian_ok argument notes'.split()

def illustrate(name, theta, x, logpmf, rank):
    theta = np.array(theta, dtype=float)
    h = 2e-4
    lp = logpmf(theta, x)
    p = np.exp(lp)
    d = len(theta)
    score = np.empty((len(x), d))
    hess = np.empty((len(x), d, d))
    eye = np.eye(d) * h
    for j in range(d):
        score[:, j] = (logpmf(theta+eye[j], x)-logpmf(theta-eye[j], x))/(2*h)
        hess[:, j, j] = (logpmf(theta+eye[j], x)-2*lp+logpmf(theta-eye[j], x))/h**2
        for k in range(j):
            hess[:, j, k] = hess[:, k, j] = (logpmf(theta+eye[j]+eye[k], x)-logpmf(theta+eye[j]-eye[k], x)-logpmf(theta-eye[j]+eye[k], x)+logpmf(theta-eye[j]-eye[k], x))/(4*h*h)
    info = (score.T*p)@score
    observed = -np.einsum('x,xij->ij', p, hess)
    eig = np.linalg.eigvalsh(info)
    numerical_rank = int(np.count_nonzero(eig > 1e-7))
    error = float(np.max(np.abs(info-observed)))
    assert abs(p.sum()-1) < 3e-7, (name, p.sum())
    assert numerical_rank == rank, (name, eig)
    assert error < 3e-5, (name, error)
    return dict(family=name, theta=theta.tolist(), x_range=[int(x[0]),int(x[-1])], retained_mass=float(p.sum()), score_mean=(p@score).tolist(), information_eigenvalues=eig.tolist(), expected_rank=rank, numerical_rank=numerical_rank, max_opg_hessian_difference=error)

def bnb(t,x):
    r,a,b=t
    return sp.gammaln(r+x)-sp.gammaln(r)-sp.gammaln(x+1)+sp.betaln(a+r,b+x)-sp.betaln(a,b)

def dnorm(t,x):
    mu,s=t
    lo=(x-mu)/s; hi=(x+1-mu)/s
    # Use the smaller tail for stable subtraction, on the finite chosen grid.
    left=sp.log_ndtr(hi); left2=sp.log_ndtr(lo)
    right=sp.log_ndtr(-lo); right2=sp.log_ndtr(-hi)
    a=np.where(lo>0,right,left); b=np.where(lo>0,right2,left2)
    return a+np.log(-np.expm1(b-a))

def dgamma(t,x):
    a,r=t
    lower=sp.gammainc(a,r*(x+1))-sp.gammainc(a,r*x)
    upper=sp.gammaincc(a,r*x)-sp.gammaincc(a,r*(x+1))
    return np.log(np.where(r*x>a,upper,lower))

def dweibull(t,x):
    q,b=t
    return np.log(q)*x**b+np.log(-np.expm1(np.log(q)*((x+1)**b-x**b)))

def pig(t,x):
    m,s=t; z=np.sqrt(s*(2+s/m**2))
    return .5*np.log(2*s/np.pi)-sp.gammaln(x+1)+s/m+.5*(x-.5)*np.log(s/(2+s/m**2))+np.log(sp.kve(x-.5,z))-z

def skellam(t,x):
    a,b=t; z=2*np.sqrt(a*b)
    return -a-b+.5*x*np.log(a/b)+np.log(sp.ive(abs(x),z))+z

def main():
    cases=[('bnbinom_off_diagonal',[2,7,3],np.arange(2000),bnb,3),('bnbinom_diagonal',[2,7,2],np.arange(2000),bnb,2),('dnorm',[.3,1.2],np.arange(-18,19),dnorm,2),('dgamma',[1.7,1.2],np.arange(80),dgamma,2),('dweibull',[.6,1.3],np.arange(45),dweibull,2),('dweibull_geometric',[.6,1.0],np.arange(120),dweibull,2),('dweibull_subunit',[.6,.7],np.arange(1000),dweibull,2),('pig',[2,3],np.arange(100),pig,2),('skellam',[2,3],np.arange(-45,46),skellam,2),('skellam_symmetric',[2,2],np.arange(-45,46),skellam,2)]
    results=[illustrate(*c) for c in cases]
    for n in (0,1,2,4):
        x=np.arange(n+1)
        results.append(illustrate('bbinom_n'+str(n),[1.3,2.1],x,lambda t,x: stats.betabinom.logpmf(x,n,*t),min(n,2)))
        def cmb(t,x):
            p,nu=t
            l=nu*(sp.gammaln(n+1)-sp.gammaln(x+1)-sp.gammaln(n-x+1))+x*np.log(p)+(n-x)*np.log1p(-p)
            return l-sp.logsumexp(l)
        results.append(illustrate('cmb_n'+str(n),[.4,.7],x,cmb,min(n,2)))
        def zib(t,x):
            p,pi=t
            l=np.log1p(-pi)+stats.binom.logpmf(x,n,p)
            return np.where(x==0,np.logaddexp(np.log(pi),l),l)
        results.append(illustrate('zib_n'+str(n),[.4,.3],x,zib,min(n,2)))
    out={'python':sys.version,'platform':platform.platform(),'numpy':np.__version__,'scipy':scipy.__version__,'seed':None,'method':'central differences, original coordinates; step 2e-4; finite sums only','tolerances':{'retained_mass':3e-7,'eigenvalue_rank_cutoff':1e-7,'hessian_opg_absolute':3e-5},'checks':results}
    (HERE/'checks.json').write_text(json.dumps(out,indent=2)+'\n')
    print(f'{len(results)} finite numerical illustrations passed')
    path=HERE/'classification.csv'
    if path.exists():
        with path.open() as f:
            reader=csv.DictReader(f); assert reader.fieldnames==COLUMNS
            rows=list(reader)
        assert [r['id'] for r in rows]==IDS
        assert all(all(r[c] for c in COLUMNS) for r in rows)
        print(f'{len(rows)} CSV rows and exact columns passed')
if __name__=='__main__':
    main()
