#!/usr/bin/env python3
"""Independent research checks; no production fitter calls. Run from repository root."""
import json, platform, sys
from pathlib import Path
import numpy as np
import scipy
from scipy.optimize import root, brentq, minimize
from scipy.special import gammaln, digamma
import mpmath as mp
OUT=Path(__file__).resolve().parent

def ll(x,w,mu,s,nu):
    z=(x-mu)/s
    return float(np.sum(w)*(gammaln((nu+1)/2)-gammaln(nu/2)-.5*np.log(nu*np.pi)-np.log(s))-(nu+1)/2*np.dot(w,np.log1p(z*z/nu)))

def conditional(x,w,nu,start=None):
    n=sum(w)
    if start is None:start=[np.dot(w,x)/n,.5*np.log(np.dot(w,(x-np.dot(w,x)/n)**2)/n)]
    def score(p):
        mu,t=p;s=np.exp(t);z=(x-mu)/s;den=nu+z*z
        return np.array([np.dot(w,(nu+1)*z/den),np.dot(w,(nu+1)*z*z/den)-n])/n
    sol=root(score,start,tol=1e-10)
    if not sol.success or np.linalg.norm(score(sol.x))>1e-8:
        def fg(p):
            s=np.exp(p[1]);v=ll(x,w,p[0],s,nu);g=score(p)*n
            return -v,-np.array([g[0]/s,g[1]])
        sol=minimize(fg,start,jac=True,method='BFGS',options={'gtol':1e-9,'maxiter':1000})
        sol=root(score,sol.x,tol=1e-10)
    if np.linalg.norm(score(sol.x))>1e-7 or not np.all(np.isfinite(sol.x)):
        raise RuntimeError(('conditional score failure',nu,sol.x,score(sol.x)))
    mu,t=sol.x;s=np.exp(t);z=(x-mu)/s
    slope=n*.5*(digamma((nu+1)/2)-digamma(nu/2)-1/nu)-.5*np.dot(w,np.log1p(z*z/nu))+(nu+1)/(2*nu)*np.dot(w,z*z/(nu+z*z))
    # Geodesic tangent inequality plus an explicit containing rectangle.
    delta=min(np.diff(np.sort(x)));R=max(x)-min(x);k=max(w);r=n-k
    low=delta/2*np.sqrt((r-k/nu)/n);high=R*np.sqrt(1+1/nu)
    if not (min(x)<=mu<=max(x) and low<=s<=high):
        raise RuntimeError(('conditional containment failure',nu,mu,s,low,high))
    diameter=np.arccosh(1+(R*R+nu*(high-low)**2)/(2*nu*low*low))
    raw=score(sol.x)*n
    gap=np.hypot(np.sqrt(nu)*raw[0],raw[1])*diameter
    return {'nu':float(nu),'mu':float(mu),'scale':float(s),'ll':ll(x,w,mu,s,nu),'dnu':float(slope),'score':float(np.linalg.norm(raw)), 'conditional_value_gap_bound':float(gap)}

def moments(x,w):
    n=sum(w);mu=np.dot(w,x)/n;v=np.dot(w,(x-mu)**2)/n;z=(x-mu)/np.sqrt(v)
    m=[float(np.dot(w,z**j)/n) for j in range(7)]
    return {'n':float(n),'mean':float(mu),'variance':float(v),'m3':m[3],'m4':m[4],'m6':m[6],'normal':float(-n/2*np.log(2*np.pi*np.e*v)),'A':float(n*(m[4]-3)/4),'Q':float(n*(m[4]/4-m[6]/6+m[3]**2/2+(1-m[4])**2/4))}

def profile(x,w,grid):
    return [conditional(x,w,v) for v in grid]

def mp_refine(x,w,p):
    mp.mp.dps=75
    xx=list(map(lambda z:mp.mpf(str(z)),x));ww=list(map(lambda z:mp.mpf(str(z)),w));n=sum(ww)
    def fun(mu,t,nu):
        s=mp.exp(t)
        return n*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-t)-(nu+1)/2*sum(wi*mp.log1p(((xi-mu)/s)**2/nu) for xi,wi in zip(xx,ww))
    fscore=lambda mu,t,nu:tuple(mp.diff(lambda a:fun(a,t,nu),mu) if j==0 else mp.diff(lambda a:fun(mu,a,nu),t) if j==1 else mp.diff(lambda a:fun(mu,t,a),nu) for j in range(3))
    r=mp.findroot(fscore,(p['mu'],mp.log(p['scale']),p['nu']),tol=mp.mpf('1e-60'),maxsteps=60)
    H=mp.matrix(3);rs=list(r)
    for i in range(3):
        for j in range(3):
            order=[0,0,0];order[i]+=1;order[j]+=1
            H[i,j]=mp.diff(fun,tuple(rs),tuple(order))
    eig=mp.eigsy(-H,eigvals_only=True)
    return {'mu':mp.nstr(r[0],55),'scale':mp.nstr(mp.exp(r[1]),55),'nu':mp.nstr(r[2],55),'ll':mp.nstr(fun(*r),55),'max_abs_score':mp.nstr(max(map(abs,fscore(*r))),8),'negative_hessian_eigenvalues':[mp.nstr(v,15) for v in eig]}

def main():
    examples={
      'negative-seam-finite-witness':([0,1,10],[9,6,5]),
      'std-negative-seam':([-1,0,1],[7,26,7]),
      'zero-slope-positive-curvature':([-1,0,1],[1,4,1]),
      'balanced-two-point':([-1,1],[1,1]),
      'half-tie-spread':([0,1,100],[2,1,1]),
      'std-equality':([-1,0,1],[1,4,1]),
      'zero-slope-negative-curvature':([-2,-1,0,1,2],[1,2,6,2,1]),
      'two-stable-ends':([-1,0,1],[51,98,51]),
      't2-stable-but-dominated':([-1,0,1],[1,3,1]),
      'interior-standard-t':([-3,-1,0,1,3],[1,4,6,4,1]),
    }
    result={'environment':{'python':sys.version,'numpy':np.__version__,'scipy':scipy.__version__,'mpmath':mp.__version__,'platform':platform.platform()},'examples':{}}
    for name,(xx,ww) in examples.items():
        x=np.array(xx,dtype=float);w=np.array(ww,dtype=float);m=moments(x,w)
        nus=[1,1.1,1.5,2,2.01,3,5,10,30,100,1000]
        ps=[]
        for nu in nus:
            if max(w)>=nu*(sum(w)-max(w)):continue
            p=conditional(x,w,nu);p['normal_gap']=p['ll']-m['normal'];ps.append(p)
        result['examples'][name]={'x':xx,'weights':ww,'moments':m,'profiles':ps}
    (OUT/'checks.json').write_text(json.dumps(result,indent=2)+'\n')
    print(json.dumps(result,indent=2))

if __name__=='__main__':main()
