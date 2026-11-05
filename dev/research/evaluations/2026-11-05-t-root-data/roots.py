"""Guarded research root boxes; no production dispatch or accepted estimates.

All interval operations enlarge by 64 eps * (1 + magnitude). Sums have
an n-dependent allowance. Guarantees are conditional on that practical
arithmetic model, not outward-rounded IEEE interval arithmetic.
"""
import json
import sys
import time
from pathlib import Path
import numpy as np
import mpmath as mp
from scipy.optimize import root
from scipy.special import digamma, polygamma

HERE = Path(__file__).resolve().parent
OLD = HERE.parent / '2026-11-04-t-cert-data'
sys.path.insert(0, str(OLD))
import study

mp.mp.dps = 70
EPS = 64 * np.finfo(float).eps

class I:
    def __init__(self, lo, hi=None):
        self.lo = np.asarray(lo, dtype=float)
        self.hi = np.asarray(lo if hi is None else hi, dtype=float)
        if np.any(self.lo > self.hi) or not np.all(np.isfinite(self.lo + self.hi)):
            raise ArithmeticError('invalid interval')
    @staticmethod
    def pad(lo, hi):
        e = EPS * (1 + np.maximum(abs(lo), abs(hi)))
        return I(lo-e, hi+e)
    def __add__(a,b):
        b=iv(b); return I.pad(a.lo+b.lo,a.hi+b.hi)
    __radd__=__add__
    def __neg__(a): return I(-a.hi,-a.lo)
    def __sub__(a,b): return a+-iv(b)
    def __rsub__(a,b): return iv(b)+-a
    def __mul__(a,b):
        b=iv(b); p=np.array([a.lo*b.lo,a.lo*b.hi,a.hi*b.lo,a.hi*b.hi])
        return I.pad(p.min(axis=0),p.max(axis=0))
    __rmul__=__mul__
    def __truediv__(a,b):
        b=iv(b)
        if np.any((b.lo<=0)&(b.hi>=0)): raise ArithmeticError('zero denominator')
        return a*I.pad(1/b.hi,1/b.lo)
    def __rtruediv__(a,b):return iv(b)/a
    def sq(a):
        lo=np.where((a.lo<=0)&(a.hi>=0),0,np.minimum(a.lo*a.lo,a.hi*a.hi))
        return I.pad(lo,np.maximum(a.lo*a.lo,a.hi*a.hi))
    def exp(a):return I.pad(np.exp(a.lo),np.exp(a.hi))
    def log(a):
        if np.any(a.lo<=0):raise ArithmeticError('log domain')
        return I.pad(np.log(a.lo),np.log(a.hi))
    def sum(a):
        e=EPS*(a.lo.size+1)*(1+np.maximum(abs(a.lo),abs(a.hi)).sum())
        return I(a.lo.sum()-e,a.hi.sum()+e)
    def mag(a):return np.maximum(abs(a.lo),abs(a.hi))
    def pair(a):return [float(a.lo),float(a.hi)]
    def __len__(a):return a.lo.size

def iv(a):return a if isinstance(a,I) else I(a)

def c_derivative(v, order):
    # Laplace integral shows c' positive decreasing and -c'' positive
    # decreasing. Evaluate endpoint differences at 70 digits, then pad.
    def at(q):
        q=mp.mpf(float(q))
        if order==1:return (mp.digamma((q+1)/2)-mp.digamma(q/2))/2
        return (mp.polygamma(1,q/2)-mp.polygamma(1,(q+1)/2))/4
    return I.pad(float(at(v.hi)),float(at(v.lo)))

def equations(x,p,v0):
    if isinstance(x,I):x=(x.lo+x.hi)/2
    m,t,w=p; v=v0*w; a=np.exp(t); r=x-m; D=r*r+a*a;n=len(x)
    g=np.array([-(v+1)*np.sum(r/D),n-(v+1)*np.sum(r*r/D),
                v0*(-n*.5*(digamma((v+1)/2)-digamma(v/2))-n*t+.5*np.log(D).sum())])
    h=np.array([[(v+1)*np.sum((a*a-r*r)/D**2),2*(v+1)*np.sum(r*a*a/D**2),-v0*np.sum(r/D)],
                [2*(v+1)*np.sum(r*a*a/D**2),2*(v+1)*np.sum(a*a*r*r/D**2),-v0*np.sum(r*r/D)],
                [-v0*np.sum(r/D),-v0*np.sum(r*r/D),n*v0*v0*.25*(polygamma(1,v/2)-polygamma(1,(v+1)/2))]])
    return g,h

def enclosure(x,lo,hi,v0,gradient=False):
    m,t,w=[I(l,h) for l,h in zip(lo,hi)];v=w*v0
    a2=(2*t).exp();r=iv(x)-m;r2=r.sq();D=r2+a2;D2=D.sq();n=len(x)
    h00=(v+1)*((a2-r2)/D2).sum()
    h01=2*(v+1)*(r*a2/D2).sum()
    h02=-v0*(r/D).sum()
    h11=2*(v+1)*(a2*r2/D2).sum()
    h12=-v0*(r2/D).sum()
    h22=n*v0*v0*c_derivative(v,2)
    hs=[[h00,h01,h02],[h01,h11,h12],[h02,h12,h22]]
    low=np.array([[h.lo for h in row] for row in hs]);high=np.array([[h.hi for h in row] for row in hs])
    if not gradient:return low,high
    gs=[-(v+1)*(r/D).sum(),n-(v+1)*(r2/D).sum(),
        v0*(-n*c_derivative(v,1)-n*t+.5*D.log().sum())]
    return low,high,np.array([g.lo for g in gs]),np.array([g.hi for g in gs])

def transform(C,lo,hi):
    mid=(lo+hi)/2;rad=(hi-lo)/2
    centre=C@mid; error=abs(C)@rad+EPS*(lo.shape[0]+1)*(1+abs(C)@np.maximum(abs(lo),abs(hi)))
    return centre-error,centre+error

def positivity(Hlo,Hhi,H0):
    # Congruence to unit curvature; Frobenius dominates spectral norm.
    C=np.linalg.inv(np.linalg.cholesky(H0))
    lo,hi=transform(C,Hlo,Hhi)
    lo,hi=transform(C,lo.T,hi.T);lo=lo.T;hi=hi.T
    error=np.linalg.norm(np.maximum(abs(lo-np.eye(len(C))),abs(hi-np.eye(len(C)))),ord='fro')
    return 1-error-EPS*(lo.size+1)*(1+np.linalg.norm(np.maximum(abs(lo),abs(hi)),ord='fro'))

def root_box(x,p,v0,point_gradient=None,diagnostics=None):
    g,H=equations(x,p,v0);C=np.linalg.inv(H)
    if point_gradient is None:
        _,_,gl,gh=enclosure(x,p,p,v0,True)
    else:
        # Exact original binary64 input and affine constants, evaluated at
        # 70 digits. The Hessian still uses the enlarged binary64 intervals.
        gg=I.pad(np.array(list(map(float,point_gradient))),np.array(list(map(float,point_gradient))))
        gl,gh=gg.lo,gg.hi
    el,eh=transform(C,gl,gh);e=np.maximum(abs(el),abs(eh))
    if diagnostics is not None:
        diagnostics.update(point_score_lower=gl.tolist(),point_score_upper=gh.tolist(),preconditioned_error=e.tolist(),trials=[])
    # Widen until the guarded point residual fits; never accept a cap.
    for width in [1e-10,1e-9,1e-8,1e-7,1e-6]:
        r=width*np.maximum(1,abs(p));lo=p-r;hi=p+r
        hl,hh=enclosure(x,lo,hi,v0)
        cl,ch=transform(C,hl,hh)
        Q=np.maximum(abs(np.eye(3)-cl),abs(np.eye(3)-ch))
        ratio=(e+Q@r)/r
        if diagnostics is not None:diagnostics['trials'].append({'width':width,'image_ratio':ratio.tolist(),'contraction_infinity':float(np.linalg.norm(Q,ord=np.inf))})
        if max(ratio)<1 and np.linalg.norm(Q,ord=np.inf)<1:
            return {'lo':lo.tolist(),'hi':hi.tolist(),'radii':r.tolist(),
                    'image_ratio':ratio.tolist(),'contraction_infinity':float(np.linalg.norm(Q,ord=np.inf)),
                    'positive_congruence_margin':float(positivity(hl,hh,H)) if np.linalg.eigvalsh(H)[0]>0 else None}
    return None

def tube(x,p,v0,strict_floor=1.):
    _,H0=equations(x,p,v0)
    # Full strict convexity in a product region and radial outward nuisance
    # gradients identify all fixed-shape optima uniformly inside that region.
    for h in [.01,.003,.001,.0003,.0001,.00003,.00001,.000003,.000001,.0000003,.0000001,.00000003,.00000001,.000000003,.000000001]:
        for r in [h,3*h,10*h,30*h]:
            widths=np.array([r,r,h]);lo=p-widths;hi=p+widths
            if v0*lo[2]<=strict_floor:continue
            hl,hh=enclosure(x,lo,hi,v0)
            margin=positivity(hl,hh,H0)
            if margin<=0:continue
            mid=(hl[:2,:2]+hh[:2,:2])/2;rad=(hh[:2,:2]-hl[:2,:2])/2
            lam=np.linalg.eigvalsh(mid)[0]-np.linalg.norm(rad,ord='fro')-EPS*(1+np.linalg.norm(mid))
            if lam<=0:continue
            cl=p.copy();ch=p.copy();cl[2]-=h;ch[2]+=h
            _,_,gl,gh=enclosure(x,cl,ch,v0,True)
            G=np.linalg.norm(np.maximum(abs(gl[:2]),abs(gh[:2])))
            if G>=lam*r:continue
            # Congruence lower bound also gives a full Hessian eigenvalue
            # bound via the Frobenius norm of its inverse Cholesky factor.
            C=np.linalg.inv(np.linalg.cholesky(H0))
            eigen_lower=margin/np.linalg.norm(C,ord='fro')**2
            return {'lo':lo.tolist(),'hi':hi.tolist(),'shape_interval':[float(np.nextafter(v0*lo[2],np.inf)),float(np.nextafter(v0*hi[2],-np.inf))],
                    'positive_congruence_margin':float(margin),'hessian_eigen_lower':float(eigen_lower),
                    'profile_second_upper':float(-eigen_lower/v0**2),
                    'nuisance_eigen_lower':float(lam),'nuisance_score_upper':float(G),
                    'inward_margin':float(lam*r-G)}
    return None

def isolate(x,nu,mu,scale):
    # Normalize for coordinates only. Original input identity is retained.
    center=float(np.mean(x));unit=float(np.std(x));y=(x-center)/unit
    p=np.array([(mu-center)/unit,np.log(scale/unit*np.sqrt(nu)),1.])
    ans=root(lambda p:equations(y,p,nu)[0],p,jac=lambda p:equations(y,p,nu)[1],tol=1e-11)
    p=ans.x
    if not np.all(np.isfinite(p)) or p[2]*nu<1:return {'outcome':'proposal_failure'}
    xx=[(mp.mpf(q)-mp.mpf(center))/mp.mpf(unit) for q in x];vv0=mp.mpf(nu);n=len(xx)
    def exact_score(m,t,w):
        v=vv0*w;a2=mp.exp(2*t);rs=[q-m for q in xx];ds=[q*q+a2 for q in rs]
        return (-mp.fsum((v+1)*r/d for r,d in zip(rs,ds)),
                n-mp.fsum((v+1)*r*r/d for r,d in zip(rs,ds)),
                vv0*(-n*(mp.digamma((v+1)/2)-mp.digamma(v/2))/2-n*t+mp.fsum(mp.log(d) for d in ds)/2))
    try:
        refined=mp.findroot(exact_score,tuple(map(mp.mpf,p)),tol=mp.mpf('1e-55'),maxsteps=50)
        if all(mp.im(q)==0 for q in refined) and refined[2]*vv0>=1:
            p=np.array(list(map(float,refined)))
    except (ValueError,ZeroDivisionError):pass
    point_gradient=exact_score(*map(mp.mpf,p))
    eig=np.linalg.eigvalsh(equations(y,p,nu)[1])
    # center and unit are exact binary64 affine constants. Enclose the
    # transformation of the exact original binary64 observations.
    yi=(I(x)-center)/unit
    diagnostics={}
    box=root_box(yi,p,nu,point_gradient,diagnostics)
    if not box:return {'outcome':'root_box_failure','eigenvalues':eig.tolist(),'point':p.tolist(),'v0':nu,'diagnostics':diagnostics}
    _,counts=np.unique(x,return_counts=True)
    strict_mass=max(counts)<nu*(p[2]-1e-6)*(len(x)-max(counts))
    neighbourhood=tube(yi,p,nu,max(1.,float(max(counts)/(len(x)-max(counts))))) if eig[0]>0 and strict_mass else None
    if neighbourhood and not (np.all(np.array(box['lo'])>np.array(neighbourhood['lo'])) and np.all(np.array(box['hi'])<np.array(neighbourhood['hi']))):
        neighbourhood=None
    lo=np.array(box['lo']);hi=np.array(box['hi']);vv=I(lo[2],hi[2])*nu
    mm=I(lo[0],hi[0])*unit+center
    ss=I(lo[1],hi[1]).exp()*unit/(.5*vv.log()).exp()
    public={'mu':mm.pair(),'scale':ss.pair(),'nu':vv.pair()}
    if vv.lo>2:public['sd']=(ss/(1-2/vv).log().__mul__(.5).exp()).pair()
    return {'outcome':'root_and_tube' if neighbourhood else 'root_only',
            'center':center,'unit':unit,'v0':nu,'point':p.tolist(),'box':box,
            'tube':neighbourhood,'coefficients':public,'eigenvalues':eig.tolist()}

def main():
    cases=json.loads((OLD/'inputs.json').read_text());previous=json.loads((OLD/'results.json').read_text())
    rows=[]
    for case,old in zip(cases,previous):
        for family in ['lst','std']:
            q=old[family].get('proposal')
            if not q or old[family]['outcome']!='value_enclosed':continue
            start=time.perf_counter()
            try:
                floor=1 if family=='lst' else 2
                if q['nu']==floor:
                    result={'outcome':'endpoint_nomination'}
                else:result=isolate(np.array(case['x']),q['nu'],q['mu'],q['scale'])
                if result.get('box') and result['coefficients']['nu'][0]<=floor:
                    result={'outcome':'constrained_endpoint_proposal'}
            except (ArithmeticError,ValueError,np.linalg.LinAlgError) as error:
                result={'outcome':'numerical_failure','reason':str(error)}
            rows.append({'name':case['name'],'family':family,'seconds':time.perf_counter()-start,**result})
            print(case['name'],family,result['outcome'],flush=True)
    (HERE/'roots.json').write_text(json.dumps(rows,indent=2,allow_nan=False)+'\n')

if __name__=='__main__':main()
