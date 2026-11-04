"""Research box certificates, conditional on the stated practical libm allowance.

No fitter or certified estimator is exported. Read previous evidence only.
"""
import csv
import importlib.util
import json
from pathlib import Path
import time
import numpy as np
from scipy.special import expit
from scipy.optimize import root

HERE = Path(__file__).resolve().parent
PREV = HERE.parent/'2026-11-03-burr-xi-data'
spec = importlib.util.spec_from_file_location('xi', PREV/'study.py')
xi = importlib.util.module_from_spec(spec)
spec.loader.exec_module(xi)
EPS = np.finfo(float).eps
PAD = 64*EPS

class I:
    """Vector intervals with deliberately conservative practical padding.

    The mathematical claims are conditional on containing the real results.
    This is not an outward-rounding or general libm error proof.
    """
    def __init__(self, lo, hi=None):
        if isinstance(lo,I):
            self.lo,self.hi=lo.lo,lo.hi
        else:
            self.lo=np.asarray(lo,dtype=float)
            self.hi=np.asarray(lo if hi is None else hi,dtype=float)
        assert np.all(self.lo <= self.hi)
    @staticmethod
    def padded(lo,hi, factor=1):
        allowance=PAD*factor*(1+np.maximum(abs(lo),abs(hi)))
        return I(lo-allowance,hi+allowance)
    def __add__(self,o):
        o=I(o);return I.padded(self.lo+o.lo,self.hi+o.hi)
    __radd__=__add__
    def __neg__(self):return I(-self.hi,-self.lo)
    def __sub__(self,o):return self+-I(o)
    def __rsub__(self,o):return I(o)+-self
    def __mul__(self,o):
        o=I(o);v=np.array([self.lo*o.lo,self.lo*o.hi,self.hi*o.lo,self.hi*o.hi])
        return I.padded(v.min(axis=0),v.max(axis=0))
    __rmul__=__mul__
    def __truediv__(self,o):
        o=I(o);assert np.all(o.lo*o.hi>0)
        return self*I.padded(1/o.hi,1/o.lo)
    def __rtruediv__(self,o):return I(o)/self
    def sum(self):
        # Account for cancellation by using sum of input magnitudes.
        allowance=PAD*(self.lo.size+1)*(1+np.sum(np.maximum(abs(self.lo),abs(self.hi))))
        return I(np.sum(self.lo)-allowance,np.sum(self.hi)+allowance)
    def exp(self):return I.padded(np.exp(self.lo),np.exp(self.hi))
    def log(self):
        assert np.all(self.lo>0)
        return I.padded(np.log(self.lo),np.log(self.hi))
    def sp(self):return I.padded(np.logaddexp(0,self.lo),np.logaddexp(0,self.hi))
    def sigmoid(self):return I.padded(expit(self.lo),expit(self.hi))
    def mag(self):return np.maximum(abs(self.lo),abs(self.hi))
    def pair(self):return [np.asarray(self.lo).tolist(),np.asarray(self.hi).tolist()]


def interval_data(x):
    # Keep original log coordinates; no unchecked standardization transform.
    return I.padded(np.log(x),np.log(x))


def score_hessian(y,t):
    k,c,b=map(I,t);n=y.lo.size
    z=c*y+b;p=z.sigmoid();w=p*(1-p)
    g=[n/k-z.sp().sum(), n/c+y.sum()-(k+1)*(y*p).sum(),n-(k+1)*p.sum()]
    h00=-n/(k*k);h01=-(y*p).sum();h02=-p.sum()
    h11=-n/(c*c)-(k+1)*(y*y*w).sum()
    h12=-(k+1)*(y*w).sum();h22=-(k+1)*w.sum()
    return g,[[h00,h01,h02],[h01,h11,h12],[h02,h12,h22]]


def numeric(y,t):
    k,c,b=t;n=len(y);z=c*y+b;p=expit(z);w=p*(1-p)
    g=np.array([n/k-np.logaddexp(0,z).sum(), n/c+y.sum()-(k+1)*np.dot(y,p),n-(k+1)*p.sum()])
    h=np.array([[-n/k**2,-np.dot(y,p),-p.sum()],[-np.dot(y,p),-n/c**2-(k+1)*np.dot(y*y,w),-(k+1)*np.dot(y,w)],[-p.sum(),-(k+1)*np.dot(y,w),-(k+1)*w.sum()]])
    return g,h


def mat_interval(a,b):
    return [[sum((float(a[i,j])*b[j][k] for j in range(len(b))),I(0))
             for k in range(len(b[0]))] for i in range(len(a))]


def contraction(y,t,r):
    """Brouwer + Banach certificate for the complete three-score system."""
    _,h=numeric((y.lo+y.hi)/2,t)
    R=-np.linalg.inv(h)
    g,_=score_hessian(y,t)
    _,hb=score_hessian(y,[I(a-v,a+v) for a,v in zip(t,r)])
    eg=[sum((R[i,j]*g[j] for j in range(3)),I(0)) for i in range(3)]
    rh=mat_interval(R,hb)
    E=[[rh[i][j]+int(i==j) for j in range(3)] for i in range(3)]
    q=np.array([[float(e.mag()) for e in row] for row in E])
    ratio=(np.array([float(a.mag()) for a in eg])+q@r)/r
    return dict(max_ratio=float(max(ratio)),q=float(max((q@r)/r)),
                score_step=[a.pair() for a in eg],R=R.tolist())


def conditional_slab(y,t,delta):
    """Certify nuisance containment uniformly for all k in the slab.

    Rectangle contraction of nuisance Newton map, uniform in k. Full
    Hessian negativity is checked by a congruence, not profile concavity
    inferred from fixed-k concavity.
    """
    _,h=numeric((y.lo+y.hi)/2,t)
    R=-np.linalg.inv(h[1:,1:])
    derivative=-np.linalg.solve(h[1:,1:],h[1:,0])
    r=2*(abs(R)@abs(h[1:,0]))*delta+delta*1e-3*np.array([t[1],max(1,abs(t[2]))])
    kb=I(t[0]-delta,t[0]+delta)
    for attempt in range(6):
        if r[0]>=t[1]:return None
        g,_=score_hessian(y,[kb,t[1],t[2]])
        _,hb=score_hessian(y,[kb,I(t[1]-r[0],t[1]+r[0]),I(t[2]-r[1],t[2]+r[1])])
        eg=[sum((R[i,j]*g[j+1] for j in range(2)),I(0)) for i in range(2)]
        rh=mat_interval(R,[row[1:] for row in hb[1:]])
        E=[[rh[i][j]+int(i==j) for j in range(2)] for i in range(2)]
        q=np.array([[float(e.mag()) for e in row] for row in E])
        ratio=(np.array([float(a.mag()) for a in eg])+q@r)/r
        if max(ratio)<.9:break
        r*=1.6
    # Congruence C^T (-H) C near identity, with C=Cholesky(-H0)^(-T).
    try:C=np.linalg.inv(np.linalg.cholesky(-h)).T
    except np.linalg.LinAlgError:return None
    hc=mat_interval(-C.T,hb)
    D=[[sum((hc[i][j]*float(C[j,k]) for j in range(3)),I(0))-int(i==k)
        for k in range(3)] for i in range(3)]
    rho=max(float(I.padded(sum(float(e.mag()) for e in row),sum(float(e.mag()) for e in row),3).hi) for row in D)
    if max(ratio)>=.9 or rho>=.9:return None
    return dict(delta=float(delta),nuisance_radii=r.tolist(),containment_ratio=float(max(ratio)),
                curvature_congruence_radius=float(rho),
                min_information_eigenvalue=float(((1-I(rho))/(I(C)*I(C)).sum()).lo))


def main():
    samples=json.loads((PREV/'samples.json').read_text())
    seeds=list(csv.DictReader((PREV/'roots.csv').open()))
    rows=[];start=time.perf_counter()
    for row in seeds:
        x=np.array(samples[row['sample']]);y=interval_data(x)
        t=np.array([float(row['k']),float(row['c']),-float(row['c'])*np.log(float(row['scale']))])
        fit=root(lambda z:numeric(np.log(x),z)[0],t,jac=lambda z:numeric(np.log(x),z)[1],tol=1e-11)
        t=fit.x
        r=1e-8*np.array([t[0],t[1],max(1,abs(t[2]))])
        local=contraction(y,t,r)
        assert local['max_ratio']<.9,(row['sample'],local)
        log_s=-I(t[2]-r[2],t[2]+r[2])/I(t[1]-r[1],t[1]+r[1])
        scale=log_s.exp()
        bounds=[I(t[0]-r[0],t[0]+r[0]).pair(),I(t[1]-r[1],t[1]+r[1]).pair(),scale.pair()]
        slab=None
        if row['type']=='maximum':
            for fraction in [.03,.01,.003,.001,.0003,.0001,.00003,.00001,.000003,.000001]:
                slab=conditional_slab(y,t,t[0]*fraction)
                if slab:break
        else:
            assert np.linalg.eigvalsh(numeric(np.log(x),t)[1])[-1]>0
        if row['type']=='maximum':assert slab is not None,row['sample']
        item=dict(sample=row['sample'],type=row['type'],n=len(x),center=t.tolist(),radii=r.tolist(),
                  coefficient_bounds=bounds,local=local,slab=slab)
        rows.append(item)
        print(row['sample'],row['type'],'root',local['max_ratio'],'slab',None if slab is None else slab['delta']/t[0],flush=True)
    summary=dict(roots=len(rows),maxima=sum(r['type']=='maximum' for r in rows),
                 seconds=time.perf_counter()-start,allowance=PAD,
                 source_revision=__import__('subprocess').check_output(['git','rev-parse','HEAD'],text=True).strip(),
                 certificates=rows)
    (HERE/'isolation.json').write_text(json.dumps(summary,indent=2)+'\n')

if __name__=='__main__':main()
