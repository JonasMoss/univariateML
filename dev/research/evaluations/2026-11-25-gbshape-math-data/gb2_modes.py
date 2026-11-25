"""Propose two GB2 power maxima and compare bracket endpoints by ratio covers."""
import json
import math
from pathlib import Path

import mpmath as mp
import numpy as np

from gamma_cover import standardized
from gb2_cover import height, minimized_loss, value_bound
from genbeta_modes import psi_pair

HERE=Path(__file__).resolve().parent


def stirling(x):
    if np.size(x)==0:
        return x
    out=np.empty_like(x)
    small=x<10
    x0=x[small]
    out[small]=np.fromiter(map(math.lgamma,x0),dtype=float,count=x0.size)
    out[small]-=(x0-.5)*np.log(x0)-x0+.5*np.log(2*np.pi)
    u=1/x[~small]
    t=u*u
    out[~small]=u*(1/12-t*(1/360-t*(1/1260-t/1680)))
    return out


def scan(y, power, ratio):
    a=power[None,:,None,None]
    r=ratio[None,None,:,None]
    sample=y[:,None,None,:]
    l=np.broadcast_to(np.min(y,axis=1)[:,None,None,None],(len(y),len(power),len(ratio),1)).copy()
    u=np.broadcast_to(np.max(y,axis=1)[:,None,None,None],l.shape).copy()
    for _ in range(31):
        m=(l+u)/2
        z=a*(sample-m)
        e=np.exp(-np.abs(z))
        d=np.sign(z)*(-np.expm1(-np.abs(z)))/np.where(z>=0,r+(1-r)*e,1-r+r*e)
        high=np.mean(d,axis=-1,keepdims=True)>0
        l=np.where(high,m,l)
        u=np.where(high,u,m)
    z=a*(sample-(l+u)/2)
    middle=(r>0)&(r<1)
    rm=np.where(middle,r,.5)
    data=np.where(middle,(np.logaddexp(np.log1p(-rm),np.log(rm)+z)-rm*z)/(rm*(1-rm)),
                  np.where(r==0,np.expm1(z)-z,np.expm1(-z)+z)).mean(axis=-1)
    rm=rm[...,0]
    edge=~middle[...,0]
    lo=.4/data
    hi=1/data
    for _ in range(20):
        k=(lo+hi)/2
        p,q=k/(1-rm),k/rm
        own=lambda x:x*(np.log(x)-psi_pair(x)[0])
        deriv=np.where(edge,own(k),own(p)+own(q)-own(p+q))/k
        increase=deriv>data
        lo=np.where(increase,k,lo)
        hi=np.where(increase,hi,k)
    k=(lo+hi)/2
    p,q=k/(1-rm),k/rm
    C=.5*np.log(k/(2*np.pi))-np.where(edge,stirling(k),stirling(p)+stirling(q)-stirling(p+q))
    ll=np.log(power)[None,:,None]+C-k*data
    return ll.max(axis=-1),ll.argmax(axis=-1)


def certify(logs,a,index,ratio):
    y,_=standardized(logs)
    values=[]
    aa=list(map(mp.mpf,a))
    for k,i in [(1,index[0]),(3,index[1])]:
        r=mp.mpf(ratio[i])
        _,d=minimized_loss(y,aa[k],r,r)
        lo,hi=mp.mpf(".4")/d,1/d
        for _ in range(75):
            mid=(lo+hi)/2
            _,g=height(mid,r)
            if g>d:
                lo=mid
            else:
                hi=mid
        kshape=(lo+hi)/2
        values.append(mp.log(aa[k])+height(kshape,r)[0]-kshape*d-mp.mpf("1e-24"))
    inspected=[]
    arr=np.array([float(u) for u in y])
    for power,target in [(aa[0],values[0]),(aa[2],min(values)),(aa[4],values[1])]:
        queue=[(mp.mpf(0),mp.mpf(1))]
        cells=0
        while queue:
            lo,hi=queue.pop()
            cells+=1
            if cells>256:
                return None
            if value_bound(y,power,power,lo,hi,target-mp.mpf("1e-11"),arr)<target:
                continue
            mid=(lo+hi)/2
            queue.extend(((lo,mid),(mid,hi)))
        inspected.append(cells)
    return dict(powers=aa,lower=values,endpoint_cells=inspected)


def run():
    mp.mp.dps=60
    rg=np.random.default_rng(2612)
    a=np.geomspace(.075,60,52)
    ratio=np.array([0,.0001,.001,.004,.012,.028,.055,.10,.18,.28,.40,.50,
                    .60,.72,.82,.90,.945,.972,.988,.996,.999,.9999,1])
    found=[]
    proposals=0
    for b in range(24):
        groups=rg.uniform(-3,3,size=(30,2))
        labels=rg.random((30,9))>.5
        noise=rg.choice([.05,.2,.6,1.4],size=(30,1))*rg.normal(size=(30,9))
        atoms=np.rint((np.where(labels,groups[:,0,None],groups[:,1,None])+noise)*10000).astype(np.int64)
        z=(atoms-atoms.mean(axis=1)[:,None])/atoms.std(axis=1)[:,None]
        with np.errstate(divide="ignore",invalid="ignore",over="ignore"):
            ll,ii=scan(z,a,ratio)
        for sample,row in enumerate(ll):
            highs=np.flatnonzero((row[1:-1]>row[:-2])&(row[1:-1]>row[2:]))+1
            for l,r in zip(highs,highs[1:]):
                m=l+np.argmin(row[l:r+1])
                j=np.argmin(row[:l])
                n=r+1+np.argmin(row[r+1:])
                if (min(row[l],row[r])-row[m]<.0005
                        or row[l]-row[j]<.0005 or row[r]-row[n]<.0005):
                    continue
                proposals+=1
                candidate=certify([mp.mpf(x) for x in atoms[sample].tolist()],
                                  a[[j,l,m,r,n]],(ii[sample,l],ii[sample,r]),ratio)
                if candidate:
                    found.append(dict(log_observations=atoms[sample].tolist(),**candidate))
                    break
            if found:
                break
        if found:
            break
    (HERE/"gb2-modes-results.json").write_text(json.dumps(
        dict(precision=60,seed=2612,samples=(b+1)*30,power_grid=[float(a[0]),float(a[-1]),len(a)],
             ratio_grid=ratio.tolist(),proposals=proposals,confirmed=found),indent=2,
        default=lambda x:mp.nstr(x,28))+"\n")
    print("GB2 two-finite-mode cases confirmed:",len(found))


if __name__=="__main__":
    run()
