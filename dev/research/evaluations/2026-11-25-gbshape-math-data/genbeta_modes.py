"""Numerical proposal scan for two finite genbeta power maxima."""
import json
from pathlib import Path

import mpmath as mp
import numpy as np

from genbeta_cover import info, mean, setup

HERE=Path(__file__).resolve().parent


def psi_pair(x):
    x=x.copy()
    d=np.zeros_like(x)
    t=np.zeros_like(x)
    for _ in range(12):
        inv=np.where(x<12,1/x,0)
        d-=inv
        t+=inv**2
        x+=x<12
    w=1/x
    z=w*w
    d+=np.log(x)-w/2-z*(1/12-z*(1/120-z*(1/252-z/240)))
    t+=w+z/2+w*z*(1/6-z*(1/30-z*(1/42-z/30)))
    return d,t


def proposals(v, powers):
    h=powers[None,:,None]
    y=v[:,None,:]
    r=-np.mean(np.log(-np.expm1(-h*y)),axis=2)
    D=np.mean(y*np.exp(-h*y)/-np.expm1(-h*y),axis=2)
    h=powers[None,:]
    c=-np.mean(np.log(v),axis=1)
    q=np.broadcast_to((.6/c)[:,None],r.shape).copy()
    p=q/h
    for _ in range(90):
        d,t=psi_pair(p+q)
        dp,tp=psi_pair(p)
        dq,tq=psi_pair(q)
        g,j=d-dp-h,d-dq-r
        a,b=tp-t,tq-t
        det=a*b-t*t
        ok=det>0
        s=np.where(ok,(b*g+t*j)/(p*det),0)
        z=np.where(ok,(t*g+a*j)/(q*det),0)
        shrink=np.minimum(1,.4/np.maximum(1e-30,np.maximum(np.abs(s),np.abs(z))))
        p=np.clip(p*(1+s*shrink),1e-8,1e9)
        q=np.clip(q*(1+z*shrink),1e-8,1e9)
    score=1/h-p+(q-1)*D
    converged=(np.maximum(np.abs(g),np.abs(j))<2e-9)&(np.maximum(p,q)<1e7)
    return score,p,q,converged


def exact_brackets(candidates):
    """Check exact observations and rational power endpoints by intervals."""
    sample=candidates[1]["log_distances"]
    mp.iv.dps=55
    I=mp.iv.mpf
    v=[I(x)*len(sample)/sum(sample) for x in sample]
    _,_,_,_,_,point,_=setup(list(map(mp.mpf,sample)),mp.mpf(3),mp.mpf(1),mp.mpf(1))

    def psi(x):
        z=x+64
        return (mp.iv.ln(z)-1/(2*z)-1/(12*z*z)+1/(120*z**4)
                -I([0,1])/(252*z**6)-sum(1/(x+j) for j in range(64)))

    brackets=[("3.06994","3.29223"),("16.43531","17.62537")]
    results=[]
    for pair in brackets:
        row=[]
        for endpoint,positive in zip(pair,(True,False)):
            at=mp.mpf(endpoint)
            p,q,*_=point(at)
            A,B,T=info(p,q)
            delta=mp.mpf("1e-11")/(A*B-T*T)
            dp,dq=2*delta*(B+T),2*delta*(A+T)
            pL,pH,qL,qH=map(lambda x:I(mp.nstr(x,45)),(p-dp,p+dp,q-dq,q+dq))
            h=I(endpoint)
            decay=[mp.iv.exp(-h*x) for x in v]
            r=-sum(mp.iv.ln(1-z) for z in decay)/len(v)
            D=sum(x*z/(1-z) for x,z in zip(v,decay))/len(v)
            for pp,qq,sign in [(pL,qL,1),(pH,qH,-1)]:
                common=psi(pp+qq)
                assert sign*(common-psi(pp)-h)>0 and sign*(common-psi(qq)-r)>0
            lo,hi=1/h-pH+(qL-1)*D,1/h-pL+(qH-1)*D
            assert lo>0 if positive else hi<0
            row.append([str(lo.a),str(hi.b)])
        results.append(row)
    return dict(log_distances=sample,brackets=brackets,score_intervals=results)


def run():
    rng=np.random.default_rng(261)
    powers=np.geomspace(.035,25,95)
    count=0
    mp.mp.dps=65
    candidates=[]
    with np.errstate(divide="ignore",invalid="ignore",over="ignore"):
        for batch in range(20):
            # Exact proposals are positive integer log distances, up to common scale.
            centers=rng.uniform(-7,4,size=(75,2))
            near=rng.choice([.03,.15,.6,1.5],size=(75,1))
            data=np.exp(np.where(rng.random((75,8))<.45,centers[:,0,None],
                                 centers[:,1,None])+rng.normal(size=(75,8))*near)
            atoms=np.maximum(1,np.rint(data*100000)).astype(np.int64)
            v=atoms/np.mean(atoms,axis=1)[:,None]
            score,p,q,ok=proposals(v,powers)
            for k, row in enumerate(score):
                brackets=np.where(ok[k,:-1]&ok[k,1:]&(row[:-1]>1e-7)&(row[1:]<-1e-7))[0]
                if len(brackets)<2:
                    continue
                count+=1
                z=list(map(mp.mpf,atoms[k].tolist()))
                checked=[]
                for i in brackets:
                    a,b=map(mp.mpf,powers[i:i+2])
                    setup_args=(a,mp.mpf(p[k,i]),mp.mpf(q[k,i]))
                    *_,point,rectangle=setup(z,*setup_args)
                    intervals=[]
                    for at in (a,b):
                        box=rectangle(at,at)
                        if box is None:
                            break
                        pL,pH,qL,qH=box
                        vals=[x/mean(z) for x in z]
                        D=mean([x/mp.expm1(at*x) for x in vals])
                        intervals.append((1/at-pH+(qL-1)*D,1/at-pL+(qH-1)*D))
                    if len(intervals)==2 and intervals[0][0]>0>intervals[1][1]:
                        checked.append(dict(bracket=[a,b],score_bounds=intervals))
                if len(checked)>=2:
                    candidates.append(dict(log_distances=atoms[k].tolist(),roots=checked))
                    if len(candidates)>=2:
                        break
            if len(candidates)>=2:
                break
    (HERE/"genbeta-modes-results.json").write_text(json.dumps(
        dict(seed=261,samples=(batch+1)*75,grid=[float(powers[0]),float(powers[-1]),len(powers)],
             precision=65,nominated=count,confirmed=candidates,exact=exact_brackets(candidates)),indent=2,
        default=lambda x:mp.nstr(x,30))+"\n")
    print("genbeta two-finite-mode cases confirmed:",len(candidates))


if __name__=="__main__":
    run()
