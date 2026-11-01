"""Seeded multistart comparison; finite numerical evidence, not a certificate.

Compile oracle_driver.cpp to build/scratch/skewt/oracle_driver, then run this
script from the repository root under the four-slot runner. Every case is
flushed to a JSONL file as it completes. Set UMVL_ORACLE_LIMIT for smoke tests.
"""
from concurrent.futures import ProcessPoolExecutor
import json
import os
import subprocess
import numpy as np
from scipy.optimize import minimize
from scipy.special import stdtr, gammaln


def cases():
    rng=np.random.default_rng(20261103)
    out=[]
    for a in (-5,-2,0,2,5):
        for nu in (2,5,15):
            for n in (30,100):
                for replicate in range(10):
                    d=a/np.hypot(1,a)
                    z=d*np.abs(rng.standard_normal(n))+rng.standard_normal(n)/np.hypot(1,a)
                    out.append((a,nu,n,replicate,z/np.sqrt(rng.chisquare(nu,n)/nu)))
    return out


def solve(case):
    a,nu,n,replicate,x=case
    text=str(n)+' '+' '.join('%.17g'%v for v in x)+'\n'
    proc=subprocess.run(['build/scratch/skewt/oracle_driver'],input=text,text=True,capture_output=True,check=True)
    status,actual,_,_,_,_,reason=proc.stdout.split();status=int(status);actual=float(actual);reason=int(reason)
    center=np.median(x);unit=np.std(x);y=(x-center)/unit
    def cost(theta,edge=False):
        loc,ls,shape=theta[:3]
        df=1 if edge else 1+np.exp(np.clip(theta[3],-30,20))
        scale=np.exp(np.clip(ls,-30,30));z=(y-loc)/scale
        h=shape*z*np.sqrt((df+1)/(df+z*z));c=stdtr(df+1,h)
        if np.any(c<=0):return 1e100
        val=np.log(2)-ls+gammaln((df+1)/2)-gammaln(df/2)-.5*np.log(df*np.pi)-(df+1)/2*np.log1p(z*z/df)+np.log(c)
        return -np.sum(val)
    best=np.inf
    for shape in (-5,-1,0,1,5):
        for df in (1.5,5,30):
            r=minimize(cost,[0,0,shape,np.log(df-1)],method='BFGS',options={'maxiter':500,'gtol':1e-6})
            best=min(best,r.fun)
        r=minimize(lambda t:cost(t,True),[0,0,shape],method='BFGS',options={'maxiter':500,'gtol':1e-6})
        best=min(best,r.fun)
    oracle=-best-n*np.log(unit)
    return dict(alpha=a,nu=nu,n=n,replicate=replicate,status=status,reason=reason,
                actual=actual if np.isfinite(actual) else None,oracle=oracle,
                loss=oracle-actual if np.isfinite(actual) else None)


def main():
    data=cases();limit=int(os.environ.get('UMVL_ORACLE_LIMIT',len(data)))
    output=os.environ.get('UMVL_ORACLE_OUTPUT','dev/research/evaluations/2026-11-01-skewt-data/oracle-results.json')
    results=[]
    with open(output+'.jsonl','w') as stream, ProcessPoolExecutor(max_workers=4) as pool:
        for result in pool.map(solve,data[:limit],chunksize=1):
            results.append(result);stream.write(json.dumps(result,allow_nan=False)+'\n');stream.flush()
            if len(results)%10==0:print('completed',len(results),'of',limit,flush=True)
    with open(output,'w') as out:json.dump(results,out,indent=2,allow_nan=False)
    print('cases',len(results),'statuses',{str(k):sum(t['status']==k for t in results) for k in sorted(set(t['status'] for t in results))})
    losses=[t['loss'] for t in results if t['loss'] is not None]
    print('worst loss',max(losses) if losses else None)


if __name__=='__main__':main()
