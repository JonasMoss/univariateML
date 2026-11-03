#!/usr/bin/env python3
"""Sharper research envelope; face inputs remain numerical candidates.

Run from the root via /tmp/umvl-cpu/run python3 <this file>.
See derivation.md section 7 for proofs, including the normalization.
"""
import os
for key in ('OPENBLAS_NUM_THREADS','OMP_NUM_THREADS','MKL_NUM_THREADS'):
    os.environ[key] = '1'
import csv
import json
import math
from collections import Counter
from functools import lru_cache
from pathlib import Path
import random
import mpmath as mp
from scipy.special import gammaln

HERE = Path(__file__).resolve().parent
MAP = HERE.parent/'2026-11-02-bbinom-map-data'

def read(name):
    with (MAP/name).open() as stream:
        return list(csv.DictReader(stream))

def second_constant(x):
    return max(8*x*x+8*x+4,4*x+6)

@lru_cache(None)
def constants(x):
    h, M, S, m = len(x), max(x), sum(x), sum(v>0 for v in x)
    factorial=sum(gammaln(v+1) for v in x)
    J=sum(sum(1/j for j in range(1,v)) for v in x)
    poslog=sum(math.log(v) for v in x if v)
    return h,M,S,m,factorial,J,poslog

def small(x, n, T):
    h,M,S,m,factorial,J,poslog=constants(x)
    H = math.log1p(n/T)
    d = h*H*M-S
    root = math.sqrt(d*d+4*h*H*m*M)
    a = 2*m*M/(d+root) if d >= 0 else (-d+root)/(2*h*H)
    poly=m*math.log(a)+(S-m)*math.log(a+M)-h*H*a-factorial
    harmonic=m*(math.log(m/(h*H-J))-1)-poslog if h*H>J else math.inf
    return min(poly,harmonic)-S*math.log1p(-M/n)

def large(x, n, T, face, saturated, hist):
    G = sum(v*v/(2*(n-v)) for v in x)
    delta = sum(count*math.log1p(math.e*min(1.,second_constant(v)/T)*len(x)/count)
                for v,count in hist.items())
    log_remainder = delta+math.log1p(-math.exp(-delta)) if delta>50 else math.log(math.expm1(delta))
    logF = face-saturated
    high = max(logF, log_remainder)
    return G+saturated+high+math.log1p(math.exp(-abs(logF-log_remainder)))

def envelope(x,n,face):
    x=tuple(x)
    hist = Counter(x)
    saturated = sum(count*math.log(count/len(x)) for count in hist.values())
    lo, hi = -30., 100.
    # Small branch increases with T, large branch decreases. Bisection of
    # their crossing is therefore a global tuning search in exact arithmetic.
    for _ in range(65):
        z = (lo+hi)/2; T = math.exp(z)
        low, high = small(x,n,T),large(x,n,T,face,saturated,hist)
        if low>high: hi=z
        else: lo=z
    T = math.exp((lo+hi)/2)
    return max(small(x,n,T),large(x,n,T,face,saturated,hist)),T

def checks():
    mp.mp.dps=70
    rng=random.Random(113)
    errors = dict(second_order=-mp.inf,small_polynomial=-mp.inf,
                  small_harmonic=-mp.inf,normalized_product=-mp.inf)
    for _ in range(120):
        n=rng.choice([10,30,100])
        a=mp.mpf(10)**rng.uniform(-3,5)
        b=mp.mpf(10)**rng.uniform(-3,5)
        t=a+b
        x=[rng.randrange(min(n,8)) for j in range(rng.randrange(3,9))]
        if not max(x): x[0]=1
        z=[];q=[];bb=[];ds=[]
        for v in x:
            zz=mp.mpf(n)**v/mp.factorial(v)*mp.rf(a,v)/mp.rf(t,v)*mp.hyp1f1(a+v,t+v,-n)
            qq=mp.rf(a,v)/mp.factorial(v)*(t/(t+n))**a*(mp.mpf(n)/(t+n))**v
            dd=min(mp.mpf(1),second_constant(v)/t)
            errors['second_order']=max(errors['second_order'],abs(zz-qq)-dd)
            pp=mp.binomial(n,v)*mp.beta(v+a,n-v+b)/mp.beta(a,b)
            z.append(zz);q.append(qq);bb.append(pp);ds.append(dd)
        h,M,S,m=len(x),max(x),sum(x),sum(v>0 for v in x)
        H=mp.log1p(n/t)
        d=h*H*M-S
        aa=(-d+mp.sqrt(d*d+4*h*H*m*M))/(2*h*H)
        upper=m*mp.log(aa)+(S-m)*mp.log(aa+M)-h*H*aa-sum(mp.loggamma(v+1) for v in x)+S*mp.log(mp.mpf(n)/(n-M))
        errors['small_polynomial']=max(errors['small_polynomial'],sum(mp.log(p) for p in bb)-upper)
        J=sum(sum(mp.mpf(1)/j for j in range(1,v)) for v in x)
        if h*H>J:
            harmonic=m*(mp.log(m/(h*H-J))-1)-sum(mp.log(v) for v in x if v)+S*mp.log(mp.mpf(n)/(n-M))
            errors['small_harmonic']=max(errors['small_harmonic'],sum(mp.log(p) for p in bb)-harmonic)
        hist=Counter(x)
        saturated=mp.fprod((mp.mpf(c)/h)**c for c in hist.values())
        remainder=mp.fprod(1+mp.e*dd*h/hist[v] for v,dd in zip(x,ds))-1
        # Directly test the general expansion bound on shifted probabilities.
        errors['normalized_product']=max(errors['normalized_product'],
            mp.fprod(qq+dd for qq,dd in zip(q,ds))-mp.fprod(q)-saturated*remainder)
    assert all(v <= mp.mpf('1e-55') for v in errors.values())
    return {k:str(v) for k,v in errors.items()}

def main():
    errors=checks(); print('refined 70-digit checks',errors,flush=True)
    samples,faces,profiles=read('samples.csv'),read('faces.csv'),read('certified.csv')
    extensions={int(r['index']):r for r in read('extended.csv')}
    rows=[]
    counts={}
    for N in (1001,1000000,1000000000000):
        counts[N]=dict(clear=0,finite_status_0=0,nonconstant=0)
        for i,(sample,face,profile) in enumerate(zip(samples,faces,profiles)):
            x=list(map(int,sample['data'].split())); best=extensions.get(i,profile)
            upper,T=envelope(x,N,float(face['nb']))
            clear=upper<float(best['loglik'])-2e-8*len(x)
            if clear:
                counts[N]['clear']+=1
                counts[N]['finite_status_0']+=int(best['status'])==0
                counts[N]['nonconstant']+=len(set(x))>1
            rows.append(dict(index=i,tail_from_N=N,T=T,upper_loglik=upper,
                             candidate_N=best['N'],candidate_status=best['status'],
                             candidate_loglik=best['loglik'],clears_numerical_candidate=clear))
        print('refined budget',N,counts[N],flush=True)
    with (HERE/'refined-diagnostics.csv').open('w',newline='') as stream:
        writer=csv.DictWriter(stream,fieldnames=list(rows[0]));writer.writeheader();writer.writerows(rows)
    # Necessary condition from sum(delta) >= ? The normalized envelope is
    # larger than its first-order remainder e*sum(delta/r_i). Keep the exact
    # example as a measured tuning check instead of reusing the old bound.
    example=[]
    for N in (1001,1000000,1000000000000):
        upper,T=envelope([0,1,2],N,-3-math.log(2))
        example.append(dict(N=N,T=T,upper_loglik=upper,candidate_loglik=-3*math.log(3)))
    summary=dict(random_trials=120,mpmath_dps=70,seed=113,checks=errors,counts=counts,easy_example=example)
    (HERE/'refined-summary.json').write_text(json.dumps(summary,indent=2)+'\n')
    print('refined example',example,flush=True)
if __name__=='__main__':main()
