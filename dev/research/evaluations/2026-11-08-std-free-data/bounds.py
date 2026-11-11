#!/usr/bin/env python3
"""Independent 80-digit sanity checks of the proved constrained bounds.
These point checks do not replace the analytical uniform-bound proofs.
"""
import json
import mpmath as mp
mp.mp.dps = 80
checks = 0

def check(ok):
    global checks
    assert ok
    checks += 1

def c(v):
    return mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2

def ll(x,m,s,v,std=False):
    r=v-2 if std else v
    return len(x)*(c(v)+mp.log(v/r)/2-mp.log(s))-(v+1)/2*sum(mp.log1p(((a-m)/s)**2/r) for a in x)

def normal(x,m,s):
    return -len(x)*mp.log(2*mp.pi)/2-len(x)*mp.log(s)-sum(((a-m)/s)**2 for a in x)/2

def score(x,m,s,v):
    return (v+1)*sum((a-m)**2/(v*s*s+(a-m)**2) for a in x)-len(x)

x=list(map(mp.mpf,[-3,-1,0,1,4]));m=mp.mpf('0.2');s=mp.mpf('1.3')
q=[((a-m)/s)**2 for a in x]
for std,values in [(False,['.001','.1','.5','1','3','100']),(True,['2.01','2.5','3','10','100'])]:
    for value in values:
        v=mp.mpf(value);r=v-2 if std else v
        R=lambda u:mp.log1p(u)-u/(1+u)
        g=len(x)*mp.diff(c,v)-sum(R(z/r) for z in q)/2+(3 if std else 1)*sum(z/(2*r*r*(1+z/r)) for z in q)
        h=len(x)*mp.diff(c,v,2)+sum(z*(z-(6 if std else 2)-(3 if std else 1)*z/r)/(2*r**3*(1+z/r)**2) for z in q)
        if std:
            g-=len(x)/(v*r)
            h+=len(x)*(1/v+1/r)/(v*r)
        f=lambda t:ll(x,m,s,t,std)
        check(mp.almosteq(g,mp.diff(f,v),rel_eps=mp.mpf('1e-60'),abs_eps=mp.mpf('1e-60')))
        check(mp.almosteq(h,mp.diff(f,v,2),rel_eps=mp.mpf('1e-60'),abs_eps=mp.mpf('1e-60')))
        check(1/(4*v*v)-1/(8*v**4)<=mp.diff(c,v)<=1/(4*v*v))

# Held-location scale roots: monotone brackets and the refitted normal tail.
d=[a-m for a in x];dmin=min(abs(a) for a in d);D2=sum(a*a for a in d)/len(d)
LN=normal(x,m,mp.sqrt(D2));C=sum(a**4 for a in d)/(4*dmin**4)
for value in ['.1','.5','1','2','4','10','100','1000']:
    v=mp.mpf(value);lo=dmin;hi=mp.sqrt((1+1/v)*D2)
    check(score(x,m,lo,v)>=0);check(score(x,m,hi,v)<=0)
    for _ in range(200):
        mid=(lo+hi)/2
        if score(x,m,mid,v)>0:lo=mid
        else:hi=mid
    root=(lo+hi)/2
    if v>=2:check(ll(x,m,root,v)-LN<=C*(1/v+1/v**2))
    lower=len(x)*mp.log(v)/2-len(x)*mp.log(2*mp.pi)/2-sum(mp.log(abs(a)) for a in d)
    check(ll(x,m,root,v)<=lower)

# Actual fixed-scale normal tails, uniformly over a sampled location grid.
R=max(x)-min(x);b=[max(a-min(x),max(x)-a) for a in x]
C=sum((a/s)**4 for a in b)/4;LN=normal(x,sum(x)/len(x),s)
locations=[min(x)+R*i/20 for i in range(21)]
for v in map(mp.mpf,[3,5,10,100]):
    for loc in locations:
        check(ll(x,loc,s,v)-LN<=C*(1/v+1/v**2))
        w=1/(v-2)
        check(ll(x,loc,s,v,True)-LN<=len(x)*w+C*(w+3*w*w))

# Uniform moving-location lower shape face for held standard deviation.
s=mp.mpf(1);j=1;delta=min(x[i+j]-x[i] for i in range(len(x)-j))/4
Q=(delta/s)**2;n=len(x)
constant=-n*mp.log(s)-n*mp.log(2*mp.pi)/2+n*mp.log(3)/2-3*(n-j)*mp.log(Q)/2
for w in map(mp.mpf,['.000001','.0001','.001','.01']):
    check(w<=min(1,Q))
    for loc in locations:
        check(ll(x,loc,s,2+w,True)<=constant+(n-mp.mpf('1.5')*j)*mp.log(w))

# Critical two-thirds atom: inner relative bound and outer uniform bound.
x=list(map(mp.mpf,[0,0,1]));n=3;k=2;h=1;delta=mp.mpf(1)/4
M=4*h/(1-delta);K=M*M/(3*k);D=mp.mpf(15)/16;L0=-3*mp.log(2)
outer_delta=mp.mpf(1)/8;Q=outer_delta**2
outer=-n*mp.log(2*mp.pi)/2+n*mp.log(3)/2-3*(n-1)*mp.log(Q)/2
for w in map(mp.mpf,['.00000001','.0000001','.000001']):
    a=mp.sqrt(w);bound=w*(D+K+h*mp.log(w)/2)
    check(bound<0)
    for loc in [-delta,-a,-a/2,-w,0,w,a/2,a,delta]:
        check(ll(x,loc,1,2+w,True)-L0<=bound)
    check(outer+mp.mpf('1.5')*mp.log(w)<L0)
    for loc in map(mp.mpf,['.25','.5','.75','1','2']):
        check(ll(x,loc,1,2+w,True)<=outer+mp.mpf('1.5')*mp.log(w))

# Explicit stationary-scale containment below shape one, without uniqueness.
x=list(map(mp.mpf,[-100,-99,0,99,100]));n=len(x);j=1
for v in map(mp.mpf,['.3','.5','.75']):
    delta=min(x[i+j]-x[i] for i in range(n-j))/4;E=v*(n-j)-j
    check(E>0);lower=delta*mp.sqrt(E/(n*v));upper=(max(x)-min(x))*mp.sqrt(1+1/v)
    for loc in [min(x)+(max(x)-min(x))*i/40 for i in range(41)]:
        check(score(x,loc,lower,v)>0)
        check(score(x,loc,upper,v)<0)
# Normal-relative value identity, including very large shapes.
for standardized in [False,True]:
    x=list(map(mp.mpf,[-1,2]));loc=mp.mpf(.2);scale=mp.sqrt(mp.mpf('2.34'))
    q=[((z-loc)/scale)**2 for z in x];n=len(x)
    for v in map(mp.mpf,['1e4','1e8','1e12']):
        r=v-2 if standardized else v
        gamma_correction=mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v/2)/2
        normal=-n*(mp.log(2*mp.pi)/2+mp.log(scale))-sum(q)/2
        relative=normal+n*(gamma_correction+(mp.log1p(2/r)/2 if standardized else 0))
        relative+=sum((v+1)*(z/r-mp.log1p(z/r))/2-(mp.mpf('1.5') if standardized else mp.mpf('.5'))*z/r for z in q)
        check(abs(relative-ll(x,loc,scale,v,standardized))<mp.mpf('1e-60'))
print(json.dumps({'precision_dps':mp.mp.dps,'checks':checks,'failed':0,
                  'evidence':'Numerical sanity checks; uniform validity is proved in derivation.md.'},indent=2))
