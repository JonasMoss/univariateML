"""Global certificate experiment; explicit budgets, never a production fit."""
import csv
import heapq
import json
import time
import numpy as np
from scipy.special import xlogy
from isolate import HERE, PREV, I, PAD, interval_data, xi


def entropy(p):
    assert np.all(p.lo>=0) and np.all(p.hi<=1)
    def h(x):return -xlogy(x,x)-xlogy(1-x,1-x)
    low=np.minimum(h(p.lo),h(p.hi))
    high=np.where((p.lo<=.5)&(p.hi>=.5),np.log(2),np.maximum(h(p.lo),h(p.hi)))
    return I.padded(low,high)


def primal(y,t):
    k,c,b=t;n=y.lo.size;z=c*y+b
    return n*I(k).log()+n*I(c).log()+z.sum()-(k+1)*z.sp().sum()


def dual_profile(y,k):
    # Numerical solver is only a supplier of feasible primal/dual proposals.
    fit=xi.previous.profile((y.lo+y.hi)/2,k)
    n=y.lo.size;c,b=fit['c'],fit['eta']-np.log(k)
    proposal=__import__('scipy').special.expit(c*(y.lo+y.hi)/2+b)
    for mix in [0.,1e-6,1e-4,.01,.1]:
        p=(1-mix)*proposal+mix/(k+1)
        j=int(np.argmax(p*(1-p)))
        others=np.delete(p,j)
        # Define p_j EXACTLY by the affine constraint; evaluate an enclosure.
        last=n/(I(k)+1)-I(others).sum()
        if last.lo>=0 and last.hi<=1:break
    else:raise ArithmeticError('dual-repair-domain')
    pl=p.copy();ph=p.copy();pl[j]=last.lo;ph[j]=last.hi
    pi=I(pl,ph)
    a=(((k+1)*pi-1)*y).sum()
    if a.lo<=0:raise ArithmeticError('dual-A')
    u=n*I(k).log()+n*(n/a).log()-n-(k+1)*entropy(pi).sum()
    l=primal(y,[k,c,b])
    assert u.hi>=l.lo
    return dict(k=float(k),lower=float(l.lo),upper=float(u.hi),
                gap=float(u.hi-l.lo),repair_width=float(last.hi-last.lo))


def weibull_box(y):
    n=y.lo.size;ym=(y.lo+y.hi)/2
    _,_,c=xi.previous.weibull(ym)
    def at(c):
        e=(c*y).exp();s=e.sum();v=n*e/s
        g=n/c+y.sum()-n*(y*e).sum()/s
        val=n*I(c).log()+c*y.sum()-n*(s/n).log()-n
        return g,val,v
    width=1e-10*max(c,1)
    for _ in range(32):
        lo,hi=c-width,c+width
        if lo<=0:raise ArithmeticError('weibull-bracket-domain')
        gl,vl,_=at(lo);gh,_,_=at(hi)
        if gl.lo>0 and gh.hi<0:break
        width*=2
    else:raise ArithmeticError('weibull-bracket-budget')
    cc=I(lo,hi);e=(cc*y).exp();eta=(n/e.sum()).log()
    v=(cc*y+eta).exp()
    A=(v*v/2).sum()-n
    # Concavity tangent from lower bracket, and any feasible lower value.
    wlow=at(c)[1].lo
    whigh=(vl+gl*(hi-lo)).hi
    assert whigh>=wlow
    return dict(c=cc,eta=eta,v=v,A=A,W=I(wlow,whigh),bracket_score=[gl.pair(),gh.pair()])


def signed_tail(y,w):
    n=y.lo.size
    if w['A'].hi>=0:return None
    v=w['v'];c=w['c'];norm=y.mag()+1
    d=[(y*v*(v-1)).sum(),(v*(v-1)).sum()]
    E=float((v*v*I((v-1).mag())*norm).sum().hi)
    candidates=[]
    for fraction in [.005,.02,.1,.3]:
        R=fraction*min(float(c.lo),1)
        low=(I(v.lo).log()-R*norm).exp()
        high=(I(v.hi).log()+R*norm).exp()
        h=1.
        for _ in range(80):
            weights=low/(1+h*high)/(1+h*high)
            # Retain only lower weights; matrix lower bound is a sum of PSD terms.
            ww=I(weights.lo)
            a=n/(float(c.hi)+R)**2+(ww*y*y).sum()
            b=(ww*y).sum();e=ww.sum()
            det=a*e-b*b
            if det.lo<=0 or a.lo<=0 or e.lo<=0:
                h/=2;continue
            mu=float((det/(a+e)).lo)
            if not np.isfinite(mu) or mu<=0:
                h/=2;continue
            D=sum(float(t.mag()) for t in d)+h*E
            # Inflate scalar final arithmetic as well.
            DD=I.padded(D,D)
            B=n/6+DD*DD/(2*I(mu))
            containment=h*DD/(I(mu)*R)
            slope=h*B/(-w['A'].hi)
            if containment.hi<.5 and slope.hi<.5:
                candidates.append(dict(h=h,R=R,mu=mu,D=float(DD.hi),B=float(B.hi),
                                       containment=float(containment.hi),slope=float(slope.hi)))
                break
            h/=2
    return max(candidates,key=lambda t:t['h']) if candidates else None


def interval_upper(a,b,n):
    """Stable concave log/chord maximum using interval arithmetic."""
    ka,kb=a['k'],b['k']
    # Avoid cancellation at close endpoints by evaluating only log excess
    # plus convex combinations of endpoint likelihood upper values.
    # n[log k - linear interpolation(log endpoints)] <= n (kb-ka)^2/(8 ka^2).
    # This deliberately looser bound avoids division by a tiny chord slope.
    excess=n*(I(kb)-ka)*(I(kb)-ka)/(8*I(ka)*I(ka))
    return float((max(a['upper'],b['upper'])+excess).hi)


def check(name,x,roots,budget=4096):
    start=time.perf_counter();raw=np.log(x);center=float(np.mean(raw));unit=float(np.std(raw))
    y=(interval_data(np.array(x))-center)/unit;n=len(x);ym=(y.lo+y.hi)/2
    m=sum(np.array(x)==min(x));k0=np.nextafter(m/(n-m),0.)
    # Exact min(log x) is a data log, not the lower endpoint of its enclosure.
    j=int(np.argmin(x));minimum=I(y.lo[j],y.hi[j])
    p=n*(n/(y-minimum).sum()).log()-n
    w=weibull_box(y);tail=signed_tail(y,w)
    finite=[r for r in roots if r['sample']==name and r['type']=='maximum']
    vals=[primal(y,[r['center'][0],r['center'][1]*unit,r['center'][2]+r['center'][1]*center]) for r in finite]
    best=max([float(p.lo),float(w['W'].lo)]+[float(a.lo) for a in vals])
    winner='Pareto' if p.lo>=w['W'].lo else 'Weibull'
    rootwin=None
    if vals and max(float(a.lo) for a in vals)>max(p.hi,w['W'].hi):
        j=max(range(len(vals)),key=lambda j:vals[j].lo)
        best=float(vals[j].lo);winner='finite';rootwin=finite[j]
    cache={};maxgap=0.;maxrepair=0.
    def get(k):
        nonlocal maxgap,maxrepair
        k=float(k)
        if k not in cache:
            if len(cache)>=budget:raise ArithmeticError('subdivision-budget')
            a=dual_profile(y,k);cache[k]=a;maxgap=max(maxgap,a['gap']);maxrepair=max(maxrepair,a['repair_width'])
        return cache[k]
    K=float(2*n);tailtype='unsigned'
    if tail:K=1/tail['h'];tailtype='signed'
    else:
        for _ in range(80):
            err=n*n*(I(K)+1)/(2*(I(K)+1-n)*(I(K)+1-n))
            if (w['W']+err).hi<best:break
            K*=2
        else:return dict(sample=name,outcome='tail-budget',seconds=time.perf_counter()-start,A=w['A'].pair())
    excluded=[]
    if rootwin:
        d=rootwin['slab']['delta'];k=rootwin['center'][0]
        # Tiny root enclosure must lie strictly inside slab.
        assert rootwin['radii'][0]<d
        assert all(a<b for a,b in zip(rootwin['radii'][1:],rootwin['slab']['nuisance_radii']))
        excluded=[(k-d,k+d)]
    knots=list(np.geomspace(k0,K,max(2,int(np.ceil(np.log(K/k0)/.4))+1)))
    for a,b in excluded:knots.extend([a,b])
    knots=sorted(set(knots));heap=[];serial=0;pruned=0
    def push(a,b):
        nonlocal serial,pruned
        if any(a['k']>=lo and b['k']<=hi for lo,hi in excluded):return
        u=interval_upper(a,b,n)
        if u<best:pruned+=1;return
        serial+=1;heapq.heappush(heap,(-u,serial,a,b))
    try:
        nodes=[get(k) for k in knots]
        for a,b in zip(nodes[:-1],nodes[1:]):push(a,b)
        while heap:
            _,_,a,b=heapq.heappop(heap)
            mid=get(np.sqrt(a['k']*b['k']))
            push(a,mid);push(mid,b)
        if winner=='Weibull' and p.hi>=w['W'].lo:raise ArithmeticError('face-tie-budget')
        if winner=='Pareto' and w['W'].hi>=p.lo:raise ArithmeticError('face-tie-budget')
        outcome='global-'+winner
    except ArithmeticError as e:outcome=str(e)
    return dict(sample=name,outcome=outcome,profiles=len(cache),seconds=time.perf_counter()-start,
                candidate=winner,root_slab=excluded,pruned=pruned,K=K,tail=tailtype,tail_constants=tail,
                A=w['A'].pair(),W=w['W'].pair(),P=p.pair(),max_dual_gap=maxgap,
                max_repair_width=maxrepair,remaining_upper=(-heap[0][0] if heap else None),lower=best)


def main():
    samples=json.loads((PREV/'samples.json').read_text());roots=json.loads((HERE/'isolation.json').read_text())['certificates']
    rows=[]
    for name,x in samples.items():
        try:r=check(name,x,roots)
        except (ArithmeticError,AssertionError,ValueError,OverflowError) as e:
            r=dict(sample=name,outcome='arithmetic-budget',detail=str(e))
        rows.append(r);print(name,r['outcome'],r.get('profiles'),r.get('seconds'),flush=True)
    (HERE/'global.json').write_text(json.dumps(rows,indent=2)+'\n')

if __name__=='__main__':main()
