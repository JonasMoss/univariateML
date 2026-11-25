"""Complete score cover for held mean, held skew and held df in sstd."""
import json
from math import exp, log, sqrt
from pathlib import Path
import numpy as np
import mpmath as mp
from boxes import I, load

mp.mp.dps=80


def case(x, mean, nu, skew):
    x=np.array(x)
    d=x-mean
    v,k=map(mp.mpf,(nu,skew))
    raw_m=(k-1/k)*2*mp.sqrt(v)*mp.gamma((v+1)/2)/(
        (v-1)*mp.sqrt(mp.pi)*mp.gamma(v/2))
    M=float(raw_m)
    raw_v=v/(v-2)*(1+(k-1/k)**2)-raw_m**2
    a,b=sqrt(nu)/skew,sqrt(nu)*skew
    nonzero=np.count_nonzero(d)
    n=len(d)
    e=(1+nu)*nonzero-n
    assert e>0
    left=n*min(a,b)/((nu+1)*sum(abs(d)))
    D=max(1.,2*abs(M)*(1+nu)*nonzero/e,
          max(a,b)*sqrt(2*(1+nu)*nonzero/e))
    right=2*(D+abs(M))/min(abs(d[d!=0]))
    # A conservative RHS at least (|M|/D+a_max²/D²) <= e/((nu+1)p).
    # Doubling the resulting y makes |r|>=2 D, and leaves strict slack.
    assert right>left
    Mbox=I.pad(M,M,4e-16)
    abox=I.pad(a,a)
    bbox=I.pad(b,b)

    def interval(l,r):
        s=I(l,r).exp()
        z=I(d)*s
        R=z+Mbox
        score=[]; diff=[]
        for a_i in [abox,bbox]:
            D=R.sq()+a_i.sq()
            term=z*R/D
            deriv=term+z.sq()*(a_i.sq()-R.sq())/D.sq()
            score.append(term); diff.append(deriv)

        def combine(left,right):
            return I(np.where(R.hi<=0,left.lo,np.where(R.lo>=0,right.lo,np.minimum(left.lo,right.lo))),
                     np.where(R.hi<=0,left.hi,np.where(R.lo>=0,right.hi,np.maximum(left.hi,right.hi))))
        return 1-(nu+1)/n*combine(*score).sum(), -(nu+1)/n*combine(*diff).sum()

    grid=np.linspace(log(left),log(right),65)
    stack=list(zip(grid[:-1],grid[1:]))
    candidates=[]
    cells=0
    for _ in range(25600):
        if not stack:
            break
        l,r=stack.pop()
        cells+=1
        g,h=interval(l,r)
        if g.lo>0 or g.hi<0:
            continue
        c=(l+r)/2
        mid,_=interval(c,c)
        bound=mid+h*I(l-c,r-c)
        if bound.lo>0 or bound.hi<0:
            continue
        if h.lo>0 or h.hi<0:
            low,high=interval(l,l)[0],interval(r,r)[0]
            if (low.hi<0 and high.lo>0) or (low.lo>0 and high.hi<0):
                # Existing strict monotonicity applies to the narrower bracket.
                a0,b0=l,r
                downward=h.hi<0
                for _ in range(64):
                    center=(a0+b0)/2
                    z,_=interval(center,center)
                    if z.lo<=0<=z.hi:
                        break
                    if (z.lo>0)==downward:
                        a0=center
                    else:
                        b0=center
                    if b0-a0<5e-12:
                        break
                candidates.append((l,r,a0,b0,'maximum' if downward else 'minimum'))
                continue
            if (low.lo>0 and high.lo>0) or (low.hi<0 and high.hi<0):
                continue
        stack.extend([(l,c),(c,r)])
    assert not stack
    residual=[mp.mpf(float(t))-mp.mpf(mean) for t in x]
    normal=mp.log(2*mp.gamma((v+1)/2)/(mp.gamma(v/2)*mp.sqrt(v*mp.pi)*(k+1/k)))

    def likelihood(t):
        y=mp.exp(t)
        total=0
        for d_i in residual:
            z=d_i*y+raw_m
            scale=k if z>0 else 1/k
            total+=mp.log1p(z*z/(v*scale*scale))
        return n*(normal+t)-(v+1)/2*total

    found=[]
    for l,r,a0,b0,kind in sorted(candidates):
        root=mp.findroot(lambda t:mp.diff(likelihood,t),(a0,b0))
        assert a0<=root<=b0, (a0,b0,root)
        g_box,_=interval(a0,b0)
        height=likelihood((mp.mpf(a0)+mp.mpf(b0))/2)
        width=mp.mpf(float(g_box.mag()))*n*(mp.mpf(b0)-mp.mpf(a0))/2+mp.mpf('1e-12')
        exact=likelihood(root)
        assert height-width<=exact<=height+width
        found.append({'kind':kind,'log_sd':mp.nstr(mp.log(raw_v)/2-root,24),
                      'loglik':mp.nstr(exact,24),'log_y_width':b0-a0,
                      'likelihood_box':[float(height-width),float(height+width)]})
    heights=sorted(r['likelihood_box'] for r in found if r['kind']=='maximum')
    assert len(heights)==1 or heights[-2][1]<heights[-1][0]
    return {'cells':cells, 'initial_log_y_extent':[log(left),log(right)],'stationary':found,
            'top_gap_lower':heights[-1][0]-heights[-2][1] if len(heights)>1 else None}


if __name__=='__main__':
    p=Path(__file__)
    old=json.loads((p.parents[1]/'2026-11-06-sstd-cert-data/skew-scale.json').read_text())
    data=load('sstd')
    est=data['estimates']
    results={'golden':case(data['data'],est['mean'],est['nu'],est['xi']),
             'six_point':case(old['data'],old['mean'],old['nu'],old['xi'])}
    print(json.dumps(results,indent=2))
