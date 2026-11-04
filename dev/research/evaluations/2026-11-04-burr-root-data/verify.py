"""Independent high precision corroboration of the retained box comparisons."""
import json
import csv
from pathlib import Path
import time
import mpmath as mp
import numpy as np
from isolate import HERE, PREV
from global_check import signed_tail, I

mp.mp.dps=80
TOL=mp.mpf('1e-60')


def main():
    start=time.perf_counter();samples=json.loads((PREV/'samples.json').read_text())
    roots=json.loads((HERE/'isolation.json').read_text())['certificates']
    globals=json.loads((HERE/'global.json').read_text())
    rootchecks=[];facechecks=[]
    prior=list(csv.DictReader((PREV/'roots.csv').open()))
    for index,info in enumerate(roots):
        x=samples[info['sample']];y=[mp.log(mp.mpf(v)) for v in x];n=len(y)
        def score(k,c,b):
            z=[c*v+b for v in y];p=[1/(1+mp.exp(-v)) for v in z]
            return (n/k-sum(mp.log1p(mp.exp(v)) for v in z),n/c+sum(y)-(k+1)*sum(v*a for v,a in zip(y,p)),n-(k+1)*sum(p))
        t=mp.findroot(score,tuple(map(mp.mpf,info['center'])),tol=TOL,maxsteps=40)
        relative=[]
        for v,a,r in zip(t,info['center'],info['radii']):
            ratio=abs(v-mp.mpf(a))/mp.mpf(r)
            assert ratio<1
            relative.append(float(ratio))
        k,c,b=t;s=mp.exp(-b/c)
        for v,box in zip([k,c,s],info['coefficient_bounds']):assert mp.mpf(box[0])<v<mp.mpf(box[1])
        H=mp.matrix([[mp.diff(lambda z:score(z,c,b)[0],k),mp.diff(lambda z:score(k,z,b)[0],c),mp.diff(lambda z:score(k,c,z)[0],b)],
                     [mp.diff(lambda z:score(z,c,b)[1],k),mp.diff(lambda z:score(k,z,b)[1],c),mp.diff(lambda z:score(k,c,z)[1],b)],
                     [mp.diff(lambda z:score(z,c,b)[2],k),mp.diff(lambda z:score(k,z,b)[2],c),mp.diff(lambda z:score(k,c,z)[2],b)]])
        eig=mp.eigsy(H,eigvals_only=True)
        assert (eig[2] < 0) if info['type']=='maximum' else (eig[2]>0)
        mixed=mp.matrix([H[1,0],H[2,0]])
        profile_curvature=H[0,0]-(mixed.T*mp.lu_solve(H[1:3,1:3],mixed))[0]
        seam_curvature=k**4*profile_curvature
        assert (seam_curvature<0) if info['type']=='maximum' else (seam_curvature>0)
        old=float(prior[index]['seam_curvature'])
        rootchecks.append(dict(sample=info['sample'],type=info['type'],box_fraction=max(relative),
                               k=str(k),c=str(c),scale=str(s),profile_k_curvature=str(profile_curvature),
                               seam_curvature=str(seam_curvature),
                               prior_quadrature_curvature=old if np.isfinite(old) else None))
    for info in globals:
        x=samples[info['sample']];raw=np.log(np.array(x));center=float(np.mean(raw));unit=float(np.std(raw))
        y=[(mp.log(mp.mpf(v))-mp.mpf(center))/mp.mpf(unit) for v in x];n=len(y)
        def at(c):
            e=[mp.exp(c*v) for v in y];v=[n*a/sum(e) for a in e]
            return n/c+sum(y)-sum(a*b for a,b in zip(y,v)),v
        from isolate import xi
        yn=(raw-center)/unit
        c0=xi.previous.weibull(yn)[2]
        c=mp.findroot(lambda z:at(z)[0],(mp.mpf(c0)*mp.mpf('.999'),mp.mpf(c0)*mp.mpf('1.001')),tol=TOL)
        v=at(c)[1];eta=mp.log(n/sum(mp.exp(c*a) for a in y))
        W=n*mp.log(c)+n*eta+c*sum(y)-n
        A=sum(a*a for a in v)/2-n
        P=n*mp.log(n/sum(a-min(y) for a in y))-n
        for v0,box in [(W,info['W']),(A,info['A']),(P,info['P'])]:
            assert mp.mpf(box[0])<v0<mp.mpf(box[1]),(info['sample'],str(v0),box)
        # Check the decisive signed-tail comparisons at 80 digits.
        tail=info.get('tail_constants')
        check={}
        if tail:
            h,R,mu,D,B=[mp.mpf(tail[k]) for k in ['h','R','mu','D','B']]
            assert A<0 and R<c
            norms=[abs(a)+1 for a in y];vv=at(c)[1]
            weight=[a*mp.exp(-R*r)/(1+h*a*mp.exp(R*r))**2 for a,r in zip(vv,norms)]
            M=mp.matrix([[n/(c+R)**2+sum(a*b*b for a,b in zip(weight,y)),sum(a*b for a,b in zip(weight,y))],
                         [sum(a*b for a,b in zip(weight,y)),sum(weight)]])
            assert mu < mp.eigsy(M,eigvals_only=True)[0]
            d=[sum(a*(a-1)*b for a,b in zip(vv,y)),sum(a*(a-1) for a in vv)]
            exactD=sum(abs(a) for a in d)+h*sum(a*a*abs(a-1)*r for a,r in zip(vv,norms))
            assert D>exactD
            assert B>n/mp.mpf(6)+D*D/(2*mu)
            assert h*D<mu*R/2 and h*B<-A/2
            check=dict(containment=float(h*D/(mu*R)),slope_ratio=float(h*B/(-A)))
        facechecks.append(dict(sample=info['sample'],W=str(W),A=str(A),P=str(P),tail=check))
    # A sign enclosure containing zero cannot trigger a Weibull decision.
    # These calls return before accessing other constants by design.
    synthetic=[]
    for interval in [[-1e-8,1e-8],[-1e-14,1e-14],[0.,0.],[1e-14,2e-14]]:
        assert signed_tail(I([0.,1.]),{'A':I(*interval)}) is None
        synthetic.append(dict(A=interval,signed_tail_selected=False))
    nearzero=[a for a in globals if a['sample'].startswith('closure-zero-')]
    assert len(nearzero)==3
    assert all(a['A'][0]<0<a['A'][1] and a['outcome']=='global-finite' and a['tail']=='unsigned' for a in nearzero)
    result=dict(precision=80,root_tolerance=str(TOL),seconds=time.perf_counter()-start,
                roots=rootchecks,faces=facechecks,synthetic_seam_checks=synthetic,
                nearzero_unsigned_finite_certificates=len(nearzero))
    (HERE/'verification.json').write_text(json.dumps(result,indent=2)+'\n')
    print('Independent checks:',len(rootchecks),'roots,',len(facechecks),'faces,',len(synthetic),'seam checks',flush=True)

if __name__=='__main__':main()
