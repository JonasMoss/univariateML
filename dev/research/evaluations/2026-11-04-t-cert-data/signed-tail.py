"""Conservative signed normal-tail bound; see signed-tail.md."""
import json
import numpy as np
import mpmath as mp
from study import HERE,scale_bounds,save,seam
mp.mp.dps=110
rows=[]
for case in json.loads((HERE/'inputs.json').read_text()):
    x=np.array(case['x']);x=(x-x.mean())/x.std();n=len(x);A=seam(x)['A']
    if A>=-1e-10:continue
    L,U=scale_bounds(x,2.)
    if L<=0:continue
    L=min(L,1.);U=max(U,1.);M=np.ptp(x)/L
    G=n*np.hypot((M**3+M)/L,M**4+M**2)
    c=n/(2*U*U)
    K=G*G/(4*c)+n*M**4/4+n/48
    N=max(2.,2*K/(-A))
    # Independent high-precision profile evaluation at the threshold.
    xx=list(map(mp.mpf,x));v=mp.mpf(N);nn=len(xx)
    def scores(m,t):
        s=mp.exp(t);z=[(q-m)/s for q in xx]
        return (mp.fsum((v+1)*q/(s*(v+q*q)) for q in z),mp.fsum((v+1)*q*q/(v+q*q)-1 for q in z))
    m,t=mp.findroot(scores,(0,0),tol=mp.mpf('1e-85'))
    ll=nn*(mp.loggamma((v+1)/2)-mp.loggamma(v/2)-mp.log(v*mp.pi)/2-t)-(v+1)/2*mp.fsum(mp.log1p(((q-m)/mp.exp(t))**2/v) for q in xx)
    mean=mp.fsum(xx)/nn;sd=mp.sqrt(mp.fsum((q-mean)**2 for q in xx)/nn)
    normal=-nn*(mp.log(sd)+mp.log(2*mp.pi)/2+mp.mpf('.5'))
    bound=mp.mpf(A)/v+mp.mpf(K)/(v*v)
    # The bound constants are binary64, so require only a wide corroboration.
    assert ll-normal<0 and ll-normal<bound*mp.mpf('1.001')
    rows.append({'name':case['name'],'A':A,'L':L,'U':U,'gradient_bound':G,'K':K,'N':N,'profile_minus_normal_at_N':str(ll-normal),'bound_at_N':str(bound)})
save('signed-tail.json',{'precision_digits':110,'rows':rows})
print('Signed-tail threshold checks passed for',len(rows),'negative-slope samples.')
