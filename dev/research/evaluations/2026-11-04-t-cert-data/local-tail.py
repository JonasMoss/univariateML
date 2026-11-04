"""Sharper signed tail using a nuisance ball and analytic Hessian bounds."""
import json
import numpy as np
import mpmath as mp
mp.mp.dps=80
from study import HERE,save,seam

def certify(x):
    x=(x-x.mean())/x.std();n=len(x);A=seam(x)['A']
    if A>=-1e-10:return {'outcome':'slope_not_strictly_negative','A':A}
    G=np.hypot(np.sum(np.abs(x*(1-x*x))),np.sum(np.abs(x*x*(1-x*x))))
    for radius in [.01,.03,.1]:
        for j in range(100):
            h=2.**(-j-1)
            zlo=np.minimum((x-radius)*np.exp(-radius),(x-radius)*np.exp(radius))
            zhi=np.maximum((x+radius)*np.exp(-radius),(x+radius)*np.exp(radius))
            zmax2=np.maximum(zlo*zlo,zhi*zhi)
            zmin2=np.where((zlo<=0)&(zhi>=0),0,np.minimum(zlo*zlo,zhi*zhi))
            if np.max(h*zmax2)>=1:continue
            q=(1+h*zmax2)**2
            # Enclose the sum before taking its absolute value.
            crosslo=np.sum(np.minimum(zlo,zlo/q));crosshi=np.sum(np.maximum(zhi,zhi/q))
            cross=2*(1+h)*np.exp(radius)*max(abs(crosslo),abs(crosshi))
            hmm=np.exp(-2*radius)*np.sum((1-h*zmax2)/q)
            htt=2*np.sum(zmin2/q)
            curvature=min(hmm-cross,htt-cross)
            if curvature<=0 or h*G>=curvature*radius:continue
            B=np.sum(x**4)/4+n*h/24+G*G/(2*curvature)
            if h*B> -A/2:continue
            return {'outcome':'signed_tail','A':A,'B':B,'h':h,'N':1/h,'radius':radius,
                    'curvature_lower':curvature,'score_bound_coefficient':G,
                    'radial_margin':curvature*radius-h*G,'slope_margin':-A/2-h*B}
    return {'outcome':'budget','A':A}
rows=[]
for case in json.loads((HERE/'inputs.json').read_text()):
    original=np.array(case['x']);q=certify(original);q['name']=case['name']
    if q['outcome']=='signed_tail':
        # Independent roots in exact original binary64 data, avoiding a
        # full-likelihood subtraction in binary64 at the threshold.
        xx=list(map(mp.mpf,case['x']));n=len(xx);mean=mp.fsum(xx)/n
        sd=mp.sqrt(mp.fsum((v-mean)**2 for v in xx)/n);xx=[(v-mean)/sd for v in xx]
        normal=-n*(mp.log(2*mp.pi)/2+mp.mpf('.5'));checks=[]
        for multiplier in [1,2]:
            nu=mp.mpf(q['N'])*multiplier
            def score(m,t):
                z=[(v-m)/mp.exp(t) for v in xx]
                return (mp.fsum((nu+1)*v/(mp.exp(t)*(nu+v*v)) for v in z),
                        mp.fsum((nu+1)*v*v/(nu+v*v)-1 for v in z))
            m,t=mp.findroot(score,(0,0),tol=mp.mpf('1e-60'))
            value=n*(mp.loggamma((nu+1)/2)-mp.loggamma(nu/2)-mp.log(nu*mp.pi)/2-t)-(nu+1)/2*mp.fsum(mp.log1p(((v-m)/mp.exp(t))**2/nu) for v in xx)
            bound=mp.mpf(q['A'])/nu+mp.mpf(q['B'])/nu**2
            assert value-normal<bound and value-normal<0
            checks.append({'nu':str(nu),'profile_minus_normal':str(value-normal),'bound':str(bound)})
        q['independent_checks']=checks
    rows.append(q)
assert not any(q['outcome']=='budget' for q in rows)
save('local-tail.json',rows)
print('Local signed tails:',sum(q['outcome']=='signed_tail' for q in rows),'of',len(rows),'workloads.')
