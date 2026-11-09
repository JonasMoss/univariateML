"""Independent 60-digit score/value checks and numerical root census.
Run after compiling probe.cpp and saving its stdout as probe.txt.
"""
from pathlib import Path
import json
import mpmath as mp
import numpy as np
from scipy.special import stdtr, gammaln
from scipy.optimize import brentq
mp.mp.dps=60
root=Path(__file__).resolve().parent
x=[mp.mpf(v) for v in ['-3','-1','-.2','.1','.8','2','4']]
def pdf(z,nu):
    return mp.gamma((nu+1)/2)/mp.gamma(nu/2)/mp.sqrt(nu*mp.pi)*(1+z*z/nu)**(-(nu+1)/2)
def cdf(z,nu):
    b=mp.betainc(nu/2,mp.mpf('.5'),0,nu/(nu+z*z),regularized=True)/2
    return b if z<0 else 1-b
def term(z,a,nu):
    h=a*z*mp.sqrt((nu+1)/(nu+z*z))
    return (nu+1)*z/(nu+z*z)-pdf(h,nu+1)/cdf(h,nu+1)*a*nu*mp.sqrt(nu+1)/(nu+z*z)**mp.mpf('1.5')
def ll(t,a,nu):
    return sum(mp.log(2*pdf(v-t,nu)*cdf(a*(v-t)*mp.sqrt((nu+1)/(nu+(v-t)**2)),nu+1)) for v in x)
fits=[];scales=[];ranges=0;max_violation=0
for line in (root/'probe.txt').read_text().splitlines():
    row=line.split()
    if row[0]=='fit':
        a,nu=map(mp.mpf,row[1:3]);assert row[3:5]==['0','1'],row
        estimate,value=map(mp.mpf,row[5:7])
        refined=mp.findroot(lambda t:sum(term(v-t,a,nu) for v in x),(estimate-mp.mpf('1e-5'),estimate+mp.mpf('1e-5')))
        assert abs(estimate-refined)<mp.mpf('1e-9'),(estimate,refined)
        assert abs(value-ll(refined,a,nu))<mp.mpf('1e-11')
        # Independent finite numerical census; this is evidence, not the global proof.
        aa,nn=float(a),float(nu);xx=np.array(list(map(float,x)))
        def score(t):
            z=xx-t;h=aa*z*np.sqrt((nn+1)/(nn+z*z))
            lp=gammaln((nn+2)/2)-gammaln((nn+1)/2)-.5*np.log((nn+1)*np.pi)-(nn+2)/2*np.log1p(h*h/(nn+1))
            return np.sum((nn+1)*z/(nn+z*z)-np.exp(lp)/stdtr(nn+1,h)*aa*nn*np.sqrt(nn+1)/(nn+z*z)**1.5)
        grid=np.linspace(-100,100,4001);vals=[score(t) for t in grid]
        roots=[brentq(score,l,r) for l,r,s,t in zip(grid[:-1],grid[1:],vals[:-1],vals[1:]) if s*t<0]
        assert roots and max(float(ll(mp.mpf(t),a,nu)) for t in roots)<=float(ll(refined,a,nu))+1e-10
        fits.append(dict(alpha=float(a),nu=float(nu),xi=str(refined),logLik=str(ll(refined,a,nu)),error=float(abs(estimate-refined)),roots=roots,evaluations=int(row[7])))
    elif row[0]=='scale':
        nu=mp.mpf(row[1]);assert row[2:4]==['0','1'],row
        estimate,value=map(mp.mpf,row[4:6])
        refined=mp.findroot(lambda s:-len(x)+(nu+1)*sum(v*v/(nu*s*s+v*v) for v in x),(estimate*.99,estimate*1.01))
        reference=sum(mp.log(pdf(v/refined,nu)/refined) for v in x)
        assert abs(estimate/refined-1)<mp.mpf('1e-10')
        assert abs(value-reference)<mp.mpf('1e-11')
        scales.append(dict(nu=float(nu),omega=str(refined),logLik=str(reference),relative_error=float(abs(estimate/refined-1))))
    elif row[0]=='golden':
        assert row[1:3]==['0','1'],row
        golden=json.loads((root.parents[3]/'univariateML-cpp/include/cppunivariate/distributions/st/golden.json').read_text())
        assert abs(float(row[3])-golden['estimates']['xi'])<1e-5
        assert abs(float(row[4])-golden['logLik'])<1e-8
    else:
        l,h,a,nu,lower,upper=map(mp.mpf,row[1:]);margin=mp.mpf(128)*mp.mpf(float(np.finfo(float).eps))*(1+max(abs(lower),abs(upper)))
        for z in [l,(3*l+h)/4,(l+h)/2,(l+3*h)/4,h]:
            v=term(z,a,nu);violation=max(lower-v,v-upper,mp.mpf(0));max_violation=max(max_violation,float(violation));assert violation<=margin,(row,z,v)
        ranges+=1
out=dict(precision=mp.mp.dps,range_boxes=ranges,range_points=5*ranges,max_unpadded_violation=max_violation,fits=fits,scales=scales)
(root/'checks.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(dict(fits=len(fits),scales=len(scales),range_points=5*ranges,max_unpadded_violation=max_violation,max_xi_error=max(v['error'] for v in fits))))
