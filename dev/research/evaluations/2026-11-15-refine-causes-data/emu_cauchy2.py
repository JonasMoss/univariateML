"""Cauchy: why tol=1e-10 alone does not fix case 764. Trace with the monotone line search, then with a noise-tolerant acceptance."""
import sys, json, math
sys.path.insert(0,'dev/validation'); sys.path.insert(0,'dev/research/evaluations/2026-11-15-refine-causes-data')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=50
import emu_cauchy as ec
eps=ec.eps
def fit(x,tol,slack=0.0,maxit=100,verbose=False):
    n=len(x); d=sorted(x); mu=d[n//2]
    d=sorted(abs(v-mu) for v in d); th=math.log(d[n//2]+eps)
    for it in range(1,maxit+1):
        inv=math.exp(-th); s1=s2=s3=s4=s5=0.0
        for v in x:
            z=(v-mu)*inv; z2=z*z; i1=1/(1+z2); i2=i1*i1
            s1+=z*i1; s2+=(z2-1)*i1; s3+=(1-z2)*i2; s4+=z*i2; s5+=z2*i2
        U=2*inv*s1; Ut=s2; Hm=2*inv*inv*s3; Hmt=4*inv*s4; Ht=4*s5
        det=Hm*Ht-Hmt*Hmt
        dmu=(Ht*U-Hmt*Ut)/det; dth=(-Hmt*U+Hm*Ut)/det
        old=ec.cost(x,mu,math.exp(th)); st=1.0
        for k in range(100):
            c=(mu+st*dmu,math.exp(th+st*dth))
            if c[1]>0 and ec.cost(x,*c)<=old+slack*max(abs(old),n): break
            st*=.5
        if verbose: print('  it',it,'newton step',dmu,dth,'accepted fraction',st,'cost diff at full step',ec.cost(x,mu+dmu,math.exp(th+dth))-old)
        dmu*=st; dth*=st; mu+=dmu; th+=dth
        if max(abs(dmu)/math.exp(th),abs(dth))<tol: break
    return mu,math.exp(th),it
a=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
res={}
for e in ex:
    if e['family']!='cauchy' or e['mode']!='double': continue
    x=list(a[int(e['case'])]); st=(mp.mpf(e['star']['location']),mp.mpf(e['star']['scale']))
    print(e['case'],'monotone line search, tol 1e-10'); fit(x,1e-10,0,verbose=True)
    for slack in (0.0,64*eps):
        m,s,it=fit(x,1e-10,slack)
        res.setdefault(e['case'],{})['slack=%g'%slack]=dict(iters=it,loc_err_over_scale=float(abs(m-st[0])/st[1]),scale_rel_err=float(abs(s-st[1])/st[1]))
        print(e['case'],'slack',slack,res[e['case']]['slack=%g'%slack])
json.dump(res,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_cauchy2.json','w'),indent=1)
