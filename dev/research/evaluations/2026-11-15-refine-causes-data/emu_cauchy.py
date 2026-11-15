"""Emulate fit_cauchy (cauchy/mle.hpp 140-202): Newton on (mu, log scale), stop max(|dmu|,|dth|) < tol after the step is applied."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=50
eps=2.220446049250313e-16
def cost(x,mu,s):
    return len(x)*(math.log(math.pi)+math.log(s))+sum(math.log(1+((v-mu)/s)**2) for v in x)
def fit(x,tol,mode='abs'):
    n=len(x); d=sorted(x); mu=d[n//2]
    d=sorted(abs(v-mu) for v in d); th=math.log(d[n//2]+eps)
    tr=[]
    for it in range(1,501):
        inv=math.exp(-th); s1=s2=s3=s4=s5=0.0
        for v in x:
            z=(v-mu)*inv; z2=z*z; i1=1/(1+z2); i2=i1*i1
            s1+=z*i1; s2+=(z2-1)*i1; s3+=(1-z2)*i2; s4+=z*i2; s5+=z2*i2
        U=2*inv*s1; Ut=s2; Hm=2*inv*inv*s3; Hmt=4*inv*s4; Ht=4*s5
        det=Hm*Ht-Hmt*Hmt
        dmu=(Ht*U-Hmt*Ut)/det; dth=(-Hmt*U+Hm*Ut)/det
        if not (Hm>0 and det>0): dmu=U*math.exp(2*th)/n; dth=Ut/n
        old=cost(x,mu,math.exp(th)); st=1.0
        for _ in range(100):
            c=(mu+st*dmu,math.exp(th+st*dth))
            if c[1]>0 and cost(x,*c)<=old: break
            st*=.5
        dmu*=st; dth*=st; mu+=dmu; th+=dth
        tr.append((it,dmu,dth,mu,math.exp(th)))
        crit=max(abs(dmu)/(abs(mu) if mode=='rel' else 1),abs(dth)) if mode!='scaled' else max(abs(dmu)/math.exp(th),abs(dth))
        if crit<tol: break
    return mu,math.exp(th),tr
if __name__=='__main__':
    a=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    out=[]
    for e in ex:
        if e['family']!='cauchy' or e['mode']!='double': continue
        x=list(a[int(e['case'])]); ret=(float(e['est']['location']),float(e['est']['scale']))
        mu,s,tr=fit(x,1.220703125e-4)
        st=(mp.mpf(e['star']['location']),mp.mpf(e['star']['scale']))
        r=dict(case=e['case'],returned=ret,emulated=(mu,s),match=(mu==ret[0] and s==ret[1]),iters=len(tr),
               last_dmu=tr[-1][1],last_dth=tr[-1][2],trace=tr)
        for name,tol,mode in (('abs1e-10',1e-10,'abs'),('scaled1e-10',1e-10,'scaled')):
            m2,s2,t2=fit(x,tol,mode)
            r[name]=dict(iters=len(t2),loc_err_over_scale=float(abs(m2-st[0])/st[1]),scale_rel_err=float(abs(s2-st[1])/st[1]))
        r['returned_loc_err_over_scale']=float(abs(mp.mpf(ret[0])-st[0])/st[1]); r['returned_scale_rel_err']=float(abs(mp.mpf(ret[1])-st[1])/st[1])
        out.append(r); print(json.dumps({k:v for k,v in r.items() if k!='trace'},default=str)); 
        for t in tr: print('   ',t)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_cauchy.json','w'),indent=1,default=str)
