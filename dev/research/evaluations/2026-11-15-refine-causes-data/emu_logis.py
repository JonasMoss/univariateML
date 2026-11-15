"""Emulate logistic_mle's Newton (logis/mle.hpp 111-190) for archive case 4689, no standardization branch (checked below)."""
import sys, json, math
sys.path.insert(0,'dev/validation')
from strict_common import archive_data
import mpmath as mp
mp.mp.dps=50
def cost(x,mu,s):
    acc=0.0
    for v in x:
        z=(v-mu)/s; az=abs(z); l=math.log1p(math.exp(-az))+(-z if z<0 else 0.0); acc+=-z-2*l
    return len(x)*math.log(s)-acc
def newton(x,tol,slack=0.0,mode='abs',maxit=100,verbose=False):
    n=len(x); mean=0.0; M2=0.0
    for i,v in enumerate(x):
        d=v-mean; mean+=d/(i+1); M2+=d*(v-mean)
    mu=mean; th=math.log(math.sqrt(M2/(n-1))*math.sqrt(3)/math.pi); tr=[]
    for it in range(maxit):
        inv=math.exp(-th); U1=U2=sc2=stz=sz2=0.0
        for v in x:
            z=(v-mu)*inv; t=math.tanh(.5*z); c2=1-t*t
            U1+=t; U2+=z*t; sc2+=c2; stz+=t+.5*z*c2; sz2+=z*z*c2
        U1*=inv; U2-=n
        J11=.5*sc2*inv*inv; J12=inv*stz; J22=(U2+n)+.5*sz2
        det=J11*J22-J12*J12
        dmu=(J22*U1-J12*U2)/det; dth=(-J12*U1+J11*U2)/det
        old=cost(x,mu,math.exp(th)); st=1.0
        for _ in range(maxit):
            c=(mu+st*dmu,math.exp(th+st*dth))
            if cost(x,*c)<=old+slack*max(abs(old),n): break
            st*=.5
        dmu*=st; dth*=st; mu+=dmu; th+=dth; tr.append((it,dmu,dth,st,mu,math.exp(th)))
        crit=max(abs(dmu)/(math.exp(th) if mode=='scaled' else 1),abs(dth))
        if crit<tol: break
    return mu,math.exp(th),tr
if __name__=='__main__':
    a=archive_data(); ex=json.load(open('dev/research/evaluations/2026-11-15-refine-causes-data/extract.json'))
    e=[e for e in ex if e['family']=='logis' and e['mode']=='double'][0]
    x=list(a[int(e['case'])]); ret=(float(e['est']['location']),float(e['est']['scale']))
    st=(mp.mpf(e['star']['location']),mp.mpf(e['star']['scale']))
    n=len(x); mean=sum(x)/n; sd=math.sqrt(sum((v-mean)**2 for v in x)/(n-1))
    print('mean',mean,'sd',sd,'1e6*sd',1e6*sd,'standardize clause 3:',1.220703125e-4<2**-40*max(abs(mean),sd))
    out=dict(case=e['case'],returned=ret)
    for name,tol,slack,mode in (('abs1.22e-4',1.220703125e-4,0,'abs'),('abs1e-10',1e-10,0,'abs'),('scaled1e-10',1e-10,0,'scaled'),('scaled1e-10+slack',1e-10,64*2.22e-16,'scaled')):
        m,s,tr=newton(x,tol,slack,mode)
        out[name]=dict(iters=len(tr),mu=m,scale=s,loc_err_abs=float(abs(m-st[0])),loc_err_over_max_loc_scale=float(abs(m-st[0])/max(abs(st[0]),st[1])),scale_rel_err=float(abs(s-st[1])/st[1]),match_returned=(m==ret[0] and s==ret[1]))
        print(name,out[name])
        if name in('abs1.22e-4','abs1e-10'):
            for t in tr: print('   ',t)
    json.dump(out,open('dev/research/evaluations/2026-11-15-refine-causes-data/emu_logis.json','w'),indent=1,default=str)
