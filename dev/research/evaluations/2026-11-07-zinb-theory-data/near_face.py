"""Independent high precision stationary point and conditioning diagnostics."""
import mpmath as mp, json, platform
from pathlib import Path
OUT=Path(__file__).resolve().parent

def compute(dps):
 mp.mp.dps=dps
 counts={0:5000,1:3958,2:4063,4:1979}; k=10000; n=15000; S=20000
 lam=mp.findroot(lambda u:u/(-mp.expm1(-u))-2,1.6)
 # Dispersion d=1/r and t=-r log p avoid enormous cancellation.
 def L(d,u):
  return sum(c*(sum(mp.log1p(j*d) for j in range(x))+x*mp.log(-mp.expm1(-u*d)/d)-u-mp.loggamma(x+1)-mp.log(-mp.expm1(-u))) for x,c in counts.items() if x)
 def mean(d,u):return mp.expm1(u*d)/d/(-mp.expm1(-u))-2
 def score(d,u):return mp.diff(lambda z:L(z,u),d)
 d,u=mp.findroot((mean,score),(mp.mpf('0.0001'),lam),tol=mp.mpf(10)**(-dps+15))
 r=1/d;p=mp.exp(-u*d);pi=1-mp.mpf(2)/3/(-mp.expm1(-u))
 B=5000*mp.log(mp.mpf(1)/3)+10000*mp.log(mp.mpf(2)/3)
 face=B+sum(c*(x*mp.log(lam)-lam-mp.loggamma(x+1)-mp.log(-mp.expm1(-lam))) for x,c in counts.items() if x)
 h=mp.matrix([[mp.diff(lambda a:L(a,u),d,2),mp.diff(lambda a:mp.diff(lambda b:L(a,b),u),d)], [mp.diff(lambda a:mp.diff(lambda b:L(a,b),u),d),mp.diff(lambda a:L(d,a),u,2)]])
 prof_h=h[0,0]-h[0,1]**2/h[1,1]
 # Log r, log u coordinates at stationary point.
 hlog=mp.matrix([[d*d*h[0,0],-d*u*h[0,1]],[-d*u*h[1,0],u*u*h[1,1]]]); eig=mp.eigsy(-hlog,eigvals_only=True)
 def joint(a,b,c):
  rr=mp.exp(a);uu=mp.exp(b); pp=mp.exp(-uu/rr)
  return counts[0]*mp.log(c+(1-c)*mp.exp(-uu))+sum(cc*(mp.log1p(-c)+sum(mp.log(rr+j) for j in range(x))+x*mp.log1p(-pp)-uu-mp.loggamma(x+1)) for x,cc in counts.items() if x)
 theta=(mp.log(r),mp.log(u),pi)
 hj=mp.matrix(3)
 for i in range(3):
  for j in range(3):
   order=[0,0,0];order[i]+=1;order[j]+=1
   hj[i,j]=mp.diff(joint,theta,tuple(order))
 ej=mp.eigsy(-hj,eigvals_only=True)
 I=S/lam**2-k*mp.exp(-lam)/(-mp.expm1(-lam))**2
 Bface=-mp.mpf(31769)/2+S*lam**2/24+S**2/(8*I)
 out=dict(face_profile_second_d=2*Bface,asymptotic_size=-2*Bface/(mp.mpf(15937)-10000*lam),dps=dps,size=r,prob=p,pi=pi,dispersion=d,t=u,lambda_zip=lam,pi_zip=1-mp.mpf(2)/3/(-mp.expm1(-lam)),C=mp.mpf(15937)-10000*lam,logLik=B+L(d,u),zip_logLik=face,gain=B+L(d,u)-face,profile_second_d=prof_h,profile_second_logr=d*d*prof_h,score_d=score(d,u),mean_residual=mean(d,u),profile_hessian_eigenvalues=list(eig),joint_hessian_eigenvalues=list(ej),joint_condition=ej[2]/ej[0],relative_size_1pct_loss=-prof_h*(d*mp.mpf('.01'))**2/2)
 return {key:([mp.nstr(z,dps-10) for z in v] if isinstance(v,list) else mp.nstr(v,dps-10) if isinstance(v,mp.mpf) else v) for key,v in out.items()}
if __name__=='__main__':
 results=[compute(70),compute(110)]
 (OUT/'near-face.json').write_text(json.dumps(dict(python=platform.python_version(),mpmath=mp.__version__,runs=results),indent=2)+'\n')
 print(json.dumps(results[-1],indent=2))
