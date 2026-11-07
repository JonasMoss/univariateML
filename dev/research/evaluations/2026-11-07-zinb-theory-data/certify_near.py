"""Sample-specific global proof checks, exact arithmetic conditional on padded signs.
No dependency on the package fitter. See report for each analytic bound.
"""
import mpmath as mp,json,argparse
from pathlib import Path
parser=argparse.ArgumentParser();parser.add_argument('--iterlim',type=int,default=1000)
iterlim=parser.parse_args().iterlim
assert iterlim>0
budget=128*min(iterlim,100000)
mp.mp.dps=65
P=Path(__file__).resolve().parent
pad=mp.mpf('1e-45')
w={1:mp.mpf(6042)/10000,2:mp.mpf(1979)/10000,3:mp.mpf(1979)/10000}
cache={}; mean_calls=0

def mean(r,t):return mp.expm1(t)/t if r==0 else r*mp.expm1(t)/(-mp.expm1(-r*t))
def theta(r):
 global mean_calls
 if r in cache:return cache[r]
 # Newton merely proposes; signed endpoints certify the unique mean root.
 guess=mp.log1p(mp.mpf(2)/(r+1))
 t=mp.findroot(lambda t:mean(r,t)-2,(guess,guess*mp.mpf('1.01')),tol=mp.mpf('1e-58'),maxsteps=min(iterlim,100000))
 lo=t-pad;hi=t+pad
 assert lo>0 and mean(r,lo)<2-pad*mp.mpf('.01') and mean(r,hi)>2+pad*mp.mpf('.01')
 mean_calls+=1;cache[r]=(lo,hi);return lo,hi

def E(z):return 1/mp.expm1(z)-1/z if z else -mp.mpf(1)/2
def J(t):return 1-t/mp.expm1(t)
def A(r):return sum(v/(r+j) for j,v in w.items())
def H(r):return sum(j*v/(r+j) for j,v in w.items())

# Origin: Q'(r)/k >= A(a)-t(0)[1+E(a*t(0))] for 0<=r<=a.
a=mp.mpf('0.01');t0=theta(mp.mpf(0))[1]
origin=A(a)-t0*(1+E(a*t0));assert origin>pad
# Compact cover of positive size score (no discarded unresolved boxes).
stack=[(a,mp.mpf(100))];accepted=[];evaluations=0
while stack:
 lo,hi=stack.pop();evaluations+=1
 assert evaluations<=budget,'certificate budget: unresolved, do not accept'
 lower=2*J(theta(hi)[0])-H(lo)
 if lower>pad:
  accepted.append((lo,hi,lower));continue
 mid=mp.sqrt(lo*hi)
 stack.extend([(mid,hi),(lo,mid)])
# Verify the sorted cover is contiguous, complete, and has strictly positive bounds.
accepted.sort();assert accepted[0][0]==a and accepted[-1][1]==100
assert all(x[1]==y[0] for x,y in zip(accepted,accepted[1:]))
# d in [0,.01], u in [1.55,1.6]. Uniform negative Hessian bounds.
k=mp.mpf(10000);S=2*k;D=mp.mpf('0.01');U=mp.mpf('1.6');V=mp.mpf('1.55');B2=mp.mpf(31769)
minus_Ldd=B2/(1+3*D)**2-S*U**2/12
minus_Luu=S/U**2-k*mp.exp(-V)/(-mp.expm1(-V))**2-S*D**2/12
cross=S/2
det=minus_Ldd*minus_Luu-cross**2
assert minus_Ldd>pad and minus_Luu>pad and det>pad
# u(d) in [1.55,1.6] for all 0<=d<=.01 by monotone mean.
def mu(d,u):return u/(-mp.expm1(-u)) if d==0 else mp.expm1(u*d)/d/(-mp.expm1(-u))
assert mu(D,V)<2-pad and mu(0,U)>2+pad
# Confirm high precision root in this box, positive inflation, and signed existence.
near=json.loads((P/'near-face.json').read_text())['runs'][-1]
d=mp.mpf(near['dispersion']);u=mp.mpf(near['t'])
assert 0<d<D and V<u<U and mp.mpf(near['pi'])>0
# Mean-profiled d score changes sign inside a narrow bracket about the candidate.
def Ld(d,u):
 v=u*d
 hp=1/mp.expm1(v)-1/v
 return sum(c*sum(mp.mpf(j)/(1+j*d) for j in range(x)) for x,c in [(1,3958),(2,4063),(4,1979)])+S*u*hp
rel=mp.mpf('1e-25');dl=d*(1-rel);dh=d*(1+rel)
# u=r*t; mean brackets above apply with r=1/d.
rlo=1/dl;rhi=1/dh
ulo,uhi=theta(rlo);vl,vh=theta(rhi)
# L_du=-S J'(u*d)<0: use upper u for lower score and vice versa.
score_left=Ld(dl,rlo*uhi);score_right=Ld(dh,rhi*vl)
assert score_left>pad and score_right<-pad
payload=dict(iterlim=iterlim,certificate_budget=budget,precision=mp.mp.dps,padding=str(pad),origin_score_lower=str(origin),compact_boxes=len(accepted),compact_evaluations=evaluations,mean_root_brackets=mean_calls,minimum_compact_F_lower=str(min(z[2] for z in accepted)),minus_Ldd_lower=str(minus_Ldd),minus_Luu_lower=str(minus_Luu),abs_Ldu_upper=str(cross),determinant_lower=str(det),u_box=[str(V),str(U)],dispersion_box=[str(dl),str(dh)],size_box=[str(1/dh),str(1/dl)],left_dispersion_score_lower=str(score_left),right_dispersion_score_upper=str(score_right),global_unique=True)
(P/'near-certificate.json').write_text(json.dumps(payload,indent=2)+'\n')
# Small full replayable cover (each endpoint is exact decimal text at this precision).
(P/'near-cover.json').write_text(json.dumps([[str(a),str(b),str(c)] for a,b,c in accepted],separators=(',',':'))+'\n')
print(json.dumps(payload,indent=2))
