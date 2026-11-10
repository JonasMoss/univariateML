"""Independent 400-digit concave endpoint profiles; exact binary64 input values.

Newton proposals use an independently differentiated objective. Concavity tangents
bound the optimum over p,q in [0,1]; root brackets certify scalar rounding.
"""
import csv, json, pathlib, math
import mpmath as mp
mp.mp.dps=400
HERE=pathlib.Path(__file__).resolve().parent
IDS=[8165,8168,8172,8173,8194,8213,11117,11118,11119,11122]
cases={c['index']:c for c in json.loads((HERE/'cases.json').read_text())}
rows={int(r[0]):r for r in csv.reader((HERE/'before-contract.csv').open())}
def ll(data,pt):
 a,b,c=pt
 if not a<b or not a<=c<=b:return mp.ninf
 ans=mp.mpf(0)
 for x in data:
  if not a<=x<=b:return mp.ninf
  ans+=mp.log(2)-mp.log(b-a)
  if x<c:
   if x==a:return mp.ninf
   ans+=mp.log(x-a)-mp.log(c-a)
  if x>c:
   if x==b:return mp.ninf
   ans+=mp.log(b-x)-mp.log(b-c)
 return ans

def profile(data,held,c):
 origin=min(data); scale=max(data)-origin
 x=[(v-origin)/scale for v in data]; m=(c-origin)/scale
 a=(held['a']-origin)/scale if 'a' in held else None
 b=(held['b']-origin)/scale if 'b' in held else None
 L=m-a if a is not None else m-min(x)
 R=b-m if b is not None else max(x)-m
 free=[i for i,v in enumerate([a,b]) if v is None and [L,R][i]>0]
 p=[mp.mpf('.5') if i in free else mp.mpf(1) for i in range(2)]
 n=len(x)
 def evaluate(p):
  w=L/p[0]+R/p[1]; A=L/p[0]**2; B=R/p[1]**2
  f=n*mp.log(2/w); g=[n*A/w,n*B/w]
  H=mp.matrix([[n*(A*A/w**2-2*A/(p[0]*w)),n*A*B/w**2],[n*A*B/w**2,n*(B*B/w**2-2*B/(p[1]*w))]])
  for z in x:
   if z==m:continue
   i=0 if z<m else 1; D=L if i==0 else R
   d=abs(m-z)/D
   t=(z-a)/L if i==0 and a is not None else ((b-z)/R if i==1 and b is not None else 1-p[i]*d)
   if t<=0:return mp.ninf,g,H
   f+=mp.log(t);g[i]-=d/t;H[i,i]-=d*d/t**2
  return f,g,H
 for it in range(600):
  f,g,H=evaluate(p)
  if not free:break
  h=mp.matrix([[H[i,j] for j in free] for i in free]); grad=mp.matrix([-g[i] for i in free]); delta=mp.lu_solve(h,grad)
  if max(abs(v) for v in delta)<mp.mpf('1e-150'):break
  step=mp.mpf(1)
  while True:
   trial=p[:]
   for i,v in zip(free,delta):trial[i]+=step*v
   if all(0<trial[i]<1 for i in free) and evaluate(trial)[0]>=f:break
   step/=2
   assert step>mp.mpf('1e-500')
  p=trial
 else:raise AssertionError('no convergence')
 f,g,H=evaluate(p)
 upper=f+sum(g[i]*((1-p[i]) if g[i]>0 else -p[i]) for i in free)
 pt=[held.get('a',origin+scale*(m-L/p[0])),held.get('b',origin+scale*(m+R/p[1])),c]
 # No round-trip for the endpoint mode.
 if a is None and L==0:pt[0]=c
 if b is None and R==0:pt[1]=c
 return dict(point=pt,loglik=ll(data,pt),upper=upper-n*mp.log(scale),iterations=it)

def around(v):
 f=float(v);return sorted(set([mp.mpf(math.nextafter(f,-math.inf)),mp.mpf(f),mp.mpf(math.nextafter(f,math.inf))]))
records=[]
for id in IDS:
 case=cases[id];data=list(map(mp.mpf,case['data']));held={k:mp.mpf(v) for k,v in case['held'].items()}
 profiles=[]
 for c in sorted(set(data)):
  r=profile(data,held,c);profiles.append(r)
 best=max(profiles,key=lambda r:r['loglik']); candidates=[]
 for r in profiles:
  a,b,c=r['point']
  # Adjacent endpoints are exhaustive for an endpoint-mode profile, whose
  # other endpoint is forced to c or held and whose scalar objective is
  # strictly concave in reciprocal width. Other modes get continuous bounds.
  choices_a=[held['a']] if 'a' in held else ([c] if a==c else around(a))
  choices_b=[held['b']] if 'b' in held else ([c] if b==c else around(b))
  for aa in choices_a:
   for bb in choices_b:candidates.append((ll(data,[aa,bb,c]),[aa,bb,c]))
 public_ll,public=max(candidates,key=lambda v:v[0])
 competitors=[r['upper'] for r in profiles if r['point'][2]!=public[2]]
 assert public[2] in [min(data),max(data)]
 assert max(competitors)<public_ll,(id,'competing mode not excluded')
 # Scores in width coordinates, and cancellation-free neighbouring gains,
 # independently evaluated at 400 digits. Concavity is in reciprocal width.
 reflected=public[2]==max(data)
 i=0 if reflected else 1
 inward=public[:];outward=public[:]
 inward[i]=mp.mpf(math.nextafter(float(public[i]),float(public[2])))
 outward[i]=mp.mpf(math.nextafter(float(public[i]),-math.inf if reflected else math.inf))
 def score(pt):
  a,b,c=pt;w=c-a if reflected else b-c;D=b-c if reflected else c-a
  z=[(c-v if reflected else v-c) for v in data if v!=c]
  return -len(data)*w/(w+D)+mp.fsum(v/(w-v) for v in z)
 assert score(inward)>0 and score(outward)<0
 assert ll(data,inward)<public_ll and ll(data,outward)<public_ll
 gap=best['loglik']-public_ll
 before_pt=[mp.mpf(float(v)) for v in rows[id][4:7]]
 row=dict(index=id,classification='public representation',continuous_point=[str(v) for v in best['point']],continuous_loglik=str(best['loglik']),continuous_upper=str(max(r['upper'] for r in profiles)),best_public_point=[float(v) for v in public],best_public_loglik=str(public_ll),public_loss=str(gap),relative_loglik_loss=str(gap/max(1,abs(best['loglik']))),before_public_point=list(map(float,before_pt)),before_public_loglik=str(ll(data,before_pt)),best_other_mode_upper=str(max(competitors)),profiles=[dict(mode=str(r['point'][2]),loglik=str(r['loglik']),upper=str(r['upper']),iterations=r['iterations']) for r in profiles])
 row.update(neighbour_score_inward=str(score(inward)),neighbour_score_outward=str(score(outward)),neighbour_gain_inward=str(ll(data,inward)-public_ll),neighbour_gain_outward=str(ll(data,outward)-public_ll),coefficient_relative_errors=[str(abs(p-q)/abs(q)) if q else str(abs(p-q)) for p,q in zip(public,best['point'])])
 records.append(row)
 print(id,'loss',mp.nstr(gap,10),'relative',mp.nstr(gap/abs(best['loglik']),8),'public unchanged',public==before_pt,flush=True)
(HERE/'reference.json').write_text(json.dumps(records,indent=2)+'\n')
