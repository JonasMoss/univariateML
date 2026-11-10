"""High precision local EMG roots and triangular represented-endpoint losses."""
import csv,json,pathlib
import mpmath as mp
mp.mp.dps=400
HERE=pathlib.Path(__file__).resolve().parent
cases={c['index']:c for c in json.loads((HERE/'cases.json').read_text())}
refs={r['index']:r for r in json.loads((HERE/'references.json').read_text())}
after={int(r[0]):r for r in csv.reader((HERE/'after.csv').open())}
records=[]
for id in [1374,1375]:
 c=cases[id];origin=mp.mpf(min(c['data']));s=mp.mpf(c['held']['sigma']);rate=mp.mpf(c['held']['lambda']);a=s*rate
 def score(delta):
  result=mp.mpf(0)
  for v in c['data']:
   z=((mp.mpf(v)-origin)-delta)/s;t=a-z
   # Beyond 50 negative normal standard deviations the discarded ratio
   # is < exp(-1250), far below the root comparison precision used here.
   ratio=0 if t< -50 else 1/(mp.sqrt(mp.pi/2)*mp.exp(t*t/2)*mp.erfc(t/mp.sqrt(2)))
   result+=a-ratio
  return result
 lo=-100*s;hi=100*s
 assert score(lo)>0 and score(hi)<0
 for _ in range(400):
  mid=(lo+hi)/2
  if score(mid)>0:lo=mid
  else:hi=mid
 public=mp.mpf(float(after[id][4]));radius=max(abs(public-origin-lo),abs(public-origin-hi));target=mp.mpf('1e-10')*max(abs(origin+(lo+hi)/2),s)
 assert radius<=target
 records.append(dict(index=id,local_root_lower=str(lo),local_root_upper=str(hi),public_location=after[id][4],public_error_bound=str(radius),contract_target=str(target),lower_score=str(score(lo)),upper_score=str(score(hi))))
# Every new restoration stop gets an independent feasible improvement.
def triangular_ll(data,p):
 a,b,c=p
 total=mp.mpf(0)
 for v in data:
  if not a<=v<=b:return mp.ninf
  total+=mp.log(2)-mp.log(b-a)
  if v<c:total+=mp.log(v-a)-mp.log(c-a)
  if v>c:total+=mp.log(b-v)-mp.log(b-c)
 return total
for id in [8165,8168,8172,8173,8194,8213,11117,11118,11119,11122]:
 data=list(map(mp.mpf,cases[id]['data']));point=list(map(lambda v:mp.mpf(float(v)),after[id][4:7]));a,b,c=point
 reflected=c==max(data)
 assert c==min(data) or reflected
 radius=max(data)-min(data)
 z=[(c-v if reflected else v-c)/radius for v in data if v!=c]
 d=((b-c) if reflected else (c-a))/radius
 def derivative(w):return -len(data)/(w+d)+mp.fsum(1/(w-v) for v in z)-len(z)/w
 lo=mp.mpf(1);hi=mp.mpf(2)
 for _ in range(128):
  if derivative(hi)<0:break
  hi*=2
 assert derivative(hi)<0
 for _ in range(400):
  mid=(lo+hi)/2
  if derivative(mid)>0:lo=mid
  else:hi=mid
 witness=point[:]
 witness[0 if reflected else 1]=c+(-1 if reflected else 1)*radius*(lo+hi)/2
 base=triangular_ll(data,point);better=triangular_ll(data,witness);gain=better-base
 assert gain>mp.mpf('1e-25'),(id,gain)
 records.append(dict(index=id,represented_loglik=str(base),witness_loglik=str(better),gain=str(gain),witness=list(map(str,witness)),after_status=int(after[id][2]),evidence='400-digit independent scalar endpoint optimum with the other two public coefficients fixed; finite feasible improvement'))
def gev_ll(data,p):
 mu,sigma,xi=p
 result=mp.mpf(0)
 for v in data:
  t=1+xi*(v-mu)/sigma
  assert t>0
  L=mp.log(t)/xi
  result-=mp.log(sigma)+(1+xi)*L+mp.exp(-L)
 return result
for id in [2708,2772,10957]:
 data=list(map(mp.mpf,cases[id]['data']));point=list(map(lambda v:mp.mpf(float(v)),after[id][4:7]));mu,sigma,xi=point
 witness=list(map(mp.mpf,refs[id]['refined_theta'])) if id==10957 else point[:]
 if id!=10957:witness[0]=min(data)+sigma*(1-(1+xi)**(-xi))/xi
 base=gev_ll(data,point);better=gev_ll(data,witness);gain=better-base
 assert gain>mp.mpf('1e-7')*len(data),(id,gain)
 records.append(dict(index=id,represented_loglik=str(base),witness_loglik=str(better),gain=str(gain),witness=list(map(str,witness)),after_status=int(after[id][2]),evidence=('400-digit re-evaluation of the independently precision-doubled conditional root' if id==10957 else '400-digit finite feasible point placing the minimum at the one-observation density mode; not a global certificate')))
(HERE/'precision.json').write_text(json.dumps(records,indent=2)+'\n')
print('PASS: two local EMG score brackets meet the public contract; ten triangular and three GEV stops have independent finite-improvement witnesses')
