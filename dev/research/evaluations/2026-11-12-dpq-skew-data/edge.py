"""Uniform-limit bounds at exact binary64 physical inputs, 400 digits."""
import study,mpmath as mp,json,math
mp.mp.dps=400
jobs=[];refs=[]
nu=float.fromhex('0x1.fffffffffffffp+1023');lam=mp.sqrt(3)
for f in ['ged','sged']:
 for mean,sd in [(0.,1.),(1.,1.),(-1e8,1e8)]:
  for x in [mean+sd*math.sqrt(3),mean-sd*math.sqrt(3)]:
   pars=[mean,sd,nu]+([2.] if f=='sged' else [])
   z=(mp.mpf(x)-mean)/sd
   # Corrections to the uniform-edge scale are O(1/nu), <1e-307.
   # The sampled distance to that edge is >= 1e-17.
   inside=abs(z)<lam;ld=-mp.log(2*lam*sd) if inside else -mp.inf
   low=mp.log((z+lam)/(2*lam)) if inside else (mp.mpf(0) if z>0 else -mp.inf)
   up=mp.log((lam-z)/(2*lam)) if inside else (mp.mpf(0) if z<0 else -mp.inf)
   for op,lo,b in [(0,1,ld),(1,1,low),(1,0,up)]:jobs.append([f,op,x,lo,1,pars]);refs.append(float(b))
rows=[]
for j,v,b in zip(jobs,study.evaluate(jobs,'native',kind='probe'),refs):
 a=float(v);e=0 if a==b else abs(a-b)/max(1,abs(b)) if math.isfinite(a) and math.isfinite(b) else math.inf
 rows.append(dict(job=j,actual=a,expected=b,error=e))
(study.H/'edge.json').write_text(json.dumps(rows,indent=2)+'\n')
print(len(rows),'edge checks',max(r['error'] for r in rows));print([r for r in rows if r['error']>1e-10])
