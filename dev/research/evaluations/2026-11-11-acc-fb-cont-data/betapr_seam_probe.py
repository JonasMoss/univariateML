"""Independent held-shape beta-prime gamma-limit stress checks."""
from pathlib import Path
import json,subprocess,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=120
p=m.mpf(float(.6611055385779123));K=1+1/p
critical=(8*K+m.sqrt(64*K*K-4*(5-K)*(20-16*K)))/(2*(5-K))
results=[]
for delta in [1e-3,1e-4,1e-5,3e-6,1e-6,1e-7,1e-8]:
 x=[1.,1.,1.,1.,float(critical*(1+m.mpf(delta)))];extreme=m.mpf(x[-1]);y=[m.mpf(v)/extreme for v in x]
 def ingredients(r):
  G=m.fsum(m.log1p(r*v) for v in y)/5;H=m.fsum(r*v/(1+r*v) for v in y)/5;q=p/H-p
  return G,H,q
 def score(r):
  G,H,q=ingredients(r);return (m.digamma(q+p)-m.digamma(q)-G)/(r*r)
 mean=m.fsum(map(m.mpf,x))/5;var=m.fsum((m.mpf(v)-mean)**2 for v in x)/5
 guessq=(p+1)/(p*(var/mean**2-1/p))+2;guess=p*extreme/(guessq*mean)
 r=m.findroot(score,(guess/2,guess*2),tol=m.mpf('1e-90'),maxsteps=100)
 G,H,q=ingredients(r);scale=extreme/r
 assert r>0 and q>0
 row=dict(id='betapr-gamma-'+repr(delta),data=x,held={'shape1':float(p)},digits=120,reference=dict(shape1=m.nstr(p,95),shape2=m.nstr(q,95),scale=m.nstr(scale,95)),builds={})
 for mode in ['native','double']:
  line=' '.join(['betapr',row['id'],'5','1','1000','1e-10',*map(repr,x),'shape1',repr(float(p))])+'\n'
  out=subprocess.check_output([str(root/'build/scratch'/('acc-strict-probe-'+mode))],input=line,text=True).strip();v=out.split('\t');entry=dict(row=out,status=v[2])
  if not v[2].startswith('stop:'):
   got={a:m.mpf(float(t)) for a,t in (z.split('=',1) for z in v[4:])};entry['max_error']=max(float(abs(got[a]-truth)/abs(truth)) for a,truth in [('shape2',q),('scale',scale)])
  row['builds'][mode]=entry;print(row['id'],mode,entry,flush=True)
 results.append(row);(P/'betapr-seam-probe.json').write_text(json.dumps(results,indent=2)+'\n')
failures=[(r['id'],mode,b['max_error']) for r in results for mode,b in r['builds'].items() if b['status']=='uncertified' and b.get('max_error',0)>1e-10]
assert not failures,failures

assert all(b["status"] in ["uncertified","certified"] for r in results for b in r["builds"].values()), "A known finite stationary point was refused"
