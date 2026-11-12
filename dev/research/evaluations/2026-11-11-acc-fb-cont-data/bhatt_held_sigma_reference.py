from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;m.mp.dps=180;results=[]
for r in json.loads((P/'bhatt-held-sigma-critical-check.json').read_text()):
 x=list(map(m.mpf,r['data']));n=len(x);H2=m.fsum(t*t-1 for t in x);Q=m.fsum((t**4-6*t*t+3)/120-(t*t-1)**2/72 for t in x);seed=m.sqrt(-H2/(12*Q))
 def raw(E):
  a=m.exp(E);g=m.mpf(0)
  for t in x:
   u=t+a;l=t-a;prob=(m.erf(u/m.sqrt(2))-m.erf(l/m.sqrt(2)))/2
   g+=a*(m.exp(-u*u/2)+m.exp(-l*l/2))/m.sqrt(2*m.pi)/prob-1
  return g
 E=m.findroot(lambda e:raw(e)/m.exp(4*e),(m.log(seed)-.01,m.log(seed)+.01),tol=m.mpf('1e-100'),maxsteps=30)
 information=-m.diff(raw,E);assert information>0
 r.update(id='strict-seed-bhatt-critical-sigma-'+repr(r['e']),family='bhatt',digits=180,reference={'mu':'0','sigma':'1','a':m.nstr(m.exp(E),140)},information_width=m.nstr(information,50))
 print(r['e'],r['reference'],flush=True);results.append(r)
held=dict(results[-1],id='strict-seed-bhatt-critical-sigma-held-mu',held={'sigma':1.,'mu':0.});held.pop('baseline',None);held.pop('builds',None);results.append(held)
(P/'bhatt-held-sigma-critical-references.json').write_text(json.dumps(results,indent=2)+'\n')
