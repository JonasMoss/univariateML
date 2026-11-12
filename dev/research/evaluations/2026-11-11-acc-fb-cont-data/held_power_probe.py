"""Independent 120-digit scalar profile at a weak exponential-limit seam."""
from pathlib import Path
import subprocess,math,json,mpmath as m
P=Path(__file__).resolve().parent;root=P.parents[3];m.mp.dps=120
cases=[]
for power in [2.,.5,math.pi]:
 cases.append(dict(id='burr-'+repr(power),family='burr',data=[1.,1.,1.,1.,float(6.00000001**(1/power))],power=power,shift=0.,inverse=False))
x=cases[0]['data'];cases += [dict(id='invburr-2',family='invburr',data=[1/v for v in x],power=2.,shift=0.,inverse=True),dict(id='pareto4-2',family='pareto4',data=[1+v for v in x],power=2.,shift=1.,inverse=False)]
results=[]
for c in cases:
 data=[m.mpf(v)-m.mpf(c['shift']) for v in c['data']];power=m.mpf(c['power']);extreme=min(data) if c['inverse'] else max(data)
 y=[(extreme/v if c['inverse'] else v/extreme)**power for v in data];n=len(y)
 def score(r):
  G=m.fsum(m.log1p(r*v) for v in y);H=m.fsum(r*v/(1+r*v) for v in y)
  return (n*H+G*H-n*G)/(r*r)
 r=m.findroot(score,(m.mpf('1e-9'),m.mpf('1e-8')),tol=m.mpf('1e-95'),maxsteps=100)
 assert r>0 and abs(score(r))<m.mpf('1e-90')
 G=m.fsum(m.log1p(r*v) for v in y);k=n/G;s=extreme*r**((1 if c['inverse'] else -1)/power)
 row=dict(**c,digits=120,reference=dict(shape1=m.nstr(k,95),shape2=m.nstr(power,95),scale=m.nstr(s,95)),builds={},before_builds={})
 for mode in ['native','double']:
  held=['shape2',repr(c['power'])]+(['min',repr(c['shift'])] if c['family']=='pareto4' else [])
  line=' '.join([c['family'],c['id'],str(n),str(len(held)//2),'1000','1e-10',*map(repr,c['data']),*held])+'\n'
  out=subprocess.check_output([str(root/'build/scratch/acc-parallel'/('fit-'+mode))],input=line,text=True).strip();parts=out.split('\t');entry={'row':out,'status':parts[2]}
  if not parts[2].startswith('stop:'):
   got={a:m.mpf(float(b)) for a,b in (v.split('=',1) for v in parts[4:])};errors={a:float(abs(got[a]-truth)/abs(truth)) for a,truth in [('shape1',k),('scale',s)]};entry.update(errors=errors,max_error=max(errors.values()))
  row['builds'][mode]=entry;print(c['id'],mode,entry,flush=True)
  baseline=root/'build/scratch'/('acc-strict-before-'+mode)
  if baseline.exists():
   old=subprocess.check_output([str(baseline)],input=line,text=True).strip();v=old.split('\t');b={'row':old,'status':v[2]}
   if not v[2].startswith('stop:'):
    got={a:m.mpf(float(t)) for a,t in (z.split('=',1) for z in v[4:])};b['max_error']=max(float(abs(got[a]-truth)/abs(truth)) for a,truth in [('shape1',k),('scale',s)])
   row['before_builds'][mode]=b
 results.append(row);(P/'held-power-edge-probe.json').write_text(json.dumps(results,indent=2)+'\n')
failures=[(r['id'],mode,b['max_error']) for r in results for mode,b in r['builds'].items() if b.get('max_error',0)>1e-10]
assert not failures,failures
