"""Local root-exclusion theorem, independent 90-digit comparisons."""
from pathlib import Path
import json,mpmath as m,os
P=Path(__file__).resolve().parent;m.mp.dps=90
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())};out=[]
for mode in ['native','double']:
 rows={r[0]:r for r in [line.split('\t') for line in (P/('before-'+mode+'.tsv')).read_text().splitlines()]}
 for key in ['route-burr-nearly-constant','route-invburr-nearly-constant','route-pareto4-nearly-constant']:
  c=cases[key];x=list(map(m.mpf,c['data']));n=len(x);inverse=c['family']=='invburr';logs=[(-1 if inverse else 1)*m.log(v) for v in x];span=max(logs)-min(logs)
  theta={k:m.mpf(float(v)) for k,v in (s.split('=',1) for s in rows[key][4:])};k,power=theta['shape1'],theta['shape2'];w=[m.exp(power*(v-max(logs))) for v in logs];w=[n*v/m.fsum(w) for v in w];S2=m.fsum(v*v for v in w)
  for target in [1e-10,1e-8,1.220703e-4]:
   tol=m.mpf(target);E=power*span*tol/(1-tol);upper=S2*m.exp(2*E)/2-n;minimum_k=k/(1+tol);remainder=64*n**3/minimum_k;margin=4096*m.mpf(2)**-52*n*n*(1+power*span)
   r=dict(id=key,mode=mode,tol=target,digits=90,shape1=m.nstr(k,75),shape2=m.nstr(power,75),A=m.nstr(S2/2-n,75),A_box_upper=m.nstr(upper,75),tail_remainder=m.nstr(remainder,75),floating_margin=m.nstr(margin,75),refuted=bool(minimum_k>=100*n and upper+remainder+margin<0),claim='No stationary point lies in this coefficient accuracy box; no global face winner asserted.')
   out.append(r);print('Burr local refutation',key,mode,target,r['refuted'],m.nstr(upper,10),flush=True)
temporary=P/('burr-local-refutation.tmp-'+str(os.getpid()));temporary.write_text(json.dumps(out,indent=2)+'\n');temporary.replace(P/'burr-local-refutation.json')
assert all(r['refuted'] for r in out if r['tol']<=1e-8)
