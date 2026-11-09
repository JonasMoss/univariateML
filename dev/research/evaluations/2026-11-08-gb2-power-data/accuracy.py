import json
from pathlib import Path
import mpmath as m
m.mp.dps=80
root=Path(__file__).parent
references=json.loads((root/'high-precision.json').read_text())
refs={str(r['subset']):r for r in references}
rows=[]
for line in (root/'after.txt').read_text().splitlines():
 t=line.split()
 if t[0] in ('0','1'):
  r=refs[t[1]]
  rows.append(dict(family=int(t[0]),subset=int(t[1]),relative_power_error=float(abs(m.mpf(t[3])/m.mpf(r['power'])-1)),relative_scale_error=float(abs(m.mpf(t[4])/m.mpf(r['scale'])-1)),absolute_loglik_error=float(abs(m.mpf(t[5])-m.mpf(r['logLik'])))))
 elif t[0]=='mixed' and t[2] in ('6','3'):
  inverse=t[2]=='3';r=refs['mixed reciprocal' if inverse else 'mixed direct']
  rows.append(dict(family=int(t[1]),subset=t[2],relative_power_error=float(abs(m.mpf(t[5])/m.mpf(r['power'])-1)),relative_shape_error=float(abs(m.mpf(t[4 if inverse else 6])/m.mpf(r['beta_shape'])-1))))
 elif t[0]=='constant_finite':
  r=refs['constant mixed finite stationary witness']
  rows.append(dict(subset=t[0],relative_power_error=float(abs(m.mpf(t[2])/m.mpf(r['power'])-1)),relative_shape_error=float(abs(m.mpf(t[3])/m.mpf(r['p'])-1)),absolute_loglik_error=float(abs(m.mpf(t[4])-5*m.mpf(r['log_density'])))))
for r in rows:
 for k,v in r.items():
  if k.startswith('relative_'):assert v<5e-10,(k,v)
print(json.dumps(rows,indent=2))
