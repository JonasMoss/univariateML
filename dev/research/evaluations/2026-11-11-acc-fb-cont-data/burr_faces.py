"""Independent 420-digit closure witnesses; no global exclusion claim."""
from pathlib import Path
import json,mpmath as m
P=Path(__file__).resolve().parent;m.mp.dps=420
cases=[c for c in json.loads((P/'cases.json').read_text()) if 'nearly-constant' in str(c['index'])]
before={r[0]:r for r in [line.split('\t') for line in (P/'before-native.tsv').read_text().splitlines()]};out=[]
for c in cases:
 x=list(map(m.mpf,c['data']));inverse=c['family']=='invburr'
 y=[1/v for v in x] if inverse else x
 L=list(map(m.log,y));n=len(y);center=m.fsum(L)/n;sd=m.sqrt(m.fsum((v-center)**2 for v in L)/n);z=[(v-center)/sd for v in L]
 def score(C):
  w=[m.exp(C*t) for t in z]
  return 1/C-m.fsum(t*v for t,v in zip(z,w))/m.fsum(w)
 C=m.findroot(score,(m.mpf(1),m.mpf(2)),tol=m.mpf('1e-350'));power=C/sd;M=m.fsum(m.exp(C*t) for t in z)/n;scale=m.exp(center+m.log(M)/power)
 w=[m.exp(C*t)/M for t in z];A=m.fsum(t*t for t in w)/2-n
 jac=-2*m.fsum(m.log(v) for v in x) if inverse else 0
 W=n*(m.log(power)-1)-m.fsum(L)-n*m.log(M)+jac
 a={k:m.mpf(float(v)) for k,v in (item.split('=') for item in before[c['index']][4:])};k,b,s=a['shape1'],a['shape2'],a['scale'];s=1/s if inverse else s
 ll=m.fsum(m.log(k*b)-m.log(v)-m.log1p(m.exp(-b*m.log(v/s)))-k*m.log1p(m.exp(b*m.log(v/s))) for v in y)+jac
 result=dict(id=c['index'],digits=420,weibull_power=m.nstr(power,80),weibull_scale=m.nstr(scale,80),weibull_loglik=m.nstr(W,80),profile_seam_coefficient=m.nstr(A,80),returned_point_loglik=m.nstr(ll,80),face_minus_returned=m.nstr(W-ll,80),claim='Independent feasible closure witness and signed local tail coefficient, not a global cover.')
 out.append(result);print(result,flush=True)
(P/'burr-face-evidence.json').write_text(json.dumps(out,indent=2)+'\n')
