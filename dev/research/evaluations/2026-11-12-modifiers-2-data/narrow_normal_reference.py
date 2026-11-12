"""250-digit conditional-normal oracle for the held-parent archive improvement."""
from pathlib import Path
from load_evidence import read_json
import json,csv
import mpmath as mp
mp.mp.dps=250
p=Path(__file__).resolve().parent;c=read_json(p/'cases.json')[371]
assert c['id']=='tnorm' and c['source']=='archive-7709'
x=[mp.mpf(v) for v in c['x']];mu=mp.mpf(c['args']['mean']);sd=mp.mpf(c['args']['sd']);a=min(x);b=max(x)
den=(mp.erf((b-mu)/sd/mp.sqrt(2))-mp.erf((a-mu)/sd/mp.sqrt(2)))/2
ref=mp.fsum(-mp.log(sd*mp.sqrt(2*mp.pi))-((v-mu)/sd)**2/2-mp.log(den) for v in x)
row=next(r for r in csv.DictReader((p/'changed-logLik.csv').open()) if r['case']=='372');got=float(row['new_logLik']);error=float(abs(mp.mpf(got)-ref))
assert error<1e-8,(got,ref,error)
(p/'narrow-normal-reference.json').write_text(json.dumps(dict(case=372,precision=250,computed=got,reference=str(ref),absolute_error=error),indent=2)+'\n')
print('Narrow held-normal window: absolute logLik error',error)
