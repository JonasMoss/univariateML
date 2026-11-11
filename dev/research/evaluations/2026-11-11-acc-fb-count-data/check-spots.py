"""Round-trip binary64 R/WASM checks against the independent stationary points."""
import json,pathlib,sys,mpmath as mp
sys.dont_write_bytecode=True
from oracle import solve
mp.mp.dps=80
P=pathlib.Path(__file__).resolve().parent
cs=json.loads((P/'cases.json').read_text());out={}
for file,mode in [('r-spots.json','native'),('wasm-spots.json','double')]:
 refs={r['index']:r for r in json.loads((P/f'before-{mode}-errors.json').read_text())['rows']}
 rows=[]
 for row in json.loads((P/file).read_text()):
  i=row['index'];c=cs[i];ref={k:mp.mpf(v) for k,v in refs[i]['reference'].items()};t={k:mp.mpf(float(v)) for k,v in row['estimates'].items()}
  integer_changed=c['family']!='bnbinom' and 'size' in t and t['size']!=ref['size']
  if integer_changed:ref=solve(c,t)
  if c.get('sample')=='nonidentified-Bernoulli-curve':
   mean=mp.mpf(sum(c['data']))/len(c['data']);ref['prob']=max(mean,t['prob']);ref['pi']=1-mean/ref['prob']
  for k,v in c['held'].items():assert t[k]==mp.mpf(float(v)),(file,i,k,'hold changed')
  error={k:float(abs(t[k]-v)/max(1,abs(v))) for k,v in ref.items() if k not in c['held']}
  e=max(error.values(),default=0);tol=c.get('tol',1e-10)
  rows.append(dict(index=i,family=c['family'],errors=error,max_error=e,tol=tol,
                   integer_differs_from_gcc_reference=bool(integer_changed),
                   reference={k:str(v) for k,v in ref.items()}))
  assert e<=tol,(file,rows[-1])
 out[file]=dict(machine='workstation',mode=mode,n=len(rows),max_error=max(r['max_error'] for r in rows),rows=rows)
(P/'spot-checks.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps({k:{p:v for p,v in z.items() if p!='rows'} for k,z in out.items()},indent=2))
