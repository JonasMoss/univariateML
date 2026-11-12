"""Retain actual outputs for each original witness, including 1e-303 masses."""
import json,subprocess,math,hashlib
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
source=json.loads((HERE.parent/'2026-11-11-oracle-gaps-data/open-point-final-probes.json').read_text())
jobs=[]
for r in source:
 if r['mode']=='native' and r['family'] in ['bnbinom','cmp','lgser','pig']:
  jobs.append((r['job'],r['expected']))
t=json.loads((ROOT/'univariateML-cpp/include/cppunivariate/distributions/skellam/truth.json').read_text())
for parameters,x,lower in [([1e8,1.],99629757.,True),([1.,1e8],-99629757.,False)]:
 index=t['parameters'].index(parameters)
 row=next(r for r in t['values'] if r[0]==index and r[1]==x)
 jobs += [(['skellam',0,x,True,False,parameters],math.exp(float(row[2]))),
          (['skellam',1,x,lower,False,parameters],math.exp(float(row[3 if lower else 4])))]
out=[]
for mode,name in [('native','probe'),('double','probe-double')]:
 for job,expected in jobs:
  f,op,x,lower,log,parameters=job
  folder='dpq-counts-'+f if f in ['skellam','lgser','pig','bnbinom'] else 'dpq-counts'
  exe=ROOT/'build/scratch'/folder/name
  line=' '.join([f,str(op),str(x),str(int(lower)),str(int(log)),str(len(parameters)),*map(str,parameters)])+'\n'
  actual=float(subprocess.check_output([str(exe)],input=line,text=True))
  error=abs(actual-expected)/(max(1,abs(expected)) if log else abs(expected))
  assert math.isfinite(actual) and error<=1e-10,(mode,job,actual,expected,error)
  out.append(dict(mode=mode,job=job,actual=actual,expected=expected,error=error,binary_sha256=hashlib.sha256(exe.read_bytes()).hexdigest()))
(HERE/'witnesses.json').write_text(json.dumps(out,indent=2,allow_nan=False)+'\n')
print('Witnesses:',len(out),'maximum normalized error:',max(r['error']for r in out))
