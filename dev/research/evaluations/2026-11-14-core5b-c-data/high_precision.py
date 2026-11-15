#!/usr/bin/env python3
"""Independent 90-digit checks of corrected likelihood payloads; no coefficient fits."""
import collections,gzip,importlib.util,json,sys
from pathlib import Path
import mpmath as mp
mp.mp.dps=90
ROOT=Path(__file__).resolve().parents[4];OUT=Path(__file__).resolve().parent
sys.path.insert(0,str(ROOT/'dev/validation'))
sp=importlib.util.spec_from_file_location('fs',ROOT/'dev/validation/fit-snapshot.py');fs=importlib.util.module_from_spec(sp);sp.loader.exec_module(fs)
archive=collections.defaultdict(list)
for c in json.loads(gzip.decompress(fs.ARCHIVE.read_bytes())):archive[c['family']].append(c)
known=json.load(open(OUT/'known-before.json'))
keys=sorted({tuple(k.split('|')[1:5]) for k,v in known.items() if v['ref'] in ['TASK-214.15','TASK-214.16']})
for mode in ['native','double']:
 before=fs.load(ROOT/f'dev/research/evaluations/2026-11-14-fit-snapshot-data/{mode}.jsonl.gz')
 after=fs.load(ROOT/f'build/snapshots/{mode}')
 for key,r in after.items():
  if key[0]=='gb2' and r['result']['kind']=='limit' and r['result'].get('limit_estimates') and (before[key]['result']['kind']!='limit' or not before[key]['result'].get('limit_estimates')):keys.append((mode,*key))
keys=sorted(set(keys))
keys+= [(mode,'gb2','archive','002247') for mode in ['native','double']]
inputs={f:{(c['source'],c['case']):c for c in fs.cases(f,archive)} for f in {k[1] for k in keys}}
number=lambda s:mp.mpf(float.fromhex(s))
records=[]
for mode,f,source,case in keys:
 row=fs.load(ROOT/f'build/snapshots/{mode}')[(f,source,case)];r=row['result']
 if r['kind'] not in ['ok','limit']:
  records.append(dict(mode=mode,family=f,source=source,case=case,kind=r['kind'],reason='Unrepresentable face parameter is rejected by the core.'));continue
 x=[mp.mpf(float(v)) for v in inputs[f][(source,case)]['data']]
 if r['kind']=='ok':co={k:number(v) for k,v in r['estimates'].items()};model=f;reported=number(r['logLik'])
 else:co={k:number(v) for k,v in r['limit_estimates'].items()};model=r['model'];reported=number(r['limit_loglik'])
 if model=='gb2':
  q,a,p,b=[co[k] for k in ['shape1','shape2','shape3','scale']]
  ll=mp.fsum(mp.log(a)-mp.log(v)-mp.loggamma(p)-mp.loggamma(q)+mp.loggamma(p+q)-p*mp.log(1+(b/v)**a)-q*mp.log(1+(v/b)**a) for v in x)
 elif model=='hnorm':
  mean,scale=co['mu'],co['sigma'];ll=mp.fsum(mp.log(2/mp.pi)/2-mp.log(scale)-((v-mean)/scale)**2/2 for v in x)
 elif model=='norm':
  mean,scale=co['mean'],co['sd'];ll=mp.fsum(-mp.log(2*mp.pi)/2-mp.log(scale)-((v-mean)/scale)**2/2 for v in x)
 elif model=='rayleigh':
  scale=co['sigma'];ll=mp.fsum(mp.log(v)-2*mp.log(scale)-(v/scale)**2/2 for v in x)
 elif model=='weibull':
  shape,scale=co['shape'],co['scale'];ll=mp.fsum(mp.log(shape)-mp.log(scale)+(shape-1)*mp.log(v/scale)-(v/scale)**shape for v in x)
 elif model=='lnorm':
  mean,scale=co['meanlog'],co['sdlog'];ll=mp.fsum(-mp.log(2*mp.pi)/2-mp.log(scale)-mp.log(v)-((mp.log(v)-mean)/scale)**2/2 for v in x)
 elif model=='unavailable:double-pareto':
  scale,left,right=co['scale'],co['left'],co['right'];ll=mp.fsum(mp.log(left)+mp.log(right)-mp.log(left+right)-mp.log(v)+(left if v<scale else -right)*mp.log(v/scale) for v in x)
 elif model=='pareto':
  shape,scale=co['a'],co['b'];ll=mp.fsum(mp.log(shape)-mp.log(v)-shape*mp.log(v/scale) for v in x)
 elif model=='power':
  scale,shape=co['alpha'],co['beta'];ll=mp.fsum(mp.log(shape)-mp.log(scale)+(shape-1)*mp.log(v/scale) for v in x)
 else:raise RuntimeError((f,case,model,co))
 parameter_check={}
 if f=='gb2' and model=='unavailable:double-pareto' and 'scale' in row['held']:
  assert co['scale']==mp.mpf(row['held']['scale']),('held endpoint moved',mode,case)
 if f=='gb2' and model in ['pareto','power'] and 'scale' in row['held']:
  endpoint=co['b'] if model=='pareto' else co['alpha']
  if endpoint!=mp.mpf(row['held']['scale']):
   expected=min(x) if model=='pareto' else max(x)
   assert endpoint==expected,(mode,case,'endpoint')
   rate=co['a'] if model=='pareto' else co['beta']
   reference_rate=len(x)/mp.fsum(mp.log(v/expected) if model=='pareto' else mp.log(expected/v) for v in x)
   error=abs(rate/reference_rate-1)
   assert error<mp.mpf('1e-10'),(mode,case,'endpoint shape',error)
   parameter_check=dict(parameter_reference=mp.nstr(reference_rate,80),parameter_relative_error=float(error))
 rel=abs(ll-reported)/max(1,abs(ll))
 assert rel<mp.mpf('1e-10'),(mode,f,case,rel)
 records.append(dict(mode=mode,family=f,source=source,case=case,model=model,reference=mp.nstr(ll,80),reported=float(reported),relative_error=float(rel),**parameter_check))
(OUT/'high-precision.json').write_text(json.dumps(records,indent=2)+'\n')
print('%d likelihoods checked at 90 digits (including two unchanged GB2 nesting references); max relative error %.3g; %d unrepresentable faces rejected' % (sum('reference' in r for r in records),max(r.get('relative_error',0) for r in records),sum('reference' not in r for r in records)))
