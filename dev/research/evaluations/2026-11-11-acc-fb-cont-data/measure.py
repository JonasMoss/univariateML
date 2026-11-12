"""Measure every public coefficient against the independent reference cache."""
from pathlib import Path
import collections,json,math,os
import mpmath as mp
P=Path(__file__).resolve().parent
mp.mp.dps=100
after_stem=os.environ.get('UMVL_MEASURE_AFTER','current');output_prefix=os.environ.get('UMVL_MEASURE_OUTPUT','')
cases={str(c['index']):c for c in json.loads((P/'cases.json').read_text())}
spot_cases={str(c['index']):c for c in json.loads((P/'spot-cases.json').read_text())}
cases.update(spot_cases)
def rows(name):
 out={}
 for line in (P/(name+'.tsv')).read_text().splitlines():
  r=line.split('\t');r[2]='unflagged' if r[1]=='sn' and not r[2].startswith('stop:') else r[2]
  out[r[0]]=r
 return out
def coef(row):return {k:mp.mpf(float(v)) for k,v in (s.split('=',1) for s in row[4:] if not s.startswith('message='))}
references={}
for file in ['probe3-native-references.json','probe6-native-references.json','census-before-native-references.json','focused-latest-native-references.json','special-references.json','bhatt-critical-width-references.json','bhatt-held-sigma-critical-references.json','coupled-transport-references.json','moderate-gamma-references.json','current-native-references.json',after_stem+'-native-references.json']:
 if not (P/file).exists():continue
 for r in json.loads((P/file).read_text()):
  c=cases.get(r['id'],{});held=c.get('held',{})
  refuted_normal=c.get('family')=='bhatt' and held.get('a') is None and held.get('sigma') is None and float(r.get('normal_H4_sum','1'))<0
  if 'reference' in r and not refuted_normal:references[r['id']]=r
# Closed normal member, exact binary64 arithmetic.
references['route-bhatt-normal-inclusion']={'reference':dict(mu='0',sigma='2',a='0'),'type':'closed form'}
scalar=json.loads((P/'scalar-references.json').read_text())['6.00000001']
references['spot-burr-weak-curvature']={'reference':dict(shape1=scalar['shape'],shape2='1',scale=scalar['scale'])}
def reference(key):
 if key in references:return references[key]
 if key.startswith('strict-') and key[7:] in references:return references[key[7:]]
 return None
def route(c,before=None):
 f=c['family'];held=c['held'];free=lambda k:held.get(k) is None
 if f in ['burr','invburr','pareto4']:
  detail='joint' if free('shape1') and free('shape2') and free('scale') else 'held scale' if not free('scale') and free('shape1') and free('shape2') else 'held power' if not free('shape2') else 'held first shape'
 elif f in ['paralogis','invparalogis']:detail='joint' if free('scale') else 'held scale'
 elif f=='betapr':detail='unit shape' if held.get('shape1')==1 or held.get('shape2')==1 else 'joint' if free('shape1') and free('shape2') else 'conditional shape'
 elif f=='bhatt':detail='normal member' if before and coef(before).get('a',1)==0 else 'finite width'
 elif f=='ged':detail='knot/Laplace' if held.get('nu',2)<=1 else 'held shape' if not free('nu') else 'free shape'
 elif f in ['gb2','fpareto']:detail='Burr delegate' if held.get('shape3')==1 else 'inverse Burr delegate' if held.get('shape1')==1 else 'beta-prime small-power delegate' if held.get('shape2',2)<1 else 'beta-prime reverse-power delegate' if held.get('shape1') is not None else 'beta-prime power delegate'
 else:detail='parent transport' if f=='pareto3' else 'existing local accuracy'
 return f+' / '+detail
all_metrics=[];identities={};summary={}
for mode in ['native','double']:
 before=rows('before-'+mode);before.update(rows('census-before-'+mode));before.update(rows('strict-before-'+mode))
 after=rows(after_stem+'-'+mode)
 changed=[key for key,r in before.items() if r[2]=='certified' and key in after and after[key]!=r]
 identities[mode]={'certified_rows':sum(r[2]=='certified' for key,r in before.items() if key in after),'changed':changed}
 assert not changed,(mode,changed)
 records=[]
 for key,row in after.items():
  c=cases[key];old=before.get(key)
  relevant=row[2] in ['uncertified','unflagged'] or (old and old[2] in ['uncertified','unflagged'])
  if not relevant:continue
  ref=reference(key)
  entry={'id':key,'family':c['family'],'route':route(c,old),'mode':mode,'before_status':old[2] if old else None,'after_status':row[2],'tol':c.get('accuracy_tol',c.get('tol',1e-10))}
  if not ref:
   entry['reference_missing']=True;records.append(entry);continue
  theta={k:mp.mpf(v) for k,v in ref['reference'].items()}
  for label,r in [('before',old),('after',row)]:
   if not r or r[2].startswith('stop:'):continue
   estimate=coef(r);errors={};strict={}
   for k,t in theta.items():
    if c['held'].get(k) is not None:assert estimate[k]==mp.mpf(c['held'][k]),(key,k,'held coefficient changed');continue
    if k=='min':continue
    floor=theta.get('sigma',theta.get('sd',theta.get('omega',mp.mpf(1)))) if k in ['mu','mean','xi'] else mp.mpf(1) if (c['family']=='bhatt' and k=='a') or (c['family']=='sn' and k=='alpha') else mp.mpf(0)
    errors[k]=float(abs(estimate[k]-t)/max(abs(t),floor))
    strict[k]=float(abs(estimate[k]-t)/max(abs(t),floor if k in ['mu','mean','xi'] or t==0 else mp.mpf(0)))
   entry[label+'_errors']=errors;entry[label+'_max']=max(errors.values(),default=0.);entry[label+'_strict_relative_max']=max(strict.values(),default=0.)
  records.append(entry)
 all_metrics+=records
 grouped=collections.defaultdict(list)
 for r in records:grouped[r['route']].append(r)
 summary[mode]={k:dict(rows=len(v),before_max=max((r.get('before_max',0) for r in v),default=0),after_max=max((r.get('after_max',0) for r in v),default=0),stopped=sum(r['after_status'].startswith('stop:') for r in v),missing=[r['id'] for r in v if r.get('reference_missing')]) for k,v in grouped.items()}
(P/(output_prefix+'coefficient-errors.json')).write_text(json.dumps(all_metrics,indent=2)+'\n')
(P/(output_prefix+'route-errors.json')).write_text(json.dumps(summary,indent=2)+'\n')
(P/(output_prefix+'certified-current-identity.json')).write_text(json.dumps(identities,indent=2)+'\n')
failures=[(r['mode'],r['id'],r.get('after_max'),r['tol']) for r in all_metrics if r.get('after_max',0)>r['tol']]
missing=[(r['mode'],r['id']) for r in all_metrics if r.get('reference_missing') and not r['after_status'].startswith('stop:')]
print('coefficient failures',failures,'missing successful references',missing)
print(json.dumps(summary,indent=2))
assert not failures and not missing
bindings=[]
binding_setting=os.environ.get('UMVL_MEASURE_BINDINGS','1')
for name in ([] if binding_setting=='0' else ['r','wasm'] if binding_setting=='1' else binding_setting.split(',')):
 path=P/(name+'-spots.jsonl')
 if not path.exists():continue
 observed=set()
 for line in path.read_text().splitlines():
  if not line.startswith('{'):continue
  r=json.loads(line);key=str(r['id']);observed.add(key);c=cases[key];ref=reference(key)
  entry={'binding':name,'id':key,'family':c['family'],'error':r.get('error','')}
  expected=c.get('expected_stop')
  if expected and r.get('error'):
   mode='native' if name=='r' else 'double'
   old=rows('strict-before-'+mode)[key];current=rows(after_stem+'-'+mode)[key]
   assert old[2]==current[2]=='stop:'+expected['kind'],(key,'expected stop is not baseline-preserving')
   if expected['message'] in r['error']:
    entry['expected_stop']=expected['kind'];entry['actual_error']=entry['error'];entry['error']='';bindings.append(entry);continue
  if ref and r.get('estimates'):
   truth={k:mp.mpf(v) for k,v in ref['reference'].items()};errors={}
   for k,t in truth.items():
    if k=='min' or c['held'].get(k) is not None:continue
    v=mp.mpf(float(r['estimates'][k]));floor=truth.get('sigma',truth.get('sd',truth.get('omega',mp.mpf(1)))) if k in ['mu','mean','xi'] else mp.mpf(1) if (c['family']=='bhatt' and k=='a') or (c['family']=='sn' and k=='alpha') else mp.mpf(0)
    errors[k]=float(abs(v-t)/max(abs(t),floor))
   entry['errors']=errors;entry['max']=max(errors.values(),default=0)
  else:entry['reference_missing']=not bool(ref)
  bindings.append(entry)
 for missing_key in sorted(set(spot_cases)-observed):bindings.append({'binding':name,'id':missing_key,'error':'spot result missing'})
(P/(output_prefix+'binding-errors.json')).write_text(json.dumps(bindings,indent=2)+'\n')
print('binding errors',[(r['binding'],r['id'],r.get('error'),r.get('max')) for r in bindings if r.get('error') or r.get('reference_missing') or r.get('max',0)>1e-10])
assert not any(r.get('error') or r.get('reference_missing') or r.get('max',0)>1e-10 for r in bindings)
