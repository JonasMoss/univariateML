#!/usr/bin/env python3
"""Compare captures to stored independent high-precision stationary references."""
import json,math
from pathlib import Path
import mpmath as mp
import yaml
root=Path(__file__).resolve().parents[4];here=Path(__file__).resolve().parent
mp.mp.dps=80
names={}
for id in 'sn skellam sgomp dgamma pareto3 zinb zmnbinom ztnbinom ht emg genbeta'.split():
 p=root/f'univariateML-cpp/include/cppunivariate/distributions/{id}/README.md'
 names[id]=[x['name'] for x in yaml.safe_load(p.read_text().split('---')[1])['params']]
def rows(p,key):return {r[key]:r for r in map(json.loads,p.read_text().splitlines())}
def error(est,ref,held):
 return float(max([abs(mp.mpf(est[k])-mp.mpf(v))/max(abs(mp.mpf(v)),mp.mpf('1e-300')) for k,v in ref.items() if k not in held and k in est] or [0]))
gold=[]
for id in names:
 g=json.loads((root/f'univariateML-cpp/include/cppunivariate/distributions/{id}/golden.json').read_text());ref=g['reference']['estimates'];held=g.get('fit_args',{})
 entry={'id':id,'fit_errors':{},'loglik_errors':{}}
 for p in sorted(here.glob('*-*.jsonl')):
  if not p.name.startswith(('after-','before-')):continue
  r=rows(p,'id').get(id)
  if r:
   entry[p.stem]=r['error'] or error(r['estimates'],ref,held)
   if not r['error']:
    ll=max(float(abs(mp.mpf(r['logLik'])-mp.mpf(v))/abs(mp.mpf(v))) for v in [g['logLik'],g['reference']['logLik']])
    entry['loglik_errors'][p.stem]=ll
    entry['fit_errors'][p.stem]=max(entry[p.stem],error(r['estimates'],g['estimates'],held),ll)
 after=[v for k,v in entry['fit_errors'].items() if k.startswith('after-')]
 measured=max(after)
 cushion=1.5*measured
 base=10**math.floor(math.log10(cushion)) if measured else 1
 entry['allowance']=next(base*k for k in (1,2,3,5,10) if base*k>=cushion) if measured else 0
 assert not measured or entry['allowance']<=3*measured
 gold.append(entry)
(here/'golden-errors.json').write_text(json.dumps(gold,indent=2)+'\n')
if (here/'replay-after.jsonl').exists():
 records=[];before=rows(here/'replay-before.jsonl','index');after=rows(here/'replay-after.jsonl','index')
 for entry in json.loads((here/'replay-references.json').read_text()):
  c,r=entry['case'],entry['reference'];i=c['index'];ref=dict(zip(names[c['family']],r['refined_theta']));b,a=before.get(i),after.get(i)
  if b is None or a is None:continue
  records.append(dict(index=i,id=c['family'],sample=c['sample'],held=c['held'],before=b['error'] or error(b['estimates'],ref,c['held']),after=a['error'] or error(a['estimates'],ref,c['held'])))
 (here/'replay-errors.json').write_text(json.dumps(records,indent=2)+'\n')
 from collections import Counter
 print('replay',len(records),'stops',Counter(r['id'] for r in records if isinstance(r['after'],str)),'misses',Counter(r['id'] for r in records if isinstance(r['after'],float) and r['after']>1e-10))
 for r in records:
  if isinstance(r['after'],str) or r['after']>1e-10:print(r)
print('goldens',[(r['id'],r['allowance']) for r in gold])
