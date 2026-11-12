#!/usr/bin/env python3
"""Reconcile completed censuses/retries and record every status/point change."""
import collections,csv,gzip,json,struct
from pathlib import Path
root=Path(__file__).resolve().parents[4];folder=Path(__file__).resolve().parent;scratch=root/'build/scratch/seam-rerun'
key=lambda r:(r['family'],r['source'],r['index'])
fit=lambda r:r['outcome'] in ('pass','mismatch')
def read(path):
 text=gzip.decompress(path.read_bytes()).decode() if path.suffix=='.gz' else path.read_text()
 return [json.loads(l) for l in text.splitlines()]
def counts(rows):return dict(collections.Counter(r['outcome'] for r in rows))
def bits(values):return [struct.pack('!d',v).hex() for v in values]
def census(phase,mode):
 rows={key(r):r for r in read(scratch/f'{phase}-{mode}.jsonl')}
 assert len(rows)==11285,(phase,mode,len(rows))
 for prefix in ('retry','retry2','retry3','late'):
  p=scratch/f'{prefix}-{phase}-{mode}.jsonl'
  if p.exists():rows.update({key(r):r for r in read(p)})
 return rows
oldfolder=root/'dev/research/evaluations/2026-11-11-seam-check-data'
summary={}
for mode in ('native','double'):
 before=census('before',mode);after=census('after',mode)
 drift=[];likelihood=[];goldens=[];reason_changes=[]
 for k,a in after.items():
  b=before[k]
  if fit(a) and fit(b):
   if bits(a['parameters'])!=bits(b['parameters']):drift.append(dict(key=k,before=b['parameters'],after=a['parameters']))
   if a['reported']!=b['reported']:
    entry=dict(key=k,before=b['reported'],after=a['reported'],delta=a['reported']-b['reported']);likelihood.append(entry)
    if a['source']=='golden':goldens.append(entry)
  elif a['outcome']!=b['outcome']:drift.append(dict(key=k,before=b['outcome'],after=a['outcome']))
  if a['outcome']=='stop' and b['outcome']=='stop' and a['reason']!=b['reason']:reason_changes.append(dict(key=k,before=b['reason'],after=a['reason']))
 # Changes to mismatch/pass from this job are repairs, not fit-status changes.
 # Record every coarse outcome change separately from coefficient/stop changes.
 summary[mode]=dict(before=counts(before.values()),after=counts(after.values()),parameters_or_terminal_status_changes=drift,stop_reason_changes=reason_changes,likelihood_changes=likelihood,golden_likelihood_changes=goldens,remaining=[r for r in after.values() if r['outcome']=='mismatch'],open=[r for r in after.values() if r['outcome']=='open fit'],outside_support=[r for r in after.values() if r.get('outside_support')],by_family={f:counts(r for r in after.values() if r['family']==f) for f in sorted({r['family'] for r in after.values()})})
 for old_name in ('before','final'):
  old={key(r):r for r in read(oldfolder/f'{old_name}.jsonl.gz')}
  changes=[]
  for k,a in after.items():
   b=old.get(k)
   if b is None or a['outcome']!=b['outcome'] or (a['outcome']=='mismatch' and a.get('attribution')!=b.get('attribution')):
    changes.append(dict(family=k[0],source=k[1],index=k[2],previous=b['outcome'] if b else 'absent',current=a['outcome'],previous_attribution=b.get('attribution','') if b else '',current_attribution=a.get('attribution',''),sample=a.get('sample',''),held=json.dumps(a.get('held',{}),sort_keys=True),reason=a.get('reason','')))
  path=folder/f'status-changes-{mode}-vs-first-{old_name}.csv'
  with path.open('w',newline='') as dest:
   writer=csv.DictWriter(dest,fieldnames=['family','source','index','previous','current','previous_attribution','current_attribution','sample','held','reason']);writer.writeheader();writer.writerows(changes)
  summary[mode][f'changes_vs_first_{old_name}']=changes
 # Full reconciled rows remain machine-readable and contain exact inputs.
 with gzip.open(folder/f'final-{mode}.jsonl.gz','wt') as dest:
  for r in after.values():dest.write(json.dumps(r)+'\n')
(folder/'analysis.json').write_text(json.dumps(summary,indent=2)+'\n')
for mode,s in summary.items():
 print(mode,s['before'],'=>',s['after'])
 print('point/status drift:',len(s['parameters_or_terminal_status_changes']),'stop-reason drift:',len(s['stop_reason_changes']))
 print('golden changes:',s['golden_likelihood_changes'])
 print('changes from first final:',len(s['changes_vs_first_final']))
