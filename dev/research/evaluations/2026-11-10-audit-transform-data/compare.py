#!/usr/bin/env python3
"""Raw-density validation and frozen-reference comparison for the family replay."""
import csv,gzip,json,pathlib,collections
import mpmath as mp
HERE=pathlib.Path(__file__).resolve().parent
OLD=HERE.parent/'2026-11-09-fit-audit-rerun-data'
CASES=json.loads(gzip.decompress((HERE.parent/'2026-11-07-fit-audit-data/cases.json.gz').read_bytes()))
def read(phase):
 rows={}
 for row in csv.reader((HERE/f'{phase}.csv').read_text().splitlines()):
  r=dict(family=row[0],index=int(row[2]),status=int(row[3]),logLik=row[8],theta={s.split('=')[0]:s.split('=')[1] for s in row[9:]})
  rows[r['index']]=r
 return rows
from reference import NAMES,raw_ll as evaluate

def raw_ll(c,r):
 return evaluate(c['family'],list(map(mp.mpf,c['data'])),[mp.mpf(float(r['theta'][name])) for name in NAMES[c['family']]])
def main():
 mp.mp.dps=360
 before,after=read('before'),read('after');refs={r['index']:r for r in json.loads((HERE/'reference.json').read_text())}

 old={r['index']:r for r in map(json.loads,gzip.decompress((OLD/'reference.jsonl.gz').read_bytes()).splitlines())}
 summary={};details=[]
 for family in NAMES:
  indices=[i for i,r in after.items() if r['family']==family]
  d=dict(cases=len(indices),before_statuses=dict(collections.Counter(str(before[i]['status']) for i in indices)),after_statuses=dict(collections.Counter(str(after[i]['status']) for i in indices)),before_likelihood_misses=[],after_likelihood_misses=[],remaining_saved_coefficient_displacements=[],status_changes=[])
  for i in indices:
   entry=dict(index=i,family=family)
   for phase,rows in [('before',before),('after',after)]:
    r=rows[i];e=dict(status=r['status'],reported_logLik=r['logLik'],theta=r['theta'])
    if r['status']==0:
     ll=raw_ll(CASES[i],r);err=abs(mp.mpf(float(r['logLik']))-ll)/max(1,abs(ll))
     e.update(raw_logLik=mp.nstr(ll,40),likelihood_relative_error=float(err))
     if err>mp.mpf('1e-10'):d[phase+'_likelihood_misses'].append(i)
    entry[phase]=e
   if i in refs:
    ref=refs[i];r=after[i]
    if r['status']!=0:
     assert i==4283 and r['status']==3 and abs(mp.mpf(ref['logLik']))>mp.mpf(float.fromhex('0x1.fffffffffffffp+1023')), (i,r)
     entry['reference_check']=dict(stopped=True,status=r['status']);
     if before[i]['status']!=r['status']:d['status_changes'].append(i)
     details.append(entry);continue
    errors={name:float(abs(mp.mpf(float(r['theta'][name]))-mp.mpf(v))/max(mp.mpf('1e-300'),abs(mp.mpf(v)))) if mp.mpf(v) else float(abs(mp.mpf(float(r['theta'][name])))) for name,v in zip(NAMES[family],ref['theta'])}
    err=float(abs(mp.mpf(float(r['logLik']))-mp.mpf(ref['logLik']))/max(1,abs(mp.mpf(ref['logLik']))))
    strict_err=float(abs(mp.mpf(float(r['logLik']))/mp.mpf(ref['logLik'])-1)) if mp.mpf(ref['logLik']) else err
    entry['reference_check']=dict(coefficient_relative_errors=errors,logLik_relative_error=err,strict_logLik_relative_error=strict_err,raw_logLik_relative_error=entry['after']['likelihood_relative_error'],passed=max([err,strict_err,entry['after']['likelihood_relative_error']]+list(errors.values()))<=1e-10)
    if not entry['reference_check']['passed']: print('FAILED',i,entry['reference_check'],flush=True)
   elif after[i]['status']==0 and 'refined_theta' in old.get(i,{}):
    rr=old[i];names=NAMES[family];errors=[float(abs(mp.mpf(float(after[i]['theta'][k]))-mp.mpf(v))/max(abs(mp.mpf(v)),mp.mpf('1e-300'))) if mp.mpf(v) else 0 for k,v in zip(names,rr['refined_theta']) if k not in CASES[i]['held']]
    if errors and max(errors)>1e-10:d['remaining_saved_coefficient_displacements'].append(dict(index=i,error=max(errors),old_classification=rr['classification']))
   if before[i]['status']!=after[i]['status']:d['status_changes'].append(i)
   details.append(entry)
  summary[family]=d
 (HERE/'comparison.json').write_text(json.dumps(dict(summary=summary,cases=details),indent=2)+'\n')
 ranked={r['index'] for r in json.loads((HERE/'targets.json').read_text())}
 with (HERE/'transitions.csv').open('w') as stream:
  writer=csv.writer(stream);writer.writerow(['family','index','ranked_silent','before_status','after_status','before_likelihood_relative_error','after_likelihood_relative_error','independent_reference_passed','strict_reference_logLik_relative_error','correct_numerical_stop'])
  for r in details:
   ref=r.get('reference_check',{});writer.writerow([r['family'],r['index'],r['index'] in ranked,r['before']['status'],r['after']['status'],r['before'].get('likelihood_relative_error',''),r['after'].get('likelihood_relative_error',''),ref.get('passed',''),ref.get('strict_logLik_relative_error',''),ref.get('stopped',False)])
 print(json.dumps(summary,indent=2));assert all(r.get('reference_check',{}).get('passed',True) for r in details), 'independent reference check failed'
 assert all(not s['after_likelihood_misses'] for s in summary.values()), 'reported/raw likelihood mismatch'
 print('All independent reference and returned-point likelihood checks passed.')
if __name__=='__main__':main()
