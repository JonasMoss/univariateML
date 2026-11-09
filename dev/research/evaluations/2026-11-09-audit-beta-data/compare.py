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
NAMES={'betapr':['shape1','shape2','scale'],'nsbeta':['shape1','shape2','min','max'],'pareto4':['shape1','shape2','scale','min']}
def raw_ll(c,r):
 # Decimal strings in the CSV represent binary64 values: recover their exact
 # binary values before high-precision evaluation, just as for frozen data.
 p=[mp.mpf(float(r['theta'][name])) for name in NAMES[c['family']]]
 x=list(map(mp.mpf,c['data']));a,b=p[:2]
 if c['family']=='betapr':
  s=p[2];return mp.fsum((a-1)*mp.log(v/s)-(a+b)*mp.log1p(v/s)-mp.log(s)-mp.log(mp.beta(a,b)) for v in x)
 if c['family']=='nsbeta':
  lo,hi=p[2:];y=[(v-lo)/(hi-lo) for v in x]
  return mp.fsum((a-1)*mp.log(v)+(b-1)*mp.log1p(-v)-mp.log(mp.beta(a,b))-mp.log(hi-lo) for v in y)
 s,lo=p[2:]
 return mp.fsum(mp.log(a)+mp.log(b)-mp.log(v-lo)-mp.log1p(mp.exp(-b*mp.log((v-lo)/s)))-a*mp.log1p(mp.exp(b*mp.log((v-lo)/s))) for v in x)
def main():
 mp.mp.dps=700
 before,after=read('before'),read('after');refs={r['index']:r for r in json.loads((HERE/'reference.json').read_text())}
 assert set(refs)=={248,249,251,252,254,255,256,257,260,5348,10791,10792,10799,11011,5666,5669,272}
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
    ref=refs[i];r=after[i]; assert r['status']==0,(i,r)
    errors={name:float(abs(mp.mpf(float(r['theta'][name]))-mp.mpf(v))/max(mp.mpf('1e-300'),abs(mp.mpf(v)))) if mp.mpf(v) else float(abs(mp.mpf(float(r['theta'][name])))) for name,v in zip(NAMES[family],ref['theta'])}
    err=float(abs(mp.mpf(float(r['logLik']))-mp.mpf(ref['logLik']))/max(1,abs(mp.mpf(ref['logLik']))))
    entry['reference_check']=dict(coefficient_relative_errors=errors,logLik_relative_error=err,raw_logLik_relative_error=entry['after']['likelihood_relative_error'],passed=max([err,entry['after']['likelihood_relative_error']]+list(errors.values()))<=1e-10)
    assert entry['reference_check']['passed'],(i,entry['reference_check'])
   elif after[i]['status']==0 and 'refined_theta' in old.get(i,{}):
    rr=old[i];names=NAMES[family];errors=[float(abs(mp.mpf(float(after[i]['theta'][k]))-mp.mpf(v))/max(abs(mp.mpf(v)),mp.mpf('1e-300'))) if mp.mpf(v) else 0 for k,v in zip(names,rr['refined_theta']) if k not in CASES[i]['held']]
    if errors and max(errors)>1e-10:d['remaining_saved_coefficient_displacements'].append(dict(index=i,error=max(errors),old_classification=rr['classification']))
   if before[i]['status']!=after[i]['status']:d['status_changes'].append(i)
   details.append(entry)
  summary[family]=d
 (HERE/'comparison.json').write_text(json.dumps(dict(summary=summary,cases=details),indent=2)+'\n')
 print(json.dumps(summary,indent=2));print('All requested regressions pass the 1e-10 coefficient and logLik checks.')
if __name__=='__main__':main()
