"""Paired seeded study: four reserved workers, hashed/resumable build checkpoints.

Run through run4 or simbox-run; no installations. Each fit worker has one thread.
All observations are generated before any fit, and paired sample hashes must match.
"""
import collections,hashlib,json,math,re,shutil,subprocess,time
from pathlib import Path
root=Path.cwd();data=root/'dev/research/evaluations/2026-11-09-genbeta-allfree-data';scratch=root/'build/scratch'
family=root/'univariateML-cpp/include/cppunivariate/distributions/genbeta'
source=(data/'seeded-study.cpp').read_text()
baseline=re.sub(r'#include "([^"]+)"',lambda m:'#include "'+str((family/m[1]).resolve())+'"',(data/'mle-baseline.hpp.txt').read_text())
(scratch/'study-baseline.hpp').write_text(baseline)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
compiler=subprocess.check_output(['c++','--version'],text=True)
launcher=['ccache','c++'] if shutil.which('ccache') else ['c++']
allrows={};execution={};compile_flags={}
header_hashes={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in family.glob('*.hpp')}
keys=['index','model','n','p','q','a','seed','status','certified','certificate_reason','work','milliseconds','loglik','fitted_p','fitted_q','fitted_a','sample_hash']
for label,header in [('legacy',scratch/'study-baseline.hpp'),('current',family/'mle.hpp')]:
 src=scratch/('study-'+label+'.cpp');exe=scratch/('study-'+label)
 src.write_text(source.replace('HEADER','"'+str(header)+'"'))
 flags=['-O2','-std=c++17','-pthread',*(['-DLEGACY'] if label=='legacy' else [])]
 compile_flags[label]=flags
 subprocess.run([*launcher,*flags,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src),'-o',str(exe)],check=True)
 raw=scratch/('seeded-'+label+'.txt');signature=scratch/('seeded-'+label+'.sha256')
 fingerprint=hashlib.sha256((src.read_text()+json.dumps(header_hashes,sort_keys=True)+compiler+json.dumps(flags)).encode()).hexdigest()
 if not signature.exists() or signature.read_text()!=fingerprint:
  raw.write_text('');signature.write_text(fingerprint)
 complete=[];indices=set()
 for line in raw.read_text().splitlines(keepends=True):
  if not line.endswith('\n'):break
  try:values=list(map(float,line.split()))
  except ValueError:break
  if len(values)!=len(keys) or values[0]!=int(values[0]) or not 0<=values[0]<220 or int(values[0]) in indices:break
  complete.append(line);indices.add(int(values[0]))
 raw.write_text(''.join(complete))
 start=time.monotonic()
 with raw.open('a') as f:
  subprocess.run([str(exe)],input=str(len(indices))+' '+' '.join(map(str,sorted(indices)))+'\n',text=True,stdout=f,check=True)
 execution[label]=dict(workers=4,resumed_rows=len(indices),invocation_wall_seconds=time.monotonic()-start,compilation_excluded=True)
 rows=[]
 for line in raw.read_text().splitlines():
  row=dict(zip(keys,map(float,line.split())))
  rows.append({k:v if math.isfinite(v) else None for k,v in row.items()})
 rows.sort(key=lambda r:r['index'])
 assert [r['index'] for r in rows]==list(range(220))
 allrows[label]=rows
changes=[]
for before,after in zip(allrows['legacy'],allrows['current']):
 assert all(before[k]==after[k] for k in ['index','model','n','p','q','a','seed','sample_hash'])
 after['legacy_status']=before['status']
 if before['status']!=after['status']:
  changes.append(dict(input={k:after[k] for k in ['index','model','n','p','q','a','seed','sample_hash']},before=before['status'],after=after['status'],certified=after['certified'],reason=after['certificate_reason']))
def category(r):
 if r['certified']:
  assert r['status'] in (0,1,2)
  return {0:'certified_finite',1:'certified_gamma',2:'certified_power'}[r['status']]
 if r['status'] in (0,1,2):return 'uncertified_legacy'
 return 'proved_rejection' if r.get('legacy_status',r['status']) in (0,1,2) else 'legacy_failure'
def summarize(rows):
 counts=collections.Counter(category(r) for r in rows)
 for k in ['certified_finite','certified_gamma','certified_power','uncertified_legacy','proved_rejection','legacy_failure']:counts.setdefault(k,0)
 ms=sorted(r['milliseconds'] for r in rows)
 return dict(total=len(rows),counts=dict(counts),fractions={k:v/len(rows) for k,v in counts.items()},mean_ms=sum(ms)/len(ms),median_ms=(ms[(len(ms)-1)//2]+ms[len(ms)//2])/2,max_ms=max(ms),sum_fit_seconds=sum(ms)/1000)
summary={str((model,n)):summarize([r for r in allrows['current'] if r['model']==model and r['n']==n]) for model in [0,1] for n in [50,300]}
summary['all']=summarize(allrows['current']);summary['legacy']=summarize(allrows['legacy'])
out=dict(header_sha256=header_hashes,compiler=compiler.splitlines()[0],compile_flags=compile_flags,workers=4,execution=execution,
 seed_rule='mt19937_64(1165700+seed), seed=0..4; libstdc++ gamma_distribution; rounded support endpoints redrawn',
 models={'0':'genbeta: independent gamma(p),gamma(q), x=exp(-log1p(gq/gp)/a)','1':'gamma-like: Y=gamma(p,1)/p, x=exp(-Y)'},
 summary=summary,status_changes=changes,rows=allrows,source_sha256=hashlib.sha256(source.encode()).hexdigest())
(data/'seeded-study.json').write_text(json.dumps(out,indent=2)+'\n')
print(json.dumps(out,indent=2))
