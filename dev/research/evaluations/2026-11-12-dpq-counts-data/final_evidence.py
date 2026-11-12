"""Single-slot final evidence: boundary references, fit replay, payload/scope checks."""
import collections,hashlib,json,math,os,subprocess
from pathlib import Path
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
BASE='217ab8b533cac25ccc8eba8d66eccde1b376f0b4'
def run(*args):subprocess.run(args,check=True,cwd=ROOT)
run('node','--v8-pool-size=1',str(HERE/'check_cmp_far.mjs'))
for script in ['check_cmp_far.py','check_bnb_far.py','witnesses.py']:
 run('python3',str(HERE/script))
key=lambda r:(r['family'],r['source'],r['index'])
changes=[];inherited=[]
for mode in ['native','double']:
 path=HERE/f'fits-final-bnbinom-{mode}.jsonl'
 args=['python3','dev/validation/seam-check.py','--family','bnbinom','--output',str(path)]
 if mode=='double':args+=['--no-long-double']
 run(*args)
 new=[json.loads(line)for line in path.read_text().splitlines()]
 if mode=='native':old=[json.loads(line)for line in (HERE/'fits-before.jsonl').read_text().splitlines()]
 else:old=json.loads((HERE/'fits-before-double.json').read_text())
 old={key(r):r for r in old if r['family']=='bnbinom'}
 assert len(new)==len(old)==113
 for row in new:
  before=old[key(row)]
  assert row['outcome'] in ['pass','stop','mismatch'],row
  if row['outcome']=='mismatch':
   # An unchanged baseline fitter discrepancy is an open finding, not a
   # new evaluator failure or an excuse to change the fit's status.
   assert mode=='double' and row['index']==579 and row['attribution']=='fitter',row
   assert row['parameters']==before['coefficients'] and row['reported']==before['logLik']
   assert row['dpqr_error']<1e-10
   inherited.append(dict(mode=mode,baseline=before,final=row))
  if mode=='native':
   assert row['outcome']==before['outcome']
   if row['outcome']=='pass':
    assert row['parameters']==before['parameters'] and row['reported']==before['reported']
    if row['dpqr']!=before['dpqr']:changes.append(dict(mode=mode,index=row['index'],field='density_sum',before=before['dpqr'],after=row['dpqr']))
   else:assert row['reason']==before['reason']
  else:
   assert (row['outcome'] in ['pass','mismatch'])==(before['status']=='Ok')
   if row['outcome'] in ['pass','mismatch']:assert row['parameters']==before['coefficients'] and row['reported']==before['logLik']
   else:assert row['reason']==before['reason']
for finding in inherited:
 row=finding['final'];names=['size','alpha','beta'];holds=[row['held'].get(n,0)for n in names]
 line=' '.join(map(str,['bnbinom',row['mask'],3,len(row['data']),*holds,*row['data']]))+'\n'
 exe=ROOT/'build/scratch/dpq-counts-fit-double/probe-22fd1b36752b-before-double'
 cells=subprocess.check_output([str(exe)],input=line,text=True).strip().split('\t')
 assert cells[0]=='ok'
 values=list(map(float,cells[1:]));baseline_density=math.fsum(values[4:])
 assert values[0]==row['reported'] and values[1:4]==row['parameters'] and baseline_density==row['dpqr']
 finding.update(baseline_density_sum=baseline_density,baseline_binary_sha256=hashlib.sha256(exe.read_bytes()).hexdigest())
(HERE/'fit-preservation-final-bnbinom.json').write_text(json.dumps(dict(records_per_mode=113,fit_changes=[],density_sum_changes=changes,inherited_open_seams=inherited),indent=2)+'\n')
D=ROOT/'univariateML-cpp/include/cppunivariate/distributions'
def payload(t):
 out=[]
 for cat in ['values','quantiles']:
  for r in t[cat]+[v['row']for v in t.get('excluded_'+cat,[])]:out.append(json.dumps([cat,t['parameters'][r[0]],*r[1:]]))
  for p in t.get('excluded_parameter_points',[]):
   for r in p[cat]:out.append(json.dumps([cat,p['parameters'],*r[1:]]))
 return collections.Counter(out)
receipts=[]
for f in ['bnbinom','cmp','lgser','pig','skellam']:
 p=D/f/'truth.json';current=json.loads(p.read_text())
 baseline=json.loads(subprocess.check_output(['git','show',BASE+':'+str(p.relative_to(ROOT))],text=True))
 assert payload(current)==payload(baseline)
 for k in ['sampler_points','historical_sampler_points']:assert current.get(k)==baseline.get(k)
 for name in ['mle.hpp','golden.json','truth.py']:
  q=D/f/name;blob=subprocess.check_output(['git','show',BASE+':'+str(q.relative_to(ROOT))]);assert q.read_bytes()==blob
 receipts.append(dict(family=f,numeric_payload_preserved=True,fitter_golden_generator_unchanged=True))
run('git','diff','--check')
files=subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()
manifest={p:hashlib.sha256((ROOT/p).read_bytes()).hexdigest()for p in files}
(HERE/'final-preservation.json').write_text(json.dumps(dict(baseline=BASE,families=receipts,source_sha256=manifest),indent=2)+'\n')
print('Final fit, frozen-payload and source checks passed',flush=True)
