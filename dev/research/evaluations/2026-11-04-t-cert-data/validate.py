"""Collect final foreground regression results and retained evidence identities."""
import hashlib,json,platform,re,subprocess
from collections import Counter
from pathlib import Path
HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
commands={
 'cpp-dev':'/tmp/umvl-cpu/run4 just cpp-test',
 'cpp-no-long-double':'/tmp/umvl-cpu/run4 just cpp-test-no-long-double',
 'r-dev':'/tmp/umvl-cpu/run4 just test',
 'cpp-full':'/tmp/umvl-cpu/run4 env UMVL_TEST_TIER=full just cpp-test',
 'r-full':'/tmp/umvl-cpu/run4 just test-full'}
records=[]
for name,command in commands.items():
 p=ROOT/f'build/scratch/t-cert-{name}.log';text=p.read_text()
 if name.startswith('cpp'):
  matches=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',text);assert matches,name
  result={'passed':int(matches[-1]),'failed':0}
 else:
  matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text);assert matches,name
  f,w,s,pas=map(int,matches[-1]);assert f==w==0,name
  if name=='r-full':assert s==0
  result=dict(zip(['failed','warned','skipped','passed'],[f,w,s,pas]))
 records.append({'command':command,'machine':'local workstation','result':result,'log_sha256':hashlib.sha256(p.read_bytes()).hexdigest()})
results=json.loads((HERE/'results.json').read_text());baseline=json.loads((HERE/'baseline.json').read_text())
summary={}
comparisons=[]
for fam in ['lst','std']:
 times=[q[fam]['seconds'] for q in results];native=[q[fam]['mean_seconds_3_repetitions'] for q in baseline]
 summary[fam]={'outcomes':dict(Counter(q[fam]['outcome'] for q in results)),
               'median_research_seconds':sorted(times)[len(times)//2],
               'median_legacy_seconds':sorted(native)[len(native)//2],
               'max_cached_profiles':max(q[fam].get('profiles',0) for q in results)}
 for research,native in zip(results,baseline):
  r,b=research[fam],native[fam]
  if r['outcome']=='value_enclosed' and b['logLik'] is not None:
   assert b['logLik']<=r['upper']+1e-6,(research['name'],fam)
   comparisons.append({'name':research['name'],'family':fam,'native_status':b['status'],
                       'proposal_minus_native_logLik':r['proposal']['value']-b['logLik']})
local=[q for q in json.loads((HERE/'local-tail.json').read_text()) if q['outcome']=='signed_tail']
summary['local_signed_tail']={'count':len(local),'N_min':min(q['N'] for q in local),'N_max':max(q['N'] for q in local)}
global_tail=json.loads((HERE/'signed-tail.json').read_text())['rows']
summary['global_signed_tail']={'count':len(global_tail),'N_min':min(q['N'] for q in global_tail),'N_max':max(q['N'] for q in global_tail)}
independent=json.loads((HERE/'independent.json').read_text())
summary['independent']={'profile_checks':len(independent['profile_enclosures_checked']),
                        'max_abs_profile_value_error':max(q['abs_value_error'] for q in independent['profile_enclosures_checked']),
                        'max_relative_golden_error':max(q['max_relative_golden_error'] for q in independent['golden_roots']),
                        'golden_tolerance':3e-5}
paths=[p for p in HERE.iterdir() if p.is_file() and p.name!='validation.json']
for fam in ['lst','std']:
 for name in ['mle.hpp','README.md','golden.json','test.cpp']:
  paths.append(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{fam}/{name}')
for fam in ['lst','std']:
 paths.append(ROOT/f'bindings/common/distributions/{fam}/fit.hpp')
value={'revision':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip(),
       'machine':platform.platform(),
       'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
       'R_packages':subprocess.check_output(['Rscript','-e','cat(R.version.string, "; BH", as.character(packageVersion("BH")), "; Rcpp", as.character(packageVersion("Rcpp")), "; testthat", as.character(packageVersion("testthat")))'],text=True),
       'commands':records,'research_summary':summary,'native_comparisons':comparisons,
       'hashes':{str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths}}
(HERE/'validation.json').write_text(json.dumps(value,indent=2)+'\n')
print(json.dumps({'commands':records,'summary':summary},indent=2))
