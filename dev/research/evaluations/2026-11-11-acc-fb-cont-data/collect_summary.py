"""Collect completed validation results; no fitter or test mutation."""
from pathlib import Path
import json,re,subprocess,os
P=Path(__file__).resolve().parent;root=P.parents[3];work=root/'build/scratch'
assert 'ALL FINAL COMMANDS FINISHED' in (work/'complete-validation.log').read_text(),'Final foreground validation has not finished'
def clean(file):return re.sub(r'\x1b\[[0-9;]*m','',file.read_text())
result=dict(baseline='bc8ff7be2e3bc4b9186a56fc4f2e8ee45c163714',machine=os.uname().nodename,checks=json.loads((P/'complete-validation.json').read_text())['checks'],js_site=json.loads((P/'final-js-site-checks.json').read_text())['checks'],cpp={},r={})
conflicts=[]
for key in ['cpp-dev-complete','cpp-double-complete']:
 text=clean(work/(key+'.log'));m=re.search(r'(\d+)% tests passed, (\d+) tests failed out of (\d+)',text);assert m
 failed=re.findall(r'^\s*\d+ - ([^ ]+) \(Failed\)',text,re.M)
 names=list(dict.fromkeys(re.findall(r'^TEST CASE:\s*(.*)',text,re.M)))
 result['cpp'][key]=dict(total=int(m[3]),failed=int(m[2]),passed=int(m[3])-int(m[2]),failed_shards=failed,failed_cases=names)
 assert set(failed)=={'distribution_burr','distribution_bhatt'},failed
 sections=re.split(r'(?m)^=+\s*$',text)
 conflicts.extend(section.strip() for section in sections if 'TEST CASE:' in section and ('ERROR:' in section or 'FATAL ERROR:' in section))
for key in ['r-dev-complete','r-full-complete']:
 text=clean(work/(key+'.log'));summary=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text);assert summary
 fail,warn,skip,passed=map(int,summary[-1]);errors=list(dict.fromkeys(re.findall(r"(?:Error|Failure) \('([^']+)'\): (.*)",text)))
 result['r'][key]=dict(fail=fail,warn=warn,skip=skip,passed=passed,errors=[dict(location=a,test=b) for a,b in errors]);assert fail==6 and warn==0,(key,fail,warn)
text=clean(work/'js-test-final.log');counts={key:int(value) for key,value in re.findall(r'^# (tests|pass|fail|cancelled|skipped|todo) (\d+)',text,re.M)}
result['js']=counts;result['js_failed_tests']=re.findall(r'^not ok \d+ - (.*)',text,re.M);assert counts['fail']==2,counts
for check in result['js_site']:
 if check['check']!='js-test':assert check['exit_code']==0,check
for check in result['checks']:
 if check['check'] not in ['cpp-dev-complete','cpp-double-complete','r-dev-complete','r-full-complete']:assert check['exit_code']==0,check
result['certified_identity']=json.loads((P/'certified-current-identity.json').read_text());assert all(not r['changed'] for r in result['certified_identity'].values())
a=json.loads((P/'coefficient-errors.json').read_text());result['coefficients']={}
for mode in ['native','double']:
 rows=[r for r in a if r['mode']==mode];good=[r for r in rows if not r['after_status'].startswith('stop:')];largest=max(good,key=lambda r:r.get('after_max',0))
 result['coefficients'][mode]=dict(relevant=len(rows),returned=len(good),max_error=largest['after_max'],max_error_id=largest['id'],new_stops=[dict(id=r['id'],status=r['after_status']) for r in rows if r['before_status'] in ['uncertified','unflagged'] and r['after_status'].startswith('stop:')])
 assert all(r.get('after_max',0)<=r['tol'] for r in rows)
bind=json.loads((P/'binding-errors.json').read_text());result['bindings']={}
for binding in ['r','wasm']:
 rows=[r for r in bind if r['binding']==binding];returned=[r for r in rows if 'max' in r];preserved=[r for r in rows if r.get('expected_stop')]
 result['bindings'][binding]=dict(rows=len(rows),returned=len(returned),preserved_stops=len(preserved),max_error=max((r['max'] for r in returned),default=0));assert not any(r.get('error') or r.get('reference_missing') or r.get('max',0)>1e-10 for r in rows)
result['versions']={}
for name,args in [('compiler',['g++','--version']),('R',['Rscript','--version']),('node',['node','--version']),('emscripten',['em++','--version'])]:
 try:result['versions'][name]=subprocess.check_output(args,stderr=subprocess.STDOUT,text=True).splitlines()[0]
 except FileNotFoundError:result['versions'][name]='not on default PATH; use Justfile toolchain'
(P/'validation-summary.json').write_text(json.dumps(result,indent=2)+'\n');(P/'cpp-expectation-conflicts.txt').write_text('\n\n'.join(dict.fromkeys(conflicts))+'\n')
print(json.dumps(result,indent=2))
