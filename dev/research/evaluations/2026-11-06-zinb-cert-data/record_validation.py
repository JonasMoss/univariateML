"""Collect only the final green runs; retain preliminary logs separately."""
import hashlib,json,re
from pathlib import Path
base=Path(__file__).resolve().parent
names=['cpp-dev','cpp-no-long-double','cpp-full','r-dev','r-full','js-build','js-test','site-check','oracle','timing','independent']
records=[]
for name in names:
 record=json.loads((base/(name+'-run.json')).read_text());assert record['exit_code']==0,(name,record)
 record['name']=name;record['log']=name+'.txt'
 log=(base/(name+'.txt')).read_text()
 if name.startswith('cpp-'):
  n=re.findall(r'100% tests passed out of (\d+)',log);assert n,(name,'missing ctest summary')
  record['tests_passed']=int(n[-1]);record['ctest_seconds']=float(re.findall(r'Total Test time \(real\) =\s*([0-9.]+) sec',log)[-1])
 if name.startswith('r-'):
  rows=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',log);assert rows,(name,'missing testthat summary')
  fail,warn,skip,passed=map(int,rows[-1]);assert fail==warn==0,(name,rows[-1]);assert name!='r-full' or skip==0
  record['tests']=dict(fail=fail,warn=warn,skip=skip,passed=passed)
 if name=='js-test':
  record['tests']={key:int(re.findall(r'# '+key+r' (\d+)',log)[-1]) for key in ['tests','pass','fail','skipped']}
  assert record['tests']['fail']==record['tests']['skipped']==0
 records.append(record)
html=(base.parents[3]/'site/dist/distributions/zinb/index.html').read_text()
assert '<math' in html and not re.search(r'<script[^>]*mathjax',html,re.I)
summary=dict(mathml_elements=html.count('<math'),environment=json.loads((base/'environment.json').read_text()),source_manifest_sha256=hashlib.sha256((base/'source-sha256.txt').read_bytes()).hexdigest(),runs=records,scalar=json.loads((base/'oracle.json').read_text())['summary'],joint=json.loads((base/'global-oracle.json').read_text())['summary'],retries=json.loads((base/'global-retries.json').read_text()),timing=json.loads((base/'timing.json').read_text()))
(base/'validation.json').write_text(json.dumps(summary,indent=2)+'\n')
print(json.dumps([dict(name=r['name'],machine=r['machine'],elapsed_seconds=r['elapsed_seconds'],tests=r.get('tests',r.get('tests_passed'))) for r in records],indent=2))
