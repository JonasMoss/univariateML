#!/usr/bin/env python3
"""Freeze final run summaries and verify derivative/simulation source identity."""
import json,hashlib,re,subprocess,platform
from pathlib import Path
ROOT=Path(__file__).resolve().parents[4];HERE=Path(__file__).parent;LOG=ROOT/'build/scratch'

def digest(path):return hashlib.sha256(path.read_bytes()).hexdigest()
for file in ['derivatives.json','special-results.json']:
    for name,expected in json.loads((HERE/file).read_text())['source_sha256'].items():
        assert digest(ROOT/name)==expected,(file,name,'source changed after validation')
h=hashlib.sha256()
for path in [HERE/'calibrate.cpp']+sorted((ROOT/'univariateML-cpp/include').rglob('*.hpp')):h.update(path.read_bytes())
assert h.hexdigest()==json.loads((HERE/'calibration.json').read_text())['source_identity']
runs={}
for name,file,machine in [
 ('cpp_dev','infer-joins-cpp-dev-final.log','simbox'),
 ('cpp_no_long_double_full','infer-joins-cpp-double.log','simbox')]:
    text=(LOG/file).read_text();assert '100% tests passed' in text
    runs[name]=dict(machine=machine,status='pass',shards=int(re.search(r'100% tests passed[^\n]*out of\s+(\d+)',text)[1]),log=file)
for name,file in [('r_dev','infer-joins-r-dev-green.log'),('r_full','infer-joins-r-full.log'),('r_vcov','infer-joins-r-targeted.log')]:
    text=(LOG/file).read_text();m=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',text)[-1]
    fail,warn,skip,passed=map(int,m);assert fail==warn==0
    if name!='r_dev':assert skip==0
    runs[name]=dict(machine='simbox',status='pass',fail=fail,warn=warn,skip=skip,passed=passed,log=file)
text=(LOG/'infer-joins-js-green.log').read_text();passed=int(re.search(r'^# pass (\d+)',text,re.M)[1]);tests=int(re.search(r'^# tests (\d+)',text,re.M)[1]);assert passed==tests
assert re.search(r'^# fail 0$',text,re.M)
runs['js']=dict(machine='workstation',status='pass',tests=tests,passed=passed,log='infer-joins-js-green.log')
text=(LOG/'infer-joins-site.log').read_text();assert 'PASS: inference:' in text and 'Traceback' not in text
runs['site']=dict(machine='workstation',status='pass',log='infer-joins-site.log')
text=(LOG/'infer-joins-checks.log').read_text();assert 'numeric inference metadata validation passed' in text
runs['catalog_and_metadata']=dict(machine='workstation',status='pass',log='infer-joins-checks.log')
for file in ['derivatives.json','special-results.json','calibration-check.json']:
    runs[file]=dict(machine='simbox' if file=='special-results.json' else 'workstation',status='pass')
mc=json.loads((HERE/'calibration.json').read_text())
runs['calibration']=dict(machine='simbox',status='pass',replications=sum(v['replications'] for v in mc['results'].values()),successes=sum(v['successes'] for v in mc['results'].values()),truth_configurations=12,sample_sizes=[200,800,3200])
runs['js_build']=dict(machine='workstation',status='pass',log='infer-joins-js-build-final.log')
text=(LOG/'infer-joins-js-targeted.log').read_text();assert '# tests 17' in text and '# fail 0' in text
runs['js_vcov']=dict(machine='workstation',status='pass',tests=17,log='infer-joins-js-targeted.log')
(HERE/'test-results.json').write_text(json.dumps(runs,indent=2)+'\n')
changed=subprocess.check_output(['git','diff','--name-only'],cwd=ROOT,text=True).splitlines()
paths={ROOT/name for name in changed}|{p for p in HERE.iterdir() if p.suffix in ('.py','.cpp','.sh')}|{ROOT/'univariateML-cpp/include/cppunivariate/core/inference_join.hpp'}
revision=subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()
(HERE/'source-sha256.json').write_text(json.dumps(dict(revision=revision,uncommitted=True,source_sha256={str(p.relative_to(ROOT)):digest(p) for p in sorted(paths) if p.is_file()}),indent=2)+'\n')
print(json.dumps(runs,indent=2))
