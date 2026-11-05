"""Parse completed suites and retain compact environment/hash evidence."""
import hashlib
import json
import platform
import re
import statistics
import subprocess
from collections import Counter
from pathlib import Path
import mpmath
import numpy
import scipy

HERE=Path(__file__).resolve().parent
ROOT=HERE.parents[3]
commands={
    'cpp-dev':'just cpp-test',
    'cpp-no-long-double':'just cpp-test-no-long-double',
    'r-dev':'just test',
    'js-build':'just js-build',
    'js-test':'just js-test',
    'cpp-full':'env UMVL_TEST_TIER=full just cpp-test',
    'r-full':'just test-full'}
records=[]
for name,command in commands.items():
    path=ROOT/f'build/scratch/t-root-{name}.log';log=path.read_text()
    if name.startswith('cpp'):
        matches=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',log)
        assert matches,name
        result={'passed':int(matches[-1]),'failed':0}
    elif name.startswith('r-'):
        matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',log)
        assert matches,name
        f,w,s,p=map(int,matches[-1]);assert f==w==0
        if name=='r-full':assert s==0
        result={'passed':p,'failed':f,'warnings':w,'skipped':s}
    elif name=='js-test':
        counts={key:int(value) for key,value in re.findall(r'^# (tests|pass|fail|skipped) (\d+)',log,re.M)}
        assert counts['fail']==0 and counts['pass']>0
        result=counts
    else:
        artifacts=[ROOT/'build/js/univariateML.mjs',ROOT/'build/js/univariateML.wasm']
        assert all(p.is_file() and p.stat().st_size>0 for p in artifacts)
        result={'completed':True,'artifacts':{p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in artifacts}}
    records.append({'command':'/tmp/umvl-cpu/run4 '+command,'machine':'local workstation','result':result,
                    'log_sha256':hashlib.sha256(path.read_bytes()).hexdigest()})

global_rows=json.loads((HERE/'global.json').read_text());roots=json.loads((HERE/'roots.json').read_text())
baseline=json.loads((HERE/'baseline.json').read_text());summary={}
for family in ['lst','std']:
    rows=[q for q in global_rows if q['family']==family]
    root_times={(q['name'],q['family']):q['seconds'] for q in roots if q['outcome']=='root_and_tube'}
    total=lambda q:q['seconds']+root_times.get((q['name'],family),0)
    golden=next(q for q in rows if q['name']==family+'-golden')
    native=next(q for q in baseline if q['name']==family+'-golden')[family]
    summary[family]={'outcomes':dict(Counter(q['outcome'] for q in rows)),
                     'max_profiles':max(q.get('profiles',0) for q in rows),
                     'median_research_seconds':statistics.median(total(q) for q in rows),
                     'median_native_seconds':statistics.median(q[family]['mean_seconds_3_repetitions'] for q in baseline),
                     'golden_research_seconds':total(golden),'golden_native_seconds':native['mean_seconds_3_repetitions']}
independent=json.loads((HERE/'independent.json').read_text())
summary['independent']={'roots':len(independent['roots_checked']),'endpoint_roots':len(independent['endpoint_roots_checked']),'global_logic':independent['global_logic_checks'],
                        'golden_boxes':independent['golden_boxes'],'budgets':independent['budget_checks']}
files=[p for p in HERE.iterdir() if p.is_file() and p.name not in ['validation.json','final-checks.json']]
for family in ['lst','std','sstd']:
    files.extend(ROOT/f'univariateML-cpp/include/cppunivariate/distributions/{family}/{name}' for name in ['README.md','mle.hpp','test.cpp','golden.json'])
files.extend([ROOT/'dev/research/evaluations/2026-11-04-t-cert-data/inputs.json',
              ROOT/'dev/research/evaluations/2026-11-04-t-cert-data/baseline.cpp'])
value={'revision':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),
       'branch':subprocess.check_output(['git','branch','--show-current'],text=True).strip(),
       'platform':platform.platform(),'python':platform.python_version(),'numpy':numpy.__version__,
       'scipy':scipy.__version__,'mpmath':mpmath.__version__,
       'node':subprocess.check_output(['node','--version'],text=True).strip(),
       'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],
       'R':subprocess.check_output(['Rscript','-e','cat(R.version.string, "; BH", as.character(packageVersion("BH")), "; Rcpp", as.character(packageVersion("Rcpp")), "; testthat", as.character(packageVersion("testthat")))'],text=True),
       'commands':records,'summary':summary,
       'hashes':{str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(files)}}
(HERE/'validation.json').write_text(json.dumps(value,indent=2)+'\n')
print(json.dumps({'commands':records,'summary':summary},indent=2))
