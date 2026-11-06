"""Freeze successful final-run evidence after all foreground commands finish."""
import hashlib,json,re,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
scratch=root/'build/scratch'
remote=(scratch/'final-simbox.log').read_text()
local=(scratch/'final-local.log').read_text()
commands=[]
for suite,result in [('cpp-test','106/106'),('test','FAIL 0, WARN 0, SKIP 154, PASS 20821'),('cpp-test-no-long-double','127/127'),('cpp-full','127/127'),('test-full','FAIL 0, WARN 0, SKIP 0, PASS 43637')]:
 assert 'END '+suite+' ' in remote,suite
 command='UMVL_TEST_TIER=full just cpp-test' if suite=='cpp-full' else 'just '+suite
 commands.append(dict(command=command,machine='simbox',result=result,exit_code=0))
for suite in ['js-build','js-test','site-check','relations']:
 assert 'END '+suite+' ' in local,suite
 commands.append(dict(command='just '+suite,machine='workstation',exit_code=0))
assert '[ FAIL 0 | WARN 0 | SKIP 154 | PASS 20821 ]' in remote
assert '[ FAIL 0 | WARN 0 | SKIP 0 | PASS 43637 ]' in remote
assert remote.count('100% tests passed out of 127')==2
js=(scratch/'t-close-js-test.log').read_text()
assert re.search(r'# fail 0\b',js),js[-1000:]
commands[-3]['result']=re.search(r'# pass (\d+)',js).group(1)+' passed; '+re.search(r'# skipped (\d+)',js).group(1)+' skipped'
rows=json.loads((here/'replay.json').read_text());assert len(rows)==68 and all(r['status']<=5 for r in rows)
ind=json.loads((here/'independent.json').read_text());assert len(ind)==42 and all(r['independent_root_inside'] for r in ind)
arith=json.loads((here/'arithmetic.json').read_text());assert len(arith)==39 and all(r['passed'] for r in arith)
assert json.loads((here/'quantile-root.json').read_text())['both_native_boxes_enclose_root']
benchmark=json.loads((here/'benchmark-final.json').read_text())
for row in benchmark:
 if row['version']=='after':assert row['status']==(0 if row['budget']==4096 else 6),row
sources={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in [root/name for name in subprocess.check_output(['git','diff','--name-only'],text=True).splitlines()]}
expected={'bindings/js/test.mjs','bindings/r/distributions/std/test.R','univariateML-cpp/include/cppunivariate/distributions/lst/certificate.hpp','univariateML-cpp/include/cppunivariate/distributions/lst/test.cpp','univariateML-cpp/include/cppunivariate/distributions/std/test.cpp'}
assert set(sources)==expected,sources
snapshot=json.loads((here/'environment.json').read_text())
for name,digest in snapshot['source'].items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
for family in ['lst','std']:
 name=f'univariateML-cpp/include/cppunivariate/distributions/{family}/golden.json'
 assert (root/name).read_bytes()==subprocess.check_output(['git','show','HEAD:'+name])
logs={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in scratch.glob('*.log') if p.name in ['final-simbox.log','final-local.log','native-final.log','arithmetic-final.log','quantile-root.log','independent-final.log','environment.log'] or p.name.startswith('t-close-')}
remote_logs={name:digest for digest,name in re.findall(r'(?m)^([a-f0-9]{64})  (build/scratch/[^\n]+\.log)$',remote)}
result=dict(task='TASK-140.1',base_revision=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),branch=subprocess.check_output(['git','branch','--show-current'],text=True).strip(),source_sha256=sources,verified_simbox_source_count=len(snapshot['source']),commands=commands,retained_replay=dict(outcomes=68,prior_certified_preserved=60,prior_budgets_resolved=2,additional_finite_cases=6),independent_boxes=42,arithmetic_boxes=39,arithmetic_sampled_points=sum(r['points'] for r in arith),goldens_unchanged=True,permitted_final_failures=[],local_log_sha256=logs,simbox_log_sha256=remote_logs,earlier_nonpassing_attempts='search-history.json')
(here/'validation.json').write_text(json.dumps(result,indent=2)+'\n')
print(json.dumps({k:v for k,v in result.items() if k in ['commands','retained_replay','independent_boxes','arithmetic_sampled_points','goldens_unchanged']},indent=2))
