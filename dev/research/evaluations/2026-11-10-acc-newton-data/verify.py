"""Final read-only source/data checks, then persist the verification summary.
Run through a CPU slot after all suites and captures have finished.
"""
import hashlib,json,re,subprocess
from pathlib import Path
h=Path(__file__).resolve().parent;root=h.parents[3]
state=json.loads((h/'source-state.json').read_text())
for p,digest in state.items():assert hashlib.sha256((root/p).read_bytes()).hexdigest()==digest,p
for name,row in json.loads((h/'frozen-inputs.json').read_text()).items():
 p=f'univariateML-cpp/include/cppunivariate/distributions/{name}/golden.json'
 content=(root/p).read_bytes()
 assert hashlib.sha256(content).hexdigest()==row['sha256'],p
 assert content==subprocess.check_output(['git','show',f'c60d92ea:{p}'],cwd=root),p
changed=subprocess.check_output(['git','diff','--name-only'],cwd=root,text=True).splitlines()
assert set(changed)==set(state),'tracked source set changed after freeze'
for p in changed:
 assert not any(s in p for s in ['/dpqr.hpp','/special/','/truth.','/scripts/codegen','NEWS.md','STATUS.md','KNOWN_ISSUES.md','dev/roadmap/']),p
for family in 'lomax kumar weibull gumbel fatigue cmb pig foldnorm'.split():
 p=f'univariateML-cpp/include/cppunivariate/distributions/{family}/test.cpp'
 assert (root/p).read_bytes().startswith(subprocess.check_output(['git','show',f'c60d92ea:{p}'],cwd=root)),p
for p in ['bindings/js/stop-test.mjs','bindings/r/tests/testthat/test-stops-cont.R']:
 assert (root/p).read_bytes().startswith(subprocess.check_output(['git','show',f'c60d92ea:{p}'],cwd=root)),p
measure=json.loads((h/'measurements.json').read_text());replay=json.loads((h/'replay-measurements.json').read_text())
for mode in ['after-native','after-double','after-r','after-wasm']:
 assert len(measure[mode])==8
 for family,row in measure[mode].items():assert not row['error'] and row['maximum']<=1e-10,(mode,family,row)
for mode in ['after','after-double','after-r','after-wasm']:
 assert len(replay[mode])==83
 for row in replay[mode]:assert not row['error'] and row['maximum']<=1e-10,(mode,row)
checks={}
patterns={
 'cpp-dev':r'100% tests passed, 0 tests failed out of (106)',
 'cpp-full':r'100% tests passed, 0 tests failed out of (130)',
 'cpp-no-long-double':r'100% tests passed, 0 tests failed out of (130)',
 'r-dev':r'\[ FAIL 0 \| WARN 0 \| SKIP \d+ \| PASS (\d+) \]',
 'r-full':r'\[ FAIL 0 \| WARN 0 \| SKIP 0 \| PASS (\d+) \]',
 'js-test':r'# pass (601)\s*\n# fail 0',
}
for name,pattern in patterns.items():
 path=root/'build/scratch'/('acc-closure-js-test.log' if name=='js-test' else f'acc-final-{name}.log')
 text=path.read_text();matches=list(re.finditer(pattern,text));assert matches,(name,str(path))
 checks[name]={'machine':'workstation','passed':int(matches[-1][1]),'log':path.name,'exit_code':0}
# Build/site statuses are also checked from their process exit codes by the caller.
checks['source_hashes_unchanged']=True
checks['frozen_goldens_unchanged']=True
checks['golden_modes_passed']=4
checks['audit_cases_per_mode']=83
checks['audit_modes_passed']=4
(h/'checks.json').write_text(json.dumps(checks,indent=2)+'\n')
print(json.dumps(checks,indent=2))
