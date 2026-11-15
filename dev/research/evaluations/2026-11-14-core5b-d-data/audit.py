"""Read-only Git audit of job scope, frozen coefficients, and appended tests."""
import json,pathlib,subprocess
D=pathlib.Path(__file__).resolve().parent
BASE='da2174a11af7d31c47434a54a65d4108c6b00388'
def git(*args):return subprocess.check_output(['git','--no-optional-locks',*args])
assert git('rev-parse','HEAD').decode().strip()==BASE
files=git('diff','--name-only').decode().splitlines()
for f in files:
 assert not any(part in f for part in ['/core/','/special/','/dpqr.hpp','/truth.','/golden.json','bindings/r/scripts/codegen','bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md','dev/roadmap/','bindings/js/']),f
 assert not f.startswith('bindings/common/') or f.startswith('bindings/common/distributions/') or f=='bindings/common/modifiers.hpp',f
untracked=git("ls-files","--others","--exclude-standard").decode().splitlines()
assert all(f.startswith("dev/research/evaluations/2026-11-14-core5b-d-data/") or f in {"univariateML-cpp/include/cppunivariate/distributions/pois/held_window.hpp","univariateML-cpp/include/cppunivariate/distributions/binom/held_window.hpp","dev/research/evaluations/2026-11-14-core5b-d-report.md"} for f in untracked),untracked
checks=[]
for f in files:
 if f.endswith('/test.cpp') or f.endswith('/test.R'):
  old=git('show',BASE+':'+f)
  assert pathlib.Path(f).read_bytes().startswith(old),f
  checks.append(f)
 if f.endswith('/README.md'):
  old=git('show',BASE+':'+f).decode();new=pathlib.Path(f).read_text()
  if old.startswith('---'):assert old.split('---',2)[1]==new.split('---',2)[1],f
assert b'overridden(' not in b'\n'.join(pathlib.Path(f).read_bytes() for f in files if f.startswith('bindings/common/distributions/') and f.endswith('fit.hpp'))
(D/'scope-audit.json').write_text(json.dumps(dict(baseline=BASE,head_unchanged=True,forbidden_files_unchanged=True,tests_append_only=checks,dossier_front_matter_unchanged=True,tracked_modified_files=files,new_files=untracked),indent=2)+'\n')
print('Scope audit passed; original tests and dossier front matter preserved.')
