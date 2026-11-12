#!/usr/bin/env python3
"""Read-only Git checks: protected inputs and append-only family tests."""
import hashlib,json,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[4]
def git(*args):return subprocess.check_output(['git',*args],cwd=root)
changed=git('diff','--name-only').decode().splitlines()
protected=[]
for path in changed:
 if path in ('bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md') or path.startswith('dev/roadmap/') or Path(path).name in ('dpqr.hpp','golden.json','truth.py','truth.json') or '/special/' in path or '/scripts/codegen' in path:
  protected.append(path)
assert not protected,protected
tests=[]
for path in changed:
 if Path(path).name not in ('test.cpp','test.R'):continue
 old=git('show','HEAD:'+path);new=(root/path).read_bytes()
 assert new.startswith(old),path
 tests.append(dict(path=path,old_sha256=hashlib.sha256(old).hexdigest(),new_sha256=hashlib.sha256(new).hexdigest(),old_bytes=len(old),new_bytes=len(new),append_only=True))
subprocess.run(['git','diff','--check'],cwd=root,check=True)
result=dict(head=git('rev-parse','HEAD').decode().strip(),modified=changed,protected_paths_changed=protected,tests=tests,diff_check='pass')
Path(__file__).with_name('source-policy.json').write_text(json.dumps(result,indent=2)+'\n')
print('Protected paths unchanged;',len(tests),'append-only test files; diff check clean.')
