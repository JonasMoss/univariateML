"""Read-only source policy checks; record compiler and evidence provenance."""
import hashlib,json,subprocess,sys
from pathlib import Path
import mpmath
here=Path(__file__).resolve().parent;root=here.parents[3]
git=lambda *args:subprocess.check_output(['git','--no-optional-locks',*args],cwd=root,text=True)
paths=git('diff','--name-only').splitlines()
base='univariateML-cpp/include/cppunivariate/'
families={'bbinom','beta','betapr','dgamma','fpareto','gb2','lgamma','lnorm','pareto4'}
for path in paths:
 if path==base+'special/beta_tails.hpp':continue
 parts=path.split('/')
 assert path.startswith(base+'distributions/') and parts[-2] in families and parts[-1] in ['dpqr.hpp','test.cpp','README.md','truth.py','truth.json'],path
 if parts[-1]=='test.cpp':assert (root/path).read_text().startswith(git('show','217ab8b5:'+path)),path
 if parts[-1]=='README.md':
  old=git('show','217ab8b5:'+path);new=(root/path).read_text();assert old.split('---',2)[1]==new.split('---',2)[1],path
result=dict(baseline='217ab8b533cac25ccc8eba8d66eccde1b376f0b4',head=git('rev-parse','HEAD').strip(),branch=git('branch','--show-current').strip(),paths=paths,test_changes_append_only=True,front_matter_unchanged=True,fitters_and_goldens_unchanged=True,python=sys.version,mpmath=mpmath.__version__,compiler=subprocess.check_output(['c++','--version'],text=True).splitlines()[0],source_sha256={p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in paths})
wasm=root/'build/js/univariateML.wasm'
if wasm.exists():result['wasm_sha256']=hashlib.sha256(wasm.read_bytes()).hexdigest()
(here/'source-policy.json').write_text(json.dumps(result,indent=2)+'\n');print('source policy green;',len(paths),'tracked files changed')
