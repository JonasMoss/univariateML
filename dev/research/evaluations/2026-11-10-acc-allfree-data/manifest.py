"""Record the tested source and local toolchain, using read-only Git commands."""
import hashlib,json,subprocess
from pathlib import Path
D=Path(__file__).resolve().parent
root=D.parents[3]
def output(*args):
 return subprocess.check_output(args,cwd=root,text=True).strip()
paths=output('git','diff','--name-only').splitlines()
for p in paths:
 assert not any(t in p for t in ['/dpqr.hpp','/special/','/truth.','bindings/r/scripts/codegen','bindings/r/NEWS.md','dev/STATUS.md','dev/KNOWN_ISSUES.md','dev/roadmap/','/golden.json']),p
paths+=[f'univariateML-cpp/include/cppunivariate/distributions/{f}/local_accuracy.hpp' for f in ['sstd','st','gb2']]
result={
 'base':output('git','rev-parse','HEAD'),
 'branch':output('git','branch','--show-current'),
 'machine':'workstation; simbox unavailable by task instruction',
 'compiler':output('c++','--version').splitlines()[0],
 'R_and_BH':output('Rscript','-e','cat(R.version.string,"; BH ",as.character(packageVersion("BH")),sep="")'),
 'node':output('node','--version'),
 'emscripten':(Path.home()/'emsdk/upstream/emscripten/emscripten-version.txt').read_text().strip().strip('"'),
 'source_sha256':{p:hashlib.sha256((root/p).read_bytes()).hexdigest() for p in sorted(set(paths))},
 'golden_sha256':{f:hashlib.sha256((root/f'univariateML-cpp/include/cppunivariate/distributions/{f}/golden.json').read_bytes()).hexdigest() for f in ['slash','sstd','sged','st','bnbinom','gb2']}}
(D/'manifest.json').write_text(json.dumps(result,indent=2)+'\n')
print('PASS: source hashes recorded; restricted paths and frozen golden files unchanged.')
