"""Read-only source/machine queries; write only the environment evidence JSON."""
import hashlib,json,platform,shlex,subprocess
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
remote='''import json,hashlib,platform,subprocess
from pathlib import Path
root=Path.home()/'umvl-wt/t-close'
paths=sorted(root.glob('univariateML-cpp/include/**/*.hpp'))+sorted(root.glob('univariateML-cpp/include/**/test.cpp'))+[root/'bindings/r/distributions/std/test.R',root/'bindings/js/test.mjs']
versions={}
for key,cmd in [('compiler',['c++','--version']),('R',['R','--version']),('python',['python3','--version'])]:
 versions[key]=subprocess.check_output(cmd,text=True).splitlines()[0]
versions['platform']=platform.platform()
versions['packages']=subprocess.check_output(['Rscript','-e','for(p in c("BH","Rcpp","testthat"))cat(p,as.character(packageVersion(p)),"\\n")'],text=True)
print(json.dumps(dict(source={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in paths},environment=versions)))'''
r=json.loads(subprocess.check_output(['ssh','-F','/home/jonas/.ssh/config','-o','BatchMode=yes','simbox',shlex.join(['nice','-n','10','python3','-c',remote])],text=True))
for name,digest in r['source'].items():assert hashlib.sha256((root/name).read_bytes()).hexdigest()==digest,name
r['verified_native_source_count']=len(r['source'])
r['workstation']={k:subprocess.check_output(cmd,text=True).splitlines()[0] for k,cmd in [('compiler',['c++','--version']),('R',['R','--version']),('node',['node','--version']),('python',['python3','--version'])]}
r['workstation']['platform']=platform.platform()
r['workstation']['emscripten']=json.loads((Path.home()/'emsdk/upstream/emscripten/emscripten-version.txt').read_text())
r['base_revision']=subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip()
(here/'environment.json').write_text(json.dumps(r,indent=2)+'\n')
print('Source identities match simbox:',len(r['source']))
print(json.dumps(r['environment'],indent=2))
print(json.dumps(r['workstation'],indent=2))
