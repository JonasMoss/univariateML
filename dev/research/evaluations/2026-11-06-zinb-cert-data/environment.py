import hashlib,json,os,platform,subprocess,sys
from pathlib import Path
import mpmath,numpy
base=Path(__file__).resolve().parent;root=base.parents[3];os.chdir(root)
def output(command):return subprocess.check_output(command,text=True,stderr=subprocess.STDOUT).strip()
env=dict(platform=platform.platform(),python=sys.version,mpmath=mpmath.__version__,numpy=numpy.__version__,compiler=output(['c++','--version']),R=output(['Rscript','-e','cat(R.version.string,"\\nRcpp ",as.character(packageVersion("Rcpp")),"\\nBH ",as.character(packageVersion("BH")),sep="")']),node=output(['node','--version']),revision=output(['git','rev-parse','HEAD']),branch=output(['git','branch','--show-current']))
(base/'environment.json').write_text(json.dumps(env,indent=2)+'\n')
paths=[root/'univariateML-cpp/include/cppunivariate/distributions/zinb'/name for name in ['certificate.hpp','global_certificate.hpp','mle.hpp','README.md','test.cpp','golden.json','dpqr.hpp','truth.py','truth.json']]
paths += [root/'bindings/common/distributions/zinb/fit.hpp',root/'bindings/r/distributions/zinb/adapter.cpp',root/'bindings/r/distributions/zinb/test.R',root/'bindings/js/stop-test.mjs']
paths += [p for p in base.iterdir() if p.suffix in ['.py','.cpp','.sh']]
(base/'source-sha256.txt').write_text(''.join(hashlib.sha256(p.read_bytes()).hexdigest()+'  '+str(p.relative_to(root))+'\n' for p in sorted(paths)))
print(json.dumps(env,indent=2))
