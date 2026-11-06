"""Source identities and environment of the foreground native validation."""
import hashlib,json,platform,subprocess,sys
from pathlib import Path
here=Path(__file__).resolve().parent
root=here.parents[3]
def output(*args):return subprocess.check_output(args,text=True).strip()
files=list((root/'univariateML-cpp/include/cppunivariate').rglob('*.hpp'))
files+=list(here.glob('*.cpp'))+list(here.glob('*.py'))+list(here.glob('*.sh'))
files+=[here.parent/'2026-11-04-t-cert-data/inputs.json',here.parent/'2026-11-05-t-root-data/independent.json']
manifest=dict(machine=platform.node(),platform=platform.platform(),python=sys.version,compiler=output('c++','--version').splitlines()[0],R=output('R','--version').splitlines()[0],r_packages=output('Rscript','-e','cat(paste(c("BH","Rcpp","testthat"),sapply(c("BH","Rcpp","testthat"),function(p)as.character(packageVersion(p))),collapse="; "))'),compile='c++ -O2 -std=c++17, plain BH include directory',precision_digits=80,arithmetic_seed=14020261106,timing='one discarded warmup and mean of three native calls for both certificate and legacy routes',hashes={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(set(files))})
(here/'native-environment.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('Native validation manifest recorded.')
