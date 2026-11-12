"""Record the local environment and immutable input/source fingerprints."""
import hashlib,importlib.metadata,json,os,platform,subprocess,sys
from pathlib import Path
import mpmath
HERE=Path(__file__).resolve().parent;ROOT=HERE.parents[3]
def command(args):return subprocess.check_output(args,cwd=ROOT,text=True,stderr=subprocess.STDOUT).strip()
try:scipy=importlib.metadata.version('scipy')
except importlib.metadata.PackageNotFoundError:scipy='not installed; not used'
boost=command(['Rscript','-e','cat(system.file("include",package="BH"))'])
files=[ROOT/'univariateML-cpp/include/cppunivariate/distributions/bnbinom'/f for f in ['mle.hpp','test.cpp','golden.json']]
files += [ROOT/'dev/research/evaluations/2026-11-07-fit-audit-data/cases.json.gz',*HERE.glob('*.py')]
record=dict(baseline=command(['git','rev-parse','HEAD']),machine='workstation',os=platform.platform(),os_release=Path('/etc/os-release').read_text(),compiler=command(['g++','--version']).splitlines()[0],python=sys.version,mpmath=mpmath.__version__,scipy=scipy,R=command(['Rscript','-e','cat(R.version.string,"\nBH ",as.character(packageVersion("BH")),"\nRcpp ",as.character(packageVersion("Rcpp")),sep="")']),boost_version_header=(Path(boost)/'boost/version.hpp').read_text(),node=command(['node','--version']),emscripten=command(['bash','-c','source "${EMSDK:-$HOME/emsdk}/emsdk_env.sh" >/dev/null 2>&1; em++ --version']),precision_modes={'native':'CPPUNIV_EXTENDED_REAL = long double','double':'-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS; CPPUNIV_EXTENDED_REAL = double'},replay_controls={'tol':1e-10,'maxit':600,'cpp_flags':['-std=c++17','-O2','-DNDEBUG']},reference_digits=[100,200],sha256={str(p.relative_to(ROOT)):hashlib.sha256(p.read_bytes()).hexdigest() for p in files})
(HERE/'environment.json').write_text(json.dumps(record,indent=2)+'\n')
print(record['compiler']);print(record['R']);print('mpmath',mpmath.__version__)
