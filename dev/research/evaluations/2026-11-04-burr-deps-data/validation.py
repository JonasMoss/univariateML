"""Retain compact final-run evidence; execute only after all suites finish."""
import hashlib,json,platform,re,subprocess,sys
import mpmath
from pathlib import Path
out=Path(__file__).resolve().parent
records={}
for name in ['cpp-dev-final','cpp-no-long-double-final','cpp-full','r-dev-final','r-full']:
    p=Path('build/scratch')/(name+'.log');s=p.read_text()
    if name.startswith('cpp'):
        matches=re.findall(r'100% tests passed, 0 tests failed out of (\d+)',s)
        assert matches,name
        result={'passed_shards':int(matches[-1])}
    else:
        matches=re.findall(r'\[ FAIL (\d+) \| WARN (\d+) \| SKIP (\d+) \| PASS (\d+) \]',s)
        assert matches,name
        fail,warn,skip,passed=map(int,matches[-1])
        assert fail==warn==0,(name,matches[-1])
        result={'fail':fail,'warn':warn,'skip':skip,'pass':passed}
    records[name]={'machine':'local workstation','result':result,'log_sha256':hashlib.sha256(p.read_bytes()).hexdigest()}
files=[]
for family in ['invburr','pareto4']:
    files.extend(Path(f'univariateML-cpp/include/cppunivariate/distributions/{family}')/n for n in ['mle.hpp','test.cpp','README.md','golden.json'])
    files.extend([Path(f'bindings/r/distributions/{family}/test.R'),Path(f'bindings/common/distributions/{family}/fit.hpp')])
files.extend(Path('univariateML-cpp/include/cppunivariate/distributions/burr')/n for n in ['mle.hpp','certificate.hpp','certificate-fixtures.inc'])
files.extend(p for p in out.iterdir() if p.is_file() and p.name not in ['validation.json','README.md'])
manifest={str(p):hashlib.sha256(p.read_bytes()).hexdigest() for p in files}
data={'base_revision':subprocess.check_output(['git','rev-parse','HEAD'],text=True).strip(),'platform':platform.platform(),'python':sys.version,'mpmath':mpmath.__version__,'compiler':subprocess.check_output(['c++','--version'],text=True).splitlines()[0],'R':subprocess.check_output(['Rscript','-e','cat(R.version.string)'],text=True),'runs':records,'source_sha256':manifest}
(out/'validation.json').write_text(json.dumps(data,indent=2)+'\n')
print(json.dumps(records,indent=2))
