"""Reproduce the untouched Burr tail failures without modifier headers."""
from pathlib import Path
import subprocess,json,hashlib
root=Path.cwd();scratch=root/'build/scratch';src=scratch/'burr-baseline.cpp'
paths=['distributions/burr/dpqr.hpp','distributions/invburr/dpqr.hpp','special/tails.hpp','distributions/burr/test.cpp','distributions/invburr/test.cpp']
checks=[]
for rel in paths:
 p=root/'univariateML-cpp/include/cppunivariate'/rel
 base=subprocess.check_output(['git','show','fe17e3b8:'+str(p.relative_to(root))])
 assert p.read_bytes()==base,rel
 checks.append(dict(path=str(p.relative_to(root)),sha256=hashlib.sha256(base).hexdigest()))
src.write_text(r'''
#include "cppunivariate/special/special.hpp"
#include "cppunivariate/distributions/burr/dpqr.hpp"
#include "cppunivariate/distributions/invburr/dpqr.hpp"
#include <iostream>
#include <iomanip>
int main() {
 std::cout<<std::setprecision(17);
 std::cout<<cppunivariate::distributions::pburr(1e100,1e-308,1e308,1,false,true)<<"\n";
 std::cout<<cppunivariate::distributions::pinvburr(1e-100,1e-308,1e308,1,true,true)<<"\n";
 volatile double shape1=1e-308,shape2=1e308;
 std::cout<<cppunivariate::distributions::pburr(1e100,shape1,shape2,1,false,true)<<"\n";
}
''')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
rows=[]
for opt in ['-O0','-O2']:
 exe=scratch/('burr-baseline'+opt)
 subprocess.run(['c++','-std=c++17',opt,'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(src),'-o',str(exe)],check=True)
 rows.append(dict(optimization=opt,output=subprocess.check_output([str(exe)],text=True).splitlines()))
native_tests=[]
for family in ['burr','invburr']:
 driver=scratch/(family+'-baseline-test.cpp');exe=scratch/(family+'-baseline-test')
 driver.write_text('#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN\n#include "doctest.h"\n#include "'+str(root/'univariateML-cpp/include/cppunivariate/distributions'/family/'test.cpp')+'"\n')
 subprocess.run(['c++','-std=c++17','-O2','-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"',
  '-I'+str(root/'univariateML-cpp/tests'),'-I'+str(root/'univariateML-cpp/include'),'-I'+boost,str(driver),'-o',str(exe)],check=True)
 result=subprocess.run([str(exe),'--no-colors','--test-case=*deep tails*'],stdout=subprocess.PIPE,stderr=subprocess.STDOUT,text=True)
 native_tests.append(dict(family=family,optimization='-O2',exit=result.returncode,output=result.stdout))
record=dict(baseline='fe17e3b8',unchanged=checks,runs=rows,native_tests=native_tests)
(root/'dev/research/evaluations/2026-11-12-modifiers-2-data/optimized-baseline.json').write_text(json.dumps(record,indent=2)+'\n')
print(json.dumps(rows))
