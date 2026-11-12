"""Compile and run the complete current tbinom doctests in both float modes."""
from pathlib import Path
import json,subprocess
root=Path.cwd();dest=Path(__file__).resolve().parent;scratch=root/'build/scratch'
source=scratch/'tbinom-family-only.cpp'
source.write_text('#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN\n#include "doctest.h"\n#include "'+str(root/'univariateML-cpp/include/cppunivariate/distributions/tbinom/test.cpp')+'"\n')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True).strip()
includes=['-I'+str(root/'univariateML-cpp/include'),'-I'+str(root/'univariateML-cpp/tests'),'-I'+boost]
runs=[]
for mode,flags in [('standard',[]),('no-long-double',['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS'])]:
    exe=scratch/('tbinom-tests-'+mode)
    subprocess.run(['c++','-std=c++17','-O2',*flags,*includes,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"',str(source),'-o',str(exe)],check=True)
    test=subprocess.run([str(exe),'--no-colors','--no-path-filenames'],text=True,stdout=subprocess.PIPE,stderr=subprocess.STDOUT)
    runs.append(dict(mode=mode,exit=test.returncode,summary=test.stdout.splitlines()[-5:]))
    assert test.returncode==0,test.stdout
    print(mode,'complete tbinom doctests passed',flush=True)
(dest/'tbinom-doctests.json').write_text(json.dumps(runs,indent=2)+'\n')
