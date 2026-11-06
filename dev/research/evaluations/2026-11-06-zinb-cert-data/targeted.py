import os,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[4]
os.chdir(root)
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
subprocess.run(['ccache','c++','-std=c++17','-O2','-Wall','-Wextra','-Wpedantic','-IunivariateML-cpp/include','-IunivariateML-cpp/tests','-I'+boost,'-DCPPUNIV_DISTRIBUTIONS_DIR="'+str(root/'univariateML-cpp/include/cppunivariate/distributions')+'"','univariateML-cpp/tests/distribution_tests_main.cpp','univariateML-cpp/include/cppunivariate/distributions/zinb/test.cpp','-o','build/scratch/zinb-tests'],check=True)
subprocess.run(['build/scratch/zinb-tests','--test-case=zinb:*'],check=True)
