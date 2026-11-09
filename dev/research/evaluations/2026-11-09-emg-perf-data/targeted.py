"""Compile and run the EMG tests in both modes; invoke under a CPU slot."""
from pathlib import Path
import subprocess
p=Path.cwd();scratch=p/'build/scratch/emg-perf'
for mode in ['native','double']:
    flags=[] if mode=='native' else ['-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS']
    binary=scratch/f'emg-tests-{mode}'
    subprocess.run(['c++','-std=c++17','-O2',*flags,f'-DCPPUNIV_DISTRIBUTIONS_DIR="{p}/univariateML-cpp/include/cppunivariate/distributions"','-IunivariateML-cpp/include','-IunivariateML-cpp/tests','univariateML-cpp/tests/distribution_tests_main.cpp','univariateML-cpp/include/cppunivariate/distributions/emg/test.cpp','-o',str(binary)],check=True)
    subprocess.run([str(binary)],check=True)
