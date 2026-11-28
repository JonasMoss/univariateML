"""Reproduce the double-only held-golden certification flags on the baseline.

Run density-grid.py first to stage the pristine headers and golden files.
"""
import json
from pathlib import Path
import subprocess

work=Path(__file__).resolve().parents[4]/'build/scratch/trgamma-grid'
include=work/'univariateML-cpp/include'
lines=['#include "cppunivariate/distributions/lst/mle.hpp"',
       '#include "cppunivariate/distributions/std/mle.hpp"',
       '#include <iostream>', '#include <vector>', 'int main() {',
       'const double nan = std::numeric_limits<double>::quiet_NaN();']
for family, method, names in [('lst','lst_mle',('mu','sigma','df')),
                               ('std','student_t_mle',('mean','sd','nu'))]:
    g=json.loads((include/f'cppunivariate/distributions/{family}/golden.json').read_text())
    x=','.join(float(v).hex() for v in g['data'])
    p=[float(g['estimates'][n]).hex() for n in names]
    lines += [f'const std::vector<double> {family}{{{x}}};']
    for mask in (1,2):
        held=','.join(p[i] if mask & (1 << i) else 'nan' for i in range(3))
        lines += [f'const auto r_{family}{mask}=cppunivariate::{method}'
                  f'(cppunivariate::span{{{family}.data(),{family}.size()}},1e-8,1000,{held});',
                  f'std::cout<<"{family} mask={mask} certified="<<r_{family}{mask}.certified<<"\\n";']
source=work/'certification.cpp'
source.write_text('\n'.join(lines+['}'])+'\n')
boost=subprocess.check_output(['Rscript','-e','cat(system.file("include",package="BH"))'],text=True)
exe=work/'certification-baseline'
subprocess.run(['ccache','c++','-std=gnu++17','-O2','-DNDEBUG',
                '-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS',
                '-I'+str(include),'-I'+boost,str(source),'-o',str(exe)],check=True)
subprocess.run([str(exe)],check=True)
