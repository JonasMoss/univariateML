#!/usr/bin/env python3
"""Inspect the unchanged baseline's exact swap-equality failure; no tolerances changed."""
from pathlib import Path
import json
import subprocess

root = Path.cwd()
scratch = root / 'build/scratch'
scratch.mkdir(parents=True, exist_ok=True)
source = scratch / 'probe-bnbinom-swap.cpp'
source.write_text(r'''
#include "cppunivariate/distributions/bnbinom/mle.hpp"
#include <iostream>
#include <iomanip>
#include <vector>
#include <cmath>
int main() {
  std::vector<int> x;
  int value;
  while(std::cin >> value) x.push_back(value);
  for(double held: {1., 5., 1.0672234787479615, 5.3647338351087397}) {
    const auto a = cppunivariate::bnbinom_mle({x.data(), x.size()}, 1e-8, 600, held, NAN, NAN);
    const auto b = cppunivariate::bnbinom_mle({x.data(), x.size()}, 1e-8, 600, NAN, NAN, held);
    std::cout << std::setprecision(17) << "held=" << held
              << " a=" << a.logLik << " b=" << b.logLik
              << " difference=" << a.logLik-b.logLik
              << " exact=" << (a.logLik==b.logLik) << '\n';
  }
}
''')
boost = subprocess.check_output(['Rscript','-e','cat(system.file("include", package="BH"))'],text=True).strip()
subprocess.run(['c++','-std=c++17','-O0','-DBOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS',
                '-IunivariateML-cpp/include','-I'+boost,str(source),'-o',str(scratch/'probe-bnbinom-swap')],check=True)
data = json.loads((root/'univariateML-cpp/include/cppunivariate/distributions/bnbinom/golden.json').read_text())['data']
subprocess.run([str(scratch/'probe-bnbinom-swap')],input=' '.join(str(int(x)) for x in data),text=True,check=True)
