set -e
source build/scratch/env.sh
mkdir -p build/scratch/count-include
cp -a univariateML-cpp/include/. build/scratch/count-include/
python3 - <<'PY'
from pathlib import Path
for f in ['emg','bhatt']:
 p=Path(f'build/scratch/count-include/cppunivariate/distributions/{f}/mle.hpp')
 s=p.read_text().replace(f'namespace {f}_detail {{',f'namespace {f}_detail {{\ninline unsigned long long calls=0, misses=0;')
 s=s.replace('evaluate(const std::vector<double>& t) const {','evaluate(const std::vector<double>& t) const {\n    ++calls;')
 s=s.replace('evaluate_uncached(const std::vector<double>& t) const {','evaluate_uncached(const std::vector<double>& t) const {\n    ++misses;')
 p.write_text(s)
s=Path('dev/validation/fit-speed-comparison.cpp').read_text().replace('#include "../../univariateML-cpp/tests/json_loader.hpp"','#include "json_loader.hpp"')
for f in ['emg','bhatt']:
 needle=f'const auto r=cppunivariate::{f}_mle'
 start=s.index(needle);end=s.index('std::cout<<std::endl;',start)
 end+=len('std::cout<<std::endl;')
 s=s[:end]+f'\nstd::cout<<"counts {f} "<<mask<<" "<<cppunivariate::{f}_detail::calls<<" "<<cppunivariate::{f}_detail::misses<<std::endl;cppunivariate::{f}_detail::calls=cppunivariate::{f}_detail::misses=0;'+s[end:]
Path('build/scratch/counts.cpp').write_text(s)
PY
c++ -O2 -std=c++17 -Ibuild/scratch/count-include -IunivariateML-cpp/tests -I"$(Rscript -e 'cat(system.file("include", package="BH"))')" build/scratch/counts.cpp -o build/scratch/counts
for id in emg bhatt; do build/scratch/counts "$id" 0; done
