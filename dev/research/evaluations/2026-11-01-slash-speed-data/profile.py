"""Generate counter-only header and driver under ignored scratch; no production instrumentation."""
from pathlib import Path
p=Path('dev/research/evaluations/2026-11-01-slash-speed-data')
s=(p/'baseline.hpp').read_text().replace('slash_baseline','slash_counted')
s=s.replace('namespace slash_counted_detail {','namespace slash_counted_detail {\ninline unsigned long long profiles=0, steps=0, visits=0, old_exp=0, new_exp=0, densities=0, grids=0;')
s=s.replace('double s=held;','++profiles; double s=held;')
s=s.replace('const double ss=std::exp(mid);','++steps; const double ss=std::exp(mid);')
s=s.replace('score+=t<1e-7', '++visits; old_exp+=(t>=1e-7&&t<=700); new_exp+=(t>=1e-7&&t<42); score+=t<1e-7')
s=s.replace('double ll=0;for(double v:x)ll+=', 'double ll=0;densities+=x.size();for(double v:x)ll+=')
s=s.replace('SlashResult best=p(grid.front());','slash_counted_detail::grids=grid.size(); SlashResult best=p(grid.front());')
Path('build/scratch/slash-counted.hpp').write_text(s)
Path('build/scratch/slash-profile.cpp').write_text('''#include "cppunivariate/distributions/slash/mle.hpp"
#include "slash-counted.hpp"
#include "json_loader.hpp"
#include <iostream>
int main(){const auto g=mltest::load_golden("univariateML-cpp/include/cppunivariate/distributions/slash/golden.json");
std::cout<<"mask,grid,profiles,scale_steps,score_visits,old_expm1,new_expm1,densities\\n";
for(int mask=0;mask<4;++mask){namespace c=cppunivariate::slash_counted_detail;
c::grids=c::profiles=c::steps=c::visits=c::old_exp=c::new_exp=c::densities=0;
cppunivariate::slash_counted_mle({g.data.data(),g.data.size()},1e-9,200,mask&2?g.estimates.at("sigma"):cppunivariate::special::kNaN,mask&1?g.estimates.at("mu"):cppunivariate::special::kNaN);
std::cout<<mask<<','<<c::grids<<','<<c::profiles<<','<<c::steps<<','<<c::visits<<','<<c::old_exp<<','<<c::new_exp<<','<<c::densities<<'\\n';}}
''')
