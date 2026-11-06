#include "bindings/common/distributions/sged/fit.hpp"
#include <cassert>
#include <iostream>
int main() {
  const std::vector<double> x{1,1,1};
  const auto r=umvl_binding::fit_sged({x.data(),x.size()},{true,0},{},{true,.5});
  std::cout<<r.error_kind<<" "<<r.model<<"\n";
  for(const auto& p:r.limit_estimates)std::cout<<p.first<<" "<<p.second<<"\n";
  assert(r.error_kind=="limit");assert(r.model=="unavailable:reflected-half-ged");
  assert(r.limit_estimates.size()==3);
  assert(std::abs(r.limit_estimates[0].second-1)<1e-7);
  assert(std::abs(r.limit_estimates[1].second-1./6)<1e-7);
}
