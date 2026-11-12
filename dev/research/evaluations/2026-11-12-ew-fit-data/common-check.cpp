#include "common/distributions/expweibull/fit.hpp"
#include <cassert>
#include <iostream>
#include <vector>
int main() {
  std::vector<double> x;
  for(int i=0;i<100;++i)x.push_back(cppunivariate::distributions::qexpweibull((i+.5)/100,2,1.5,2.5,true,false));
  const auto data=cppunivariate::span{x.data(),x.size()};
  const auto free=umvl_binding::fit_expweibull(data);
  assert(free.error.empty() && free.estimates.size()==3 && free.extra[0].second==0);
  const auto held=umvl_binding::fit_expweibull(data,{true,2},{true,1.5},{true,2.5});
  assert(held.error.empty() && held.fixed.size()==3 && held.extra[0].second==1);
  std::vector<double> lower{1,1.5,2},tied{1,2,3,3},constant(5,2.);
  const auto power=umvl_binding::fit_expweibull({lower.data(),lower.size()},{true,3});
  assert(power.error_kind=="limit" && power.model=="power" && power.limit_estimates[0].first=="alpha" && power.limit_estimates[0].second==3);
  const auto endpoint=umvl_binding::fit_expweibull({tied.data(),tied.size()},{true,3});
  assert(endpoint.error_kind=="limit" && endpoint.model=="unavailable:power-envelope-endpoint-ties");
  assert(endpoint.candidates.size()==1 && endpoint.candidates[0].model==endpoint.model);
  assert(std::isfinite(endpoint.limit_loglik) && endpoint.loglik_lower<=endpoint.limit_loglik && endpoint.loglik_upper>=endpoint.limit_loglik);
  const auto unbounded=umvl_binding::fit_expweibull({constant.data(),constant.size()});
  assert(unbounded.error_kind=="nomle");
  const auto tight=umvl_binding::fit_expweibull(data,{true,2},{true,1.5},{},1e-30);
  assert(tight.error_kind=="tolerance" && tight.attainable_tol>0 && !tight.candidates.empty());
  assert(!umvl_binding::fit_expweibull(data,{true,0}).error.empty());
  std::cout<<"expweibull common binding checks passed\n";
}
