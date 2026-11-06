// Reproduce under a single CPU slot; see the final report for the command.
#include "bindings/common/distributions/emg/fit.hpp"
#include "bindings/common/distributions/bhatt/fit.hpp"
#include "bindings/common/distributions/ged/fit.hpp"
#include "bindings/common/distributions/genbeta/fit.hpp"
#include "bindings/common/distributions/huber/fit.hpp"
#include "bindings/common/distributions/sn/fit.hpp"
#include <cassert>
#include <iostream>
inline void record(const char* family,const umvl_binding::FitResult& r) {
  if(r.error_kind=="tie") {
    assert(!r.candidates.empty());assert(r.limit_estimates.empty());
    assert(std::isinf(r.loglik_upper));
    for(const auto& c:r.candidates) {
      assert(!c.estimates.empty());assert(std::isfinite(c.loglik_lower));
      assert(c.loglik_lower<=r.loglik_lower+1e-7*(1+std::abs(r.loglik_lower)));
    }
  }
  std::cout << family << " " << (r.error_kind.empty()?"ordinary-or-success":r.error_kind)
            << " " << r.candidates.size() << '\n';
}
int main() {
  const std::vector<double> x{1,2};const cppunivariate::span s{x.data(),x.size()};
  record("emg",umvl_binding::fit_emg(s,{true,0},{true,1e-10}));
  record("bhatt",umvl_binding::fit_bhatt(s,{true,0},{true,1e-10}));
  record("ged",umvl_binding::fit_ged(s,{true,0},{true,1e-10}));
  record("huber",umvl_binding::fit_huber(s,{true,0},{true,1e-10}));
  const auto skew_normal=umvl_binding::fit_sn(s,{true,0},{true,1e-10});
  record("sn",skew_normal);
  assert(skew_normal.error_kind=="limit");assert(skew_normal.candidates.size()==1);
  assert(skew_normal.model=="hnorm");assert(skew_normal.limit_estimates.size()==2);
  assert(skew_normal.candidates[0].model=="hnorm");
  assert(skew_normal.candidates[0].estimates[0].second==0);
  assert(skew_normal.candidates[0].estimates[1].second==1e-10);
  for(const auto& c:skew_normal.candidates)assert(std::abs(c.loglik_lower+2.5e20)<1e6);
  const std::vector<double> y{std::exp(-1.),std::exp(-2.)};
  record("genbeta",umvl_binding::fit_genbeta({y.data(),y.size()},{},{true,1e100},{},{true,1}));
}
