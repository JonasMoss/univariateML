#pragma once
#include <algorithm>
#include "../../fit.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_exp(cppunivariate::span x, Held rate_arg = {}) {
  if (rate_arg.supplied && (!(rate_arg.value > 0) || !std::isfinite(rate_arg.value))) return failure("Held `rate` must be positive and finite.");
  double rate=rate_arg.supplied ? rate_arg.value : NAN;
  auto r=cppunivariate::exp_mle(cppunivariate::span{x.data(),x.size()},rate);
  if(r.status==cppunivariate::ExpStatus::Unbounded) return failure("The exponential maximum likelihood estimator does not exist when every observation is zero: the likelihood is unbounded as the rate grows.");
  if(r.status==cppunivariate::ExpStatus::InvalidInput) return failure("Exponential requires nonempty nonnegative finite data and a valid held rate.");
  if(!std::isfinite(r.rate)) return failure(std::any_of(x.begin(),x.end(),[](double z){return z>0;}) ? "Exponential MLE rate exceeds double precision." : "The exponential maximum likelihood estimator does not exist when every observation is zero: the likelihood is unbounded as the rate grows.");
  if(!std::isfinite(r.logLik)) return failure("Exponential likelihood exceeds double precision.");
  FitResult out;
  out.estimates.emplace_back("rate",r.rate); if(rate_arg.supplied) out.fixed.push_back("rate");
  out.logLik=r.logLik; return out;
}
}
