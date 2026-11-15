#pragma once
#include <algorithm>
#include "../../fit.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
namespace umvl_binding {
inline FitResult fit_exp(cppunivariate::span x, Held rate_arg = {}) {
  if (rate_arg.supplied && (!(rate_arg.value > 0) || !std::isfinite(rate_arg.value))) return invalid_argument(failure("Held `rate` must be positive and finite."));
  double rate=rate_arg.supplied ? rate_arg.value : NAN;
  auto r=cppunivariate::exp_mle(cppunivariate::span{x.data(),x.size()},rate);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) { return with_outcome(std::move(out), o, static_cast<int>(r.status),{{"rate",rate_arg.supplied}}); };
  if(r.status==cppunivariate::ExpStatus::Unbounded) return done(stop_failure(failure("The exponential maximum likelihood estimator does not exist when every observation is zero: the likelihood is unbounded as the rate grows."),{},outcome_payload(o),{},""));
  if(r.status==cppunivariate::ExpStatus::InvalidInput) return done(failure("Exponential requires nonempty nonnegative finite data and a valid held rate."));
  if(r.status!=cppunivariate::ExpStatus::Ok) return done(failure(!std::isfinite(r.rate)?"Exponential MLE rate exceeds double precision.":"Exponential likelihood exceeds double precision."));
  FitResult out;
  out.estimates.emplace_back("rate",r.rate); if(rate_arg.supplied) out.fixed.push_back("rate");
  out.logLik=r.logLik; return done(out);
}
}
