#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_dgamma(cppunivariate::span x, Held shape_arg = {}, Held rate_arg = {}, double reltol = 1e-10, int iterlim = 400) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held rate = rate_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=shape.supplied ? shape.value:nan;
  const double b=rate.supplied ? rate.value:nan;
  if (shape.supplied && (!std::isfinite(a) || a<=0)) return fail("Invalid held `shape`.");
  if (rate.supplied && (!std::isfinite(b) || b<=0)) return fail("Invalid held `rate`.");
  const auto r=cppunivariate::dgamma_mle(x,reltol,iterlim,a,b);
  if(r.stop_detail.budget_exhausted)return stop_failure(failure("MLE failed to converge; certificate work budget exhausted. Raising iterlim may resolve this stop."),r.stop_detail,"budget",{"shape","rate"});
  std::string stop_kind;
  switch(r.status) {
    case cppunivariate::DgammaStatus::PointMass: stop_kind="limit";break;
    case cppunivariate::DgammaStatus::TwoPointLimit: stop_kind="limit";break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,r.stop_detail,stop_kind,{"shape","rate"});
  };

  switch (r.status) {
    case cppunivariate::DgammaStatus::Ok: break;
    case cppunivariate::DgammaStatus::InvalidData: return stopped_fail("Requires nonempty finite nonnegative integer data (absolute value below 2^52).");
    case cppunivariate::DgammaStatus::InvalidControl: return stopped_fail("Invalid control: reltol must be finite and positive, iterlim at least 1.");
    case cppunivariate::DgammaStatus::PointMass: return stopped_fail("The maximum likelihood estimator does not exist: the likelihood approaches a point mass.");
    case cppunivariate::DgammaStatus::TwoPointLimit:
      return stopped_fail("The maximum likelihood estimator does not exist on two adjacent integers; use `mlbinom(x - k, size = 1)` with k = min(x).");
    case cppunivariate::DgammaStatus::IterationFailure: return stopped_fail("MLE failed to converge; increase iterlim.");
  }
  std::vector<std::string> fixed;
  if (shape.supplied) fixed.push_back("shape");
  if (rate.supplied) fixed.push_back("rate");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",r.shape},{"rate",r.rate}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
