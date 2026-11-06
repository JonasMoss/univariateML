#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dnorm/mle.hpp"
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_dnorm(cppunivariate::span x, Held mean_arg = {}, Held sd_arg = {}, double reltol = 1e-9, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held sd = sd_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double a=mean.supplied ? mean.value:nan;
  const double b=sd.supplied ? sd.value:nan;
  if (mean.supplied && (!std::isfinite(a))) return fail("Invalid held `mean`.");
  if (sd.supplied && (!std::isfinite(b) || b<=0)) return fail("Invalid held `sd`.");
  const auto r=cppunivariate::dnorm_mle(x,reltol,iterlim,a,b);
  std::string stop_kind;
  switch(r.status) {
    case cppunivariate::DnormStatus::PointMass: stop_kind="limit";break;
    case cppunivariate::DnormStatus::TwoPointLimit: stop_kind="limit";break;
    case cppunivariate::DnormStatus::ScaleLimit: stop_kind="limit";break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,r.stop_detail,stop_kind,{"mean","sd"});
  };

  switch (r.status) {
    case cppunivariate::DnormStatus::Ok: break;
    case cppunivariate::DnormStatus::InvalidData: return stopped_fail("Requires nonempty finite integer data (absolute value below 2^52).");
    case cppunivariate::DnormStatus::InvalidControl: return stopped_fail("Invalid control: reltol must be finite and positive, iterlim at least 1.");
    case cppunivariate::DnormStatus::PointMass: return stopped_fail("The maximum likelihood estimator does not exist: the likelihood approaches a point mass.");
    case cppunivariate::DnormStatus::TwoPointLimit:
      return stopped_fail("The maximum likelihood estimator does not exist on two adjacent integers; use `mlbinom(x - k, size = 1)` with k = min(x).");
    case cppunivariate::DnormStatus::ScaleLimit: return stopped_fail("With mean held the likelihood approaches its zero-scale limit; the maximum likelihood estimator does not exist.");
    case cppunivariate::DnormStatus::IterationFailure: return stopped_fail("MLE failed to converge; increase iterlim.");
  }
  std::vector<std::string> fixed;
  if (mean.supplied) fixed.push_back("mean");
  if (sd.supplied) fixed.push_back("sd");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",r.mean},{"sd",r.sd}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
