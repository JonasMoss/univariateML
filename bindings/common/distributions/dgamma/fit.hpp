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
  if (shape.supplied && (!std::isfinite(a) || a<=0)) return invalid_argument(fail("Invalid held `shape`."));
  if (rate.supplied && (!std::isfinite(b) || b<=0)) return invalid_argument(fail("Invalid held `rate`."));
  const auto r=cppunivariate::dgamma_mle(x,reltol,iterlim,a,b);
  const auto o = cppunivariate::outcome(r);
  const auto done = [&](FitResult out) {
    if (r.status == cppunivariate::DgammaStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(r.status));
    return with_outcome(std::move(out), o, static_cast<int>(r.status));
  };
  if(r.stop_detail.budget_exhausted)return done(stop_failure(failure("MLE failed to converge; certificate work budget exhausted. Raising iterlim may resolve this stop."),r.stop_detail,outcome_kind(o),{"shape","rate"}));
  std::string stop_kind;
  switch(r.status) {
    case cppunivariate::DgammaStatus::PointMass: stop_kind=outcome_kind(o);break;
    case cppunivariate::DgammaStatus::TwoPointLimit: stop_kind=outcome_kind(o);break;
    case cppunivariate::DgammaStatus::IterationFailure: stop_kind=outcome_kind(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,r.stop_detail,stop_kind,{"shape","rate"});
  };

  switch (r.status) {
    case cppunivariate::DgammaStatus::Ok: break;
    case cppunivariate::DgammaStatus::InvalidData: return done(stopped_fail("Requires nonempty finite nonnegative integer data (absolute value below 2^52)."));
    case cppunivariate::DgammaStatus::InvalidControl: return done(stopped_fail("Invalid control: reltol must be finite and positive, iterlim at least 1."));
    case cppunivariate::DgammaStatus::PointMass: return done(stopped_fail("The maximum likelihood estimator does not exist: the likelihood approaches a point mass."));
    case cppunivariate::DgammaStatus::TwoPointLimit:
      return done(stopped_fail("The maximum likelihood estimator does not exist on two adjacent integers; use `mlbinom(x - k, size = 1)` with k = min(x)."));
    case cppunivariate::DgammaStatus::IterationFailure:
      if(std::isfinite(r.rate_score_roundoff))
        return done(stopped_fail("Large-shape log-rate score precision is insufficient for the requested coefficient accuracy."));
      if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)
        return done(stopped_fail("The discretized gamma fit could not meet the requested coefficient accuracy in floating-point arithmetic."));
      return done(stopped_fail("MLE failed to converge; increase iterlim."));
  }
  std::vector<std::string> fixed;
  if (shape.supplied) fixed.push_back("shape");
  if (rate.supplied) fixed.push_back("rate");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",r.shape},{"rate",r.rate}}; out.logLik = r.logLik; out.iterations = r.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
