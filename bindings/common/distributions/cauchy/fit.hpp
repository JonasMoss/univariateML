#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace umvl_binding {
inline FitResult fit_cauchy(cppunivariate::span x, Held location_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held location = location_arg;
  const Held scale = scale_arg;

  const double nan=std::numeric_limits<double>::quiet_NaN();
  const double held_location=location.supplied?location.value:nan;
  if(location.supplied && (!std::isfinite(held_location)))return invalid_argument(fail("`location` must be finite."));
  const double held_scale=scale.supplied?scale.value:nan;
  if(scale.supplied && (!std::isfinite(held_scale) || !(held_scale>0)))return invalid_argument(fail("`scale` must be positive and finite."));
  std::vector<std::string> fixed;
  if(location.supplied)fixed.push_back("location");
  if(scale.supplied)fixed.push_back("scale");

  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(x,abstol,iterlim,held_location,held_scale);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::CauchyStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if(res.stop_detail.budget_exhausted)return done(stop_failure(failure("Conditional fit did not converge: numerical certificate work budget exhausted. Raising iterlim may resolve this stop."),res.stop_detail,outcome_kind(o),{"location","scale"}));
  if(res.status==cppunivariate::CauchyStatus::NoMLE && location.supplied)
    return done(stop_failure(fail("With location held the Cauchy scale MLE does not exist when at least half the observations equal that location."),res.stop_detail,outcome_kind(o),{"location","scale"},""));
  if (res.status == cppunivariate::CauchyStatus::NoMLE)
    return done(stop_failure(fail("Cauchy MLE does not exist: a sample atom has at least half the mass, except for balanced two-point samples"),res.stop_detail,outcome_kind(o),{"location","scale"},""));
  if(res.status==cppunivariate::CauchyStatus::AmbiguousMaximum)return done(stop_failure(fail("Held-scale Cauchy global maxima are tied or numerically indistinguishable; no unique certified location returned."),res.stop_detail,"tie",{"location","scale"},""));
  if(res.status==cppunivariate::CauchyStatus::InvalidInput)return done(fail("Cauchy fit requires finite data, positive tolerance and iterlim, and valid held coefficients."));
  if(res.status==cppunivariate::CauchyStatus::IterationFailure)return done(fail("Conditional fit did not converge: numerical certificate, scale bracket or iteration failure; increase iterlim only for iteration exhaustion."));
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    return done(fail("Cauchy MLE failed: non-finite or invalid parameters"));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",res.mu},{"scale",res.scale}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
