#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/logis/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace umvl_binding {
inline FitResult fit_logis(cppunivariate::span x, Held location_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {

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

  cppunivariate::LogisticResult res =
      cppunivariate::logistic_mle(x,abstol,iterlim,held_location,held_scale);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::LogisticStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"location",location_arg.supplied},{"scale",scale_arg.supplied}});
  };
  if(res.status==cppunivariate::LogisticStatus::NumericalFailure)return done(fail("Logistic likelihood cannot be evaluated at the represented coefficients."));
  if(res.status==cppunivariate::LogisticStatus::Unbounded)
    return done(stop_failure(fail("Logistic likelihood is unbounded for constant data at the fitted or held location."),{},PayloadKind::Nonexistence,{}));
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    return done(fail("Logistic MLE failed: non-finite or invalid parameters"));
  if(res.status==cppunivariate::LogisticStatus::IterationFailure)return done(fail("Conditional fit did not converge; increase iterlim."));
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",res.mu},{"scale",res.scale}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }());

}
}
