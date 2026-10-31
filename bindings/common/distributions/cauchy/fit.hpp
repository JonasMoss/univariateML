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
  if(location.supplied && (!std::isfinite(held_location)))return fail("`location` must be finite.");
  const double held_scale=scale.supplied?scale.value:nan;
  if(scale.supplied && (!std::isfinite(held_scale) || !(held_scale>0)))return fail("`scale` must be positive and finite.");
  std::vector<std::string> fixed;
  if(location.supplied)fixed.push_back("location");
  if(scale.supplied)fixed.push_back("scale");

  cppunivariate::CauchyResult res = cppunivariate::fit_cauchy(x,abstol,iterlim,held_location,held_scale);
  if(res.status==cppunivariate::CauchyStatus::NoMLE && location.supplied)
    return fail("With location held the Cauchy scale MLE does not exist when at least half the observations equal that location.");
  if (res.status == cppunivariate::CauchyStatus::NoMLE)
    return fail("Cauchy MLE does not exist: a sample atom has at least half the mass, except for balanced two-point samples");
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    return fail("Cauchy MLE failed: non-finite or invalid parameters");
  if(res.status==cppunivariate::CauchyStatus::IterationFailure)return fail("Conditional fit did not converge; increase iterlim.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"location",res.mu},{"scale",res.scale}}; out.fixed = fixed; out.has_fixed = true; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; return out; }();

}
}
