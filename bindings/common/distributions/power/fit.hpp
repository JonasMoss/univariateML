#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_power(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  std::vector<std::string> fixed;
  const double alpha_held = !alpha.supplied ? std::numeric_limits<double>::quiet_NaN() : alpha.value;
  if (alpha.supplied) {
    if (!(std::isfinite(alpha_held) && alpha_held > 0.0)) return invalid_argument(fail("Held `alpha` must be positive and finite."));
    fixed.push_back("alpha");
  }
  const double beta_held = !beta.supplied ? std::numeric_limits<double>::quiet_NaN() : beta.value;
  if (beta.supplied) {
    if (!(std::isfinite(beta_held) && beta_held > 0.0)) return invalid_argument(fail("Held `beta` must be positive and finite."));
    fixed.push_back("beta");
  }

  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, alpha_held, beta_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"alpha",alpha_arg.supplied},{"beta",beta_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };

  if(res.status==cppunivariate::PowerStatus::OutsideSupport)
    return done(fail("Held alpha must be at least the sample maximum: zero likelihood."));
  if (res.status == cppunivariate::PowerStatus::Unbounded)
    return done(nomle("Power likelihood is unbounded as beta -> Inf when every observation equals alpha; hold beta to fit constant samples."));
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    return done(fail("The power distribution is only defined for positive numbers."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.alpha},{"beta",res.beta}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
