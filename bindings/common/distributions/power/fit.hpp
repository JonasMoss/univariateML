#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_power(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, double epsilon = 1.490116e-08) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  std::vector<std::string> fixed;
  const double alpha_held = !alpha.supplied ? std::numeric_limits<double>::quiet_NaN() : alpha.value;
  if (alpha.supplied) {
    if (!(std::isfinite(alpha_held) && alpha_held > 0.0)) return fail("Held `alpha` must be positive and finite.");
    fixed.push_back("alpha");
  }
  const double beta_held = !beta.supplied ? std::numeric_limits<double>::quiet_NaN() : beta.value;
  if (beta.supplied) {
    if (!(std::isfinite(beta_held) && beta_held > 0.0)) return fail("Held `beta` must be positive and finite.");
    fixed.push_back("beta");
  }
  if (alpha.supplied && std::any_of(x.begin(),x.end(),[&](double v){return v>alpha_held;})) return fail("Held alpha must be at least the sample maximum: zero likelihood.");

  if (!(epsilon>=0.0) || !std::isfinite(epsilon))
    return fail("`epsilon` must be nonnegative and finite.");
  auto res = cppunivariate::power_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, epsilon, alpha_held, beta_held);
  if (res.status == cppunivariate::PowerStatus::Unbounded)
    return fail("Power likelihood is unbounded as beta -> Inf when every observation equals alpha; hold beta to fit constant samples.");
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    return fail("The power distribution is only defined for positive numbers.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.alpha},{"beta",res.beta}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
