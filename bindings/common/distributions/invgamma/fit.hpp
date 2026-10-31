#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_invgamma(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
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

  auto res = cppunivariate::invgamma_mle(x, abstol, iterlim, alpha_held, beta_held);
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    return fail("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      return fail("invgamma MLE failed: transformed observations exceed double precision.");
    return fail("The inverse gamma maximum likelihood estimator does not exist for constant "
               "data: the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    return fail("Inverse gamma MLE failed");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"alpha",res.shape},{"beta",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
