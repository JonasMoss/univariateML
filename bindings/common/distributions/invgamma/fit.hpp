#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_invgamma(cppunivariate::span x, Held alpha_arg = {}, Held beta_arg = {}, double tol = 1e-10, int iterlim = 100) {

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

  auto res = cppunivariate::invgamma_mle(x, tol, iterlim, alpha_held, beta_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"alpha",alpha_arg.supplied},{"beta",beta_arg.supplied}});
  };
  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if (res.status == cppunivariate::GammaStatus::InvalidInput) return done(fail("Inverse gamma requires nonempty positive finite data and valid held parameters."));
  if (res.status == cppunivariate::GammaStatus::TransformFailure) return done(fail("invgamma MLE failed: transformed sufficient statistics or fitted coefficients cannot be resolved in double precision."));
  if (res.status == cppunivariate::GammaStatus::IterationFailure)
    return done(fail("Gamma delegate optimizer IterationFailure; increase iterlim."));
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    return done(nomle("The inverse gamma maximum likelihood estimator does not exist for constant "
               "data: the likelihood is unbounded as the shape grows."));
  }
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"alpha",res.shape},{"beta",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
