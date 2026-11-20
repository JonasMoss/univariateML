#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include <limits>
#include <algorithm>
#include <cmath>
#include <vector>
namespace umvl_binding {
inline FitResult fit_invweibull(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;

  std::vector<std::string> fixed;
  const double shape_held = !shape.supplied ? std::numeric_limits<double>::quiet_NaN() : shape.value;
  if (shape.supplied) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return invalid_argument(fail("Held `shape` must be positive and finite."));
    fixed.push_back("shape");
  }
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return invalid_argument(fail("Held `scale` must be positive and finite."));
    fixed.push_back("scale");
  }

  auto res = cppunivariate::invweibull_mle(x, tol, iterlim, shape_held, scale_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape",shape_arg.supplied},{"scale",scale_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };

  if (res.status == cppunivariate::InvWeibullStatus::InvalidInput)
    return done(fail("Inverse Weibull requires nonempty positive finite data, valid held parameters, positive tol and positive iterlim."));
  if (res.status == cppunivariate::InvWeibullStatus::Unbounded)
    return done(nomle("The invweibull maximum likelihood estimator does not exist: the likelihood is unbounded when all transformed observations equal the held scale."));
  if (res.status == cppunivariate::InvWeibullStatus::IterationFailure)
    return done(fail("Inverse Weibull MLE failed to converge or cannot resolve the fitted coefficients and likelihood in double precision."));
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    return done(fail("Inverse Weibull MLE failed"));
  }
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.root_radius = res.root_radius; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
