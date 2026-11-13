#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invpareto/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_invpareto(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held scale0 = {}, double reltol = 1.220703e-4, int iterlim = 100) {
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

  double start = std::numeric_limits<double>::quiet_NaN();
  if (scale0.supplied) {
    start = scale0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return invalid_argument(fail("`scale0` must be a positive finite number."));
    }
  }
  auto res = cppunivariate::invpareto_mle(x, reltol, iterlim, start, shape_held, scale_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::LomaxStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
  if(res.status==cppunivariate::LomaxStatus::AccuracyFailure)return done(stop_failure(fail("Lomax delegate requested coefficient accuracy cannot be established."),res.stop_detail,outcome_kind(o),{"shape","scale"}));
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    return done(stop_failure(fail("The certified profile bound places every improvement over the inverse exponential limit within its tolerance. Use `mlinvexp` to fit that limit model."),res.stop_detail,outcome_kind(o),{"shape","scale"},"invexp"));
  }
  if(res.status==cppunivariate::LomaxStatus::IterationFailure) return done(stop_failure(fail("Certified Lomax delegate failed; no verified maximum. Raising iterlim may resolve a budget stop."),res.stop_detail,outcome_kind(o),{"shape","scale"},""));
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.scale) ||
      !std::isfinite(res.shape)) {
    return done(fail("Inverse Pareto MLE failed to converge (check dispersion/inputs)"));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
