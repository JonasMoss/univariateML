#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include <limits>
#include <algorithm>
#include <cmath>
#include <vector>
namespace umvl_binding {
inline FitResult fit_weibull(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held shape0 = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;

  std::vector<std::string> fixed;
  const double shape_held = !shape.supplied ? std::numeric_limits<double>::quiet_NaN() : shape.value;
  if (shape.supplied) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return fail("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return fail("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (shape0.supplied) {
    start = shape0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return fail("`shape0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::weibull_mle(x, reltol, iterlim, start, shape_held, scale_held);
  if (res.status == cppunivariate::WeibullStatus::InvalidInput)
    return fail("Weibull requires nonempty positive finite data, valid held parameters, positive reltol and positive iterlim.");
  if (res.status == cppunivariate::WeibullStatus::Unbounded)
    return fail("The weibull maximum likelihood estimator does not exist: the likelihood is unbounded when all transformed observations equal the held scale.");
  if (res.status == cppunivariate::WeibullStatus::IterationFailure)
    return fail("The held-parameter weibull score solver failed to converge; increase iterlim.");
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    if (!shape.supplied && !scale.supplied && std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      return fail("The Weibull maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    return fail("Weibull MLE failed");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
