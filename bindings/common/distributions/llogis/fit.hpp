#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_llogis(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
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

  cppunivariate::LogLogisticResult res =
      cppunivariate::loglogistic_mle(x, abstol, iterlim, shape_held, scale_held);
  if (res.status == cppunivariate::LogLogisticStatus::Unbounded)
    return fail("The log-logistic maximum likelihood estimator does not exist: the likelihood is unbounded for these constant transformed observations.");
  if (res.status == cppunivariate::LogLogisticStatus::IterationFailure)
    return fail("Log-logistic optimizer IterationFailure: failed to converge; increase iterlim.");
  if (!(res.shape>0.0) || !(res.scale>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    if (!shape.supplied && !scale.supplied && std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      return fail("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    return fail("Log-logistic MLE failed: data exceed double precision.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
