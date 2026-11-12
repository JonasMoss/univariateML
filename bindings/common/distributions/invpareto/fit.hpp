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
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return fail("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return fail("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (scale0.supplied) {
    start = scale0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return fail("`scale0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::invpareto_mle(x, reltol, iterlim, start, shape_held, scale_held);
  if(res.status==cppunivariate::LomaxStatus::AccuracyFailure)return stop_failure(fail("Lomax delegate requested coefficient accuracy cannot be established."),res.stop_detail,"numerical",{"shape","scale"});
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    return stop_failure(fail("The certified profile bound places every improvement over the inverse exponential limit within its tolerance. Use `mlinvexp` to fit that limit model."),res.stop_detail,"limit",{"shape","scale"},"invexp");
  }
  if(res.status==cppunivariate::LomaxStatus::IterationFailure) return stop_failure(fail("Certified Lomax delegate failed; no verified maximum. Raising iterlim may resolve a budget stop."),res.stop_detail,res.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget?"budget":"numerical",{"shape","scale"},"");
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.scale) ||
      !std::isfinite(res.shape)) {
    return fail("Inverse Pareto MLE failed to converge (check dispersion/inputs)");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
