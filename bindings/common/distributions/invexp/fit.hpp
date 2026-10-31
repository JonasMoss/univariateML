#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invexp/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_invexp(cppunivariate::span x, Held scale_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held scale = scale_arg;

  std::vector<std::string> fixed;
  const double scale_held = !scale.supplied ? std::numeric_limits<double>::quiet_NaN() : scale.value;
  if (scale.supplied) {
    if (!(std::isfinite(scale_held) && scale_held > 0.0)) return fail("Held `scale` must be positive and finite.");
    fixed.push_back("scale");
  }

  auto r=cppunivariate::invexp_mle(x, scale_held);
  if (r.status == cppunivariate::InvExpStatus::InvalidInput) return fail("Inverse exponential requires nonempty positive finite data and a valid held scale.");
  if (r.status != cppunivariate::InvExpStatus::Ok) return fail("Inverse exponential MLE exceeds double precision.");
  if (!(r.scale>0.0) || !std::isfinite(r.scale) || !std::isfinite(r.logLik))
    return fail("Inverse exponential MLE exceeds double precision.");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"scale",r.scale}}; out.logLik = r.logLik; out.iterations = 0; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
