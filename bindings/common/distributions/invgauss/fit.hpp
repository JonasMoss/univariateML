#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_invgauss(cppunivariate::span x, Held mean_arg = {}, Held shape_arg = {}) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held mean = mean_arg;
  const Held shape = shape_arg;

  std::vector<std::string> fixed;
  const double mean_held = !mean.supplied ? std::numeric_limits<double>::quiet_NaN() : mean.value;
  if (mean.supplied) {
    if (!(std::isfinite(mean_held) && mean_held > 0.0)) return fail("Held `mean` must be positive and finite.");
    fixed.push_back("mean");
  }
  const double shape_held = !shape.supplied ? std::numeric_limits<double>::quiet_NaN() : shape.value;
  if (shape.supplied) {
    if (!(std::isfinite(shape_held) && shape_held > 0.0)) return fail("Held `shape` must be positive and finite.");
    fixed.push_back("shape");
  }

  auto res = cppunivariate::invgauss_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, mean_held, shape_held);
  if (res.status == cppunivariate::InvGaussStatus::InvalidInput) return fail("Inverse Gaussian requires nonempty positive finite data and valid held parameters.");
  if (res.status != cppunivariate::InvGaussStatus::Ok || !(res.lambda > 0.0) || !std::isfinite(res.lambda) || !std::isfinite(res.logLik)) {
    return fail("Inverse Gaussian MLE failed: constant data have no attained maximum; other data may exceed double precision.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"mean",res.mu},{"shape",res.lambda}}; out.logLik = res.logLik; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
