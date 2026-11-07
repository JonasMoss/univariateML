#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include <limits>
#include <cmath>
namespace umvl_binding {
inline FitResult fit_lomax(cppunivariate::span x, Held lambda_arg = {}, Held kappa_arg = {}, Held lambda0 = {}, double reltol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held lambda = lambda_arg;
  const Held kappa = kappa_arg;

  std::vector<std::string> fixed;
  const double lambda_held = !lambda.supplied ? std::numeric_limits<double>::quiet_NaN() : lambda.value;
  if (lambda.supplied) {
    if (!(std::isfinite(lambda_held) && lambda_held > 0.0)) return fail("Held `lambda` must be positive and finite.");
    fixed.push_back("lambda");
  }
  const double kappa_held = !kappa.supplied ? std::numeric_limits<double>::quiet_NaN() : kappa.value;
  if (kappa.supplied) {
    if (!(std::isfinite(kappa_held) && kappa_held > 0.0)) return fail("Held `kappa` must be positive and finite.");
    fixed.push_back("kappa");
  }

  double start = std::numeric_limits<double>::quiet_NaN();
  if (lambda0.supplied) {
    start = lambda0.value;
    if (!(start > 0.0) || !std::isfinite(start)) {
      return fail("`lambda0` must be a positive finite number.");
    }
  }
  auto res = cppunivariate::lomax_mle(x, reltol, iterlim, start, lambda_held, kappa_held);
  if (res.status == cppunivariate::LomaxStatus::ExponentialLimit) {
    return stop_failure(fail("The certified profile bound places every improvement over the exponential limit within its tolerance (1e-8 per observation). Use `mlexp` to fit that limit model."),res.stop_detail,"limit",{"lambda","kappa"},"exp");
  }
  if (res.certificate_failure == cppunivariate::LomaxCertificateFailure::Budget)
    return stop_failure(fail("Lomax MLE failed to converge: global certificate budget exhausted (128 * min(iterlim, 100000) subdivision evaluations); no verified maximum. Raising iterlim may resolve this stop."),res.stop_detail,"budget",{"lambda","kappa"},"");
  if (res.certificate_failure == cppunivariate::LomaxCertificateFailure::Arithmetic)
    return fail("Lomax MLE failed to converge: numerical certificate comparisons unresolved or coefficients unrepresentable; no verified maximum.");
  if (res.status != cppunivariate::LomaxStatus::Ok || !std::isfinite(res.lambda) ||
      !std::isfinite(res.kappa)) {
    return fail("Lomax MLE failed to converge (check dispersion/inputs).");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"lambda",res.lambda},{"kappa",res.kappa}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
