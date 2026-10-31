#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/binom/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace umvl_binding {
inline FitResult fit_binom(cppunivariate::ispan x, Held size_arg = {}, Held prob_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held size = size_arg;
  const Held prob = prob_arg;

  const double size_or_nan = size.supplied
                                 ? size.value
                                 : std::numeric_limits<double>::quiet_NaN();
  // The fitter reads a non-finite size as "estimate it"; a held size that
  // is NA, NaN, infinite or fractional must not slip through that way.
  if (size.supplied && (!std::isfinite(size_or_nan) || size_or_nan < 0.0 ||
                           size_or_nan != std::floor(size_or_nan))) {
    return fail("`size` must be a nonnegative integer.");
  }
  const double p=prob.supplied?prob.value:std::numeric_limits<double>::quiet_NaN();
  if(prob.supplied && (!std::isfinite(p)||p<0||p>1))return fail("`prob` must be finite and in [0, 1].");
  auto res = cppunivariate::binom_mle(x, size_or_nan, abstol, iterlim, p);
  if (res.status == cppunivariate::BinomStatus::InvalidInput) return fail("Held parameters are incompatible with the observations.");
  if (res.status == cppunivariate::BinomStatus::SizeTooSmall) {
    return fail("`size` is smaller than `max(x)`.");
  }
  if (res.status == cppunivariate::BinomStatus::Underdispersed) {
    return fail(
        "The maximum likelihood estimator does not exist. Use `mlpois` to fit "
        "a Poisson or supply a `size` argument.");
  }
  if (res.status != cppunivariate::BinomStatus::Ok || !std::isfinite(res.size)) {
    return fail("Binomial MLE failed (check dispersion/inputs)");
  }
  std::vector<std::string> fixed;
  if(size.supplied)fixed.push_back("size");
  if(prob.supplied)fixed.push_back("prob");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"prob",res.prob}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
