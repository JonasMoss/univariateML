#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <cmath>
#include <limits>
#include <string>
namespace umvl_binding {
inline FitResult fit_bbinom(cppunivariate::ispan x, Held size_arg = {}, Held alpha_arg = {}, Held beta_arg = {}, double reltol = 1e-10, int iterlim = 100) {
  std::vector<std::string> warnings;
  const Held size = size_arg;
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  const double N = size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if (size.supplied && (!std::isfinite(N) || N < 0 || N != std::floor(N) || N > 1e7))
    return failure("`size` must be a nonnegative integer (at most 1e7).");
  if (!size.supplied && !alpha.supplied && !beta.supplied)
    return failure("Free size requires comparing all fixed-size binomial and two-point faces with the negative-binomial and Poisson limits; no reliable integer-tail stopping rule is available.");
  if (!size.supplied && alpha.supplied && !beta.supplied)
    return failure("Free size with held alpha requires comparison with the negative-binomial limit of the same shape; no reliable integer-tail stopping rule is available.");
  if (!size.supplied && !alpha.supplied && beta.supplied)
    return failure("Free size with held beta has a proved tail bound, but it is too weak for practical enumeration; fixed-size point-mass limits also require comparison.");
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = alpha.supplied ? alpha.value : nan;
  const double b = beta.supplied ? beta.value : nan;
  if (alpha.supplied && !(std::isfinite(a) && a > 0))
    return failure("`alpha` must be a finite positive number.");
  if (beta.supplied && !(std::isfinite(b) && b > 0))
    return failure("`beta` must be a finite positive number.");
  const auto res = cppunivariate::bbinom_mle(x, N, reltol, iterlim, a, b);
  const std::string n_txt = std::to_string(static_cast<long long>(std::isfinite(N)?N:0));
  switch (res.status) {
    case cppunivariate::BbinomStatus::Ok:
      break;
    case cppunivariate::BbinomStatus::SizeTooSmall:
      return failure("`size` is smaller than `max(x)`.");
    case cppunivariate::BbinomStatus::BinomialLimit:
      return failure("The maximum likelihood estimator does not exist: the likelihood increases "
                 "toward the binomial distribution as `alpha + beta` grows (the data are not "
                 "overdispersed enough). Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::TwoPointLimit:
      return failure("The maximum likelihood estimator does not exist: every observation is 0 or "
                 "`size`, and the likelihood increases toward the two-point distribution on "
                 "{0, size} as `alpha` and `beta` tend to 0 with `alpha / (alpha + beta)` "
                 "fixed. Its probability of `size` is `mean(x == size)`, the fit of "
                 "`mlbinom(x / " + n_txt + ", size = 1)`.");
    case cppunivariate::BbinomStatus::PointMass:
      return failure("The maximum likelihood estimator does not exist: all observations equal 0 "
                 "or all equal `size`, and the likelihood increases toward a point mass. "
                 "Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::Unidentified:
      if (N == 0) return failure("With `size = 0` the beta-binomial parameters are not identified.");
      return failure("With `size = 1` the beta-binomial is a Bernoulli distribution and "
                 "`alpha + beta` is not identified. Use `mlbinom(x, size = 1)`, or hold "
                 "`alpha` or `beta` fixed.");
    case cppunivariate::BbinomStatus::InvalidInput:
      return failure("Beta-binomial fitting needs nonempty nonnegative counts, positive "
                 "`reltol` and positive `iterlim`.");
    default:
      return failure("Beta-binomial MLE failed.");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik))
    return failure("Beta-binomial MLE failed.");
  std::vector<std::string> fixed;
  if (size.supplied) fixed.push_back("size");
  if (alpha.supplied) fixed.push_back("alpha");
  if (beta.supplied) fixed.push_back("beta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"alpha",res.alpha},{"beta",res.beta}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
