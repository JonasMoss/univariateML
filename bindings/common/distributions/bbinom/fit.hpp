#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/bbinom/mle.hpp"
#include <cmath>
#include <limits>
#include <string>
namespace umvl_binding {
inline FitResult fit_bbinom(cppunivariate::ispan x, Held size_arg = {}, Held alpha_arg = {}, Held beta_arg = {}, double reltol = 1e-10, int iterlim = 500) {
  std::vector<std::string> warnings;
  const Held size = size_arg;
  const Held alpha = alpha_arg;
  const Held beta = beta_arg;

  const double N = size.supplied?size.value:std::numeric_limits<double>::quiet_NaN();
  if (size.supplied && (!std::isfinite(N) || N < 0 || N != std::floor(N) || N > 1e7))
    return failure("`size` must be a nonnegative integer (at most 1e7).");
  if (!size.supplied && !alpha.supplied && !beta.supplied)
    return failure("Estimating size together with both shapes is not implemented. It would require comparing the fits at every finite size, including their binomial and two-point limits, with the negative binomial and Poisson limits, and no usable bound on large sizes is known.");
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double a = alpha.supplied ? alpha.value : nan;
  const double b = beta.supplied ? beta.value : nan;
  if (alpha.supplied && !(std::isfinite(a) && a > 0))
    return failure("`alpha` must be a finite positive number.");
  if (beta.supplied && !(std::isfinite(b) && b > 0))
    return failure("`beta` must be a finite positive number.");
  const auto res = cppunivariate::bbinom_mle(x, N, reltol, iterlim, a, b);
  if(res.accuracy_failure)return stop_failure(failure("bbinom numerical accuracy failure: the returned coefficients cannot be resolved to the requested tolerance in the local refinement."),res.stop_detail,"numerical",{"size","alpha","beta"});
  if(res.status!=cppunivariate::BbinomStatus::Ok && res.stop_detail.budget_exhausted)return stop_failure(failure("Beta-binomial search exhausted its work budget before certifying the maximum. Raising iterlim may resolve this stop."),res.stop_detail,"budget",{"size","alpha","beta"});
  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::BbinomStatus::BinomialLimit: stop_kind="limit";break;
    case cppunivariate::BbinomStatus::TwoPointLimit: stop_kind="limit";break;
    case cppunivariate::BbinomStatus::PointMass: stop_kind="limit";break;
    case cppunivariate::BbinomStatus::Unidentified: stop_kind="tie";break;
    case cppunivariate::BbinomStatus::SizeBudget: stop_kind="budget";break;
    case cppunivariate::BbinomStatus::ProfileTie: stop_kind="tie";break;
    case cppunivariate::BbinomStatus::FaceUnresolved: stop_kind="unresolved";break;
    case cppunivariate::BbinomStatus::IterationFailure: stop_kind="numerical";break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=failure(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"size","alpha","beta"});
  };

  const std::string n_txt = std::to_string(static_cast<long long>(std::isfinite(N)?N:0));
  switch (res.status) {
    case cppunivariate::BbinomStatus::Ok:
      break;
    case cppunivariate::BbinomStatus::SizeTooSmall:
      return stopped_fail("`size` is smaller than `max(x)`.");
    case cppunivariate::BbinomStatus::BinomialLimit:
      return stopped_fail("The maximum likelihood estimator does not exist: the likelihood increases "
                 "toward the binomial distribution as `alpha + beta` grows. Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::TwoPointLimit:
      return stopped_fail("The maximum likelihood estimator does not exist: every observation is 0 or "
                 "`size`, and the likelihood increases toward the two-point distribution on "
                 "{0, size} as `alpha` and `beta` tend to 0 with `alpha / (alpha + beta)` "
                 "fixed. Its probability of `size` is `mean(x == size)`, the fit of "
                 "`mlbinom(x / " + n_txt + ", size = 1)`.");
    case cppunivariate::BbinomStatus::PointMass:
      if (!size.supplied) return stopped_fail("The maximum likelihood estimator does not exist: the likelihood approaches a point mass at the common positive count.");
      return stopped_fail("The maximum likelihood estimator does not exist: all observations equal 0 "
                 "or all equal `size`, and the likelihood increases toward a point mass. "
                 "Use `mlbinom(x, size = " + n_txt + ")` instead.");
    case cppunivariate::BbinomStatus::Unidentified:
      if (!size.supplied) return stopped_fail("With all counts zero, size zero attains the maximum and the free shape is not identified.");
      if (N == 0) return stopped_fail("With `size = 0` the beta-binomial parameters are not identified.");
      return stopped_fail("With `size = 1` the beta-binomial is a Bernoulli distribution and "
                 "`alpha + beta` is not identified. Use `mlbinom(x, size = 1)`, or hold "
                 "`alpha` or `beta` fixed.");
    case cppunivariate::BbinomStatus::InvalidInput:
      return stopped_fail("Beta-binomial fitting needs nonempty nonnegative counts, positive "
                 "`reltol` and positive `iterlim`.");
    case cppunivariate::BbinomStatus::SizeBudget:
      return stopped_fail("Beta-binomial search exhausted its work budget before certifying the maximum. Raising iterlim may resolve this stop.");
    case cppunivariate::BbinomStatus::ProfileTie:
      return stopped_fail(size.supplied ? "Beta-binomial finite maxima or a limit face have an unresolved numerical tie." : "Beta-binomial free-size profiles have an unresolved numerical tie.");
    case cppunivariate::BbinomStatus::FaceUnresolved:
      return stopped_fail(size.supplied ? "Beta-binomial fixed-size search cannot resolve the binomial face against finite shapes within numerical precision or the work budget." : "Beta-binomial free-size search cannot resolve the negative binomial face against larger finite sizes.");
    default:
      return stopped_fail("Beta-binomial MLE failed to certify the maximum and coefficient accuracy.");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik))
    return stopped_fail("Beta-binomial MLE failed to certify the maximum and coefficient accuracy.");
  std::vector<std::string> fixed;
  if (size.supplied) fixed.push_back("size");
  if (alpha.supplied) fixed.push_back("alpha");
  if (beta.supplied) fixed.push_back("beta");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"size",res.size},{"alpha",res.alpha},{"beta",res.beta}}; out.extra.push_back({"certified", res.certified ? 1.0 : 0.0}); out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
