#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/dweibull/mle.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <string>
namespace umvl_binding {
inline FitResult fit_dweibull(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, double reltol = 1e-12, int iterlim = 200) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;

  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double q = shape1.supplied ? shape1.value : nan;
  const double b = shape2.supplied ? shape2.value : nan;
  if (shape1.supplied && !(q > 0 && q < 1))
    return invalid_argument(fail("`shape1` must lie strictly between 0 and 1."));
  if (shape2.supplied && !(b > 0 && std::isfinite(b)))
    return invalid_argument(fail("`shape2` must be finite and positive."));
  const auto res = cppunivariate::dweibull_mle(x, reltol, iterlim, q, b);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.error_kind != "tie")
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };

  std::string stop_kind;
  switch(res.status) {
    case cppunivariate::DweibullStatus::PointMass: stop_kind=outcome_kind(o);break;
    case cppunivariate::DweibullStatus::TwoPointLimit: stop_kind=outcome_kind(o);break;
    case cppunivariate::DweibullStatus::BernoulliLimit: stop_kind=outcome_kind(o);break;
    case cppunivariate::DweibullStatus::HeldBernoulliLimit: stop_kind=outcome_kind(o);break;
    default:break;
  }
  const auto stopped_fail = [&](const std::string& format, const auto&... arguments) {
    auto out=fail(binding_message(format,arguments...));
    return stop_kind.empty()?out:stop_failure(out,res.stop_detail,stop_kind,{"shape1","shape2"});
  };

  switch (res.status) {
    case cppunivariate::DweibullStatus::Ok:
    case cppunivariate::DweibullStatus::ShapeUnidentified:
      break;
    case cppunivariate::DweibullStatus::InvalidData:
      return done(stopped_fail("Discrete Weibull requires nonempty, finite, nonnegative integer data."));
    case cppunivariate::DweibullStatus::InvalidControl:
      return done(stopped_fail("`reltol` must be positive and `iterlim` at least 1."));
    case cppunivariate::DweibullStatus::PointMass:
      if (shape2.supplied)
        return done(stopped_fail("All observations are 0: with `shape2` held, the likelihood increases "
                   "toward the point mass at 0 as `shape1` -> 0, and the maximum "
                   "likelihood estimator does not exist."));
      return done(stopped_fail("All observations equal %.0f: the likelihood increases toward the point "
                 "mass at %.0f, and the maximum likelihood estimator does not exist.",
                 x[0], x[0]));
    case cppunivariate::DweibullStatus::BernoulliLimit:
      return done(stopped_fail("The observations are 0 and 1 only: the likelihood increases toward a "
                 "Bernoulli distribution (binomial with size 1) as `shape2` -> Inf, and "
                 "the maximum likelihood estimator does not exist. Use "
                 "`mlbinom(x, size = 1)` instead."));
    case cppunivariate::DweibullStatus::TwoPointLimit: {
      const double lo = *std::min_element(x.begin(),x.end());
      return done(stopped_fail("The observations take the adjacent values %.0f and %.0f only: the "
                 "likelihood increases toward the two-point distribution on these values "
                 "as `shape2` -> Inf, and the maximum likelihood estimator does not exist.",
                 lo, lo + 1));
    }
    case cppunivariate::DweibullStatus::HeldBernoulliLimit:
      return done(stopped_fail("The observations are at most 1: with `shape1` held, the likelihood "
                 "increases toward the Bernoulli distribution with success probability "
                 "`shape1` as `shape2` -> Inf, and the maximum likelihood estimator does "
                 "not exist."));
    case cppunivariate::DweibullStatus::Unrepresentable:
      if (res.shape1 == 0)
        return done(stopped_fail("The fitted `shape1` rounds to 0 in double precision and cannot be returned."));
      return done(stopped_fail("The fitted `shape1` rounds to 1 in double precision (the latent Weibull "
                 "scale to the power `shape2` exceeds 2^53), so the fit cannot be expressed "
                 "in extraDistr's (shape1, shape2) parametrization."));
    case cppunivariate::DweibullStatus::IterationFailure:
      return done(stopped_fail("Discrete Weibull MLE failed to converge; increase `iterlim`."));
  }
  if (res.status == cppunivariate::DweibullStatus::ShapeUnidentified)
    warnings.push_back(binding_message("All observations are 0 and `shape1` is held: the likelihood does not "
                  "depend on `shape2`, so the maximum likelihood estimator is not unique; "
                  "returning `shape2 = 1`."));
  std::vector<std::string> fixed;
  if (shape1.supplied) fixed.push_back("shape1");
  if (shape2.supplied) fixed.push_back("shape2");
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
