#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lgamma/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_lgamma(cppunivariate::span x, Held shapelog_arg = {}, Held ratelog_arg = {}, double tol = 1e-10, int iterlim = 100) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shapelog = shapelog_arg;
  const Held ratelog = ratelog_arg;

  std::vector<std::string> fixed;
  const double shapelog_held = !shapelog.supplied ? std::numeric_limits<double>::quiet_NaN() : shapelog.value;
  if (shapelog.supplied) {
    if (!(std::isfinite(shapelog_held) && shapelog_held > 0.0)) return invalid_argument(fail("Held `shapelog` must be positive and finite."));
    fixed.push_back("shapelog");
  }
  const double ratelog_held = !ratelog.supplied ? std::numeric_limits<double>::quiet_NaN() : ratelog.value;
  if (ratelog.supplied) {
    if (!(std::isfinite(ratelog_held) && ratelog_held > 0.0)) return invalid_argument(fail("Held `ratelog` must be positive and finite."));
    fixed.push_back("ratelog");
  }

  const auto res=cppunivariate::lgamma_mle(x,tol,iterlim,shapelog_held,ratelog_held);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::LgammaStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shapelog",shapelog_arg.supplied},{"ratelog",ratelog_arg.supplied}});
  };

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if(res.status==cppunivariate::LgammaStatus::InvalidInput)
    return done(fail("Loggamma fitting needs nonempty finite observations greater than 1, positive tol and positive iterlim."));
  if(res.status==cppunivariate::LgammaStatus::IterationFailure)
    return done(fail("Gamma delegate optimizer IterationFailure; increase iterlim."));
  if(res.status==cppunivariate::LgammaStatus::TransformFailure)
    return done(fail("lgamma MLE failed: transformed sufficient statistics or fitted coefficients cannot be resolved in double precision."));
  if(res.status==cppunivariate::LgammaStatus::Degenerate)
    return done(nomle("The loggamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows."));
  if(!std::isfinite(res.shapelog) || !std::isfinite(res.ratelog) || !std::isfinite(res.logLik))
    return done(fail("Loggamma MLE failed"));
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shapelog",res.shapelog},{"ratelog",res.ratelog}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
