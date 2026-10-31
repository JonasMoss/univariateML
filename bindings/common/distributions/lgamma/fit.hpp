#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/lgamma/mle.hpp"
#include <limits>
#include <cmath>
#include <algorithm>
#include <vector>
namespace umvl_binding {
inline FitResult fit_lgamma(cppunivariate::span x, Held shapelog_arg = {}, Held ratelog_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shapelog = shapelog_arg;
  const Held ratelog = ratelog_arg;

  std::vector<std::string> fixed;
  const double shapelog_held = !shapelog.supplied ? std::numeric_limits<double>::quiet_NaN() : shapelog.value;
  if (shapelog.supplied) {
    if (!(std::isfinite(shapelog_held) && shapelog_held > 0.0)) return fail("Held `shapelog` must be positive and finite.");
    fixed.push_back("shapelog");
  }
  const double ratelog_held = !ratelog.supplied ? std::numeric_limits<double>::quiet_NaN() : ratelog.value;
  if (ratelog.supplied) {
    if (!(std::isfinite(ratelog_held) && ratelog_held > 0.0)) return fail("Held `ratelog` must be positive and finite.");
    fixed.push_back("ratelog");
  }

  const auto res=cppunivariate::lgamma_mle(x,abstol,iterlim,shapelog_held,ratelog_held);
  if(res.status==cppunivariate::LgammaStatus::IterationFailure)
    return fail("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if(res.status==cppunivariate::LgammaStatus::TransformFailure)
    return fail("lgamma MLE failed: transformed observations exceed double precision.");
  if(res.status==cppunivariate::LgammaStatus::Degenerate)
    return fail("The loggamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows.");
  if(!std::isfinite(res.shapelog) || !std::isfinite(res.ratelog) || !std::isfinite(res.logLik))
    return fail("Loggamma MLE failed");
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shapelog",res.shapelog},{"ratelog",res.ratelog}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
