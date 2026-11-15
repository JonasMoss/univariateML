#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_nsbeta(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held min_arg = {}, Held max_arg = {}, double tol = 1e-10, int iterlim = 1000) {

  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const double min = min_arg.supplied?min_arg.value:NAN;
  const double max = max_arg.supplied?max_arg.value:NAN;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return invalid_argument(fail("`shape1` must be positive and finite."));
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return invalid_argument(fail("`shape2` must be positive and finite."));

 if (min_arg.supplied && max_arg.supplied && (!std::isfinite(min) || !std::isfinite(max) || !(max>min) || !std::isfinite(max-min))) return invalid_argument(fail("Held bounds must be finite and ordered."));
 std::vector<std::string> fixed{"min", "max"};
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 auto res=cppunivariate::nsbeta_mle(x,min,max,tol,iterlim,shape1.supplied?shape1.value:NAN,shape2.supplied?shape2.value:NAN);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::BetaStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"min",min_arg.supplied},{"max",max_arg.supplied}});
  };
  if(res.unsupported_bounds)return done(fail("Estimating min or max is not supported. A free endpoint with its corresponding shape free or below one gives unbounded likelihood. Other held-shape cases may have a finite MLE but are not computed. Supply both min and max."));

  const auto nomle = [&](const std::string& format, const auto&... arguments) { return stop_failure(failure(binding_message(format, arguments...), warnings), {}, outcome_payload(o), {}); };
  if (res.status == cppunivariate::BetaStatus::InvalidInput) return done(fail("Nonstandard beta requires nonempty finite data strictly inside its held bounds."));
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    return done(nomle("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow."));
  }
  if(res.status==cppunivariate::BetaStatus::IterationFailure) return done(fail("Nonstandard beta delegate search failed to converge."));
  if (!std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.logLik)) {
    return done(fail("Beta MLE failed: the data are too close to a point mass to resolve the shapes in double precision."));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"min",min},{"max",max}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
