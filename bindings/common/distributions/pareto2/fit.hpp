#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto2/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto2(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, Held min_arg = {}, double tol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape = shape_arg;
  const Held scale = scale_arg;
  const double min = min_arg.supplied?min_arg.value:NAN;

 if(shape.supplied && (!(shape.value>0)||!std::isfinite(shape.value))) return invalid_argument(fail("`shape` must be positive and finite."));
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return invalid_argument(fail("`scale` must be positive and finite."));

 if (min_arg.supplied && !std::isfinite(min)) return invalid_argument(fail("Held bounds must be finite and ordered."));
 std::vector<std::string> fixed;
 if(min_arg.supplied)fixed.push_back("min");
 if(shape.supplied) fixed.push_back("shape");
 if(scale.supplied) fixed.push_back("scale");
 auto res=cppunivariate::pareto2_mle(x,min,tol,iterlim,shape.supplied?shape.value:NAN,scale.supplied?scale.value:NAN);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::Pareto2Status::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape",shape_arg.supplied},{"scale",scale_arg.supplied},{"min",min_arg.supplied}});
  };
  if(res.status==cppunivariate::Pareto2Status::UnsupportedThreshold)return done(fail("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values."));
  if(res.status==cppunivariate::Pareto2Status::Unbounded && !min_arg.supplied)
    return done(stop_failure(fail("The free-threshold Pareto II likelihood is unbounded."),res.stop_detail,outcome_payload(o),{}));
  if(res.status==cppunivariate::Pareto2Status::Unbounded)return done(stop_failure(fail("Pareto II likelihood is unbounded through concentration at its held lower endpoint."),res.stop_detail,outcome_payload(o),{}));
  if(res.status==cppunivariate::Pareto2Status::Unattained)return done(stop_failure(fail("Pareto II has a finite unattained supremum as scale decreases to zero at the critical endpoint multiplicity."),res.stop_detail,outcome_payload(o),{}));
  if(res.status==cppunivariate::Pareto2Status::LimitRepresentationFailure)return done(stop_failure(fail("The exponential limit rate or likelihood cannot be represented in binary64."),res.stop_detail,outcome_payload(o),{"shape","scale","min"}));
  if(res.status==cppunivariate::Pareto2Status::IterationFailure && std::isinf(res.scale) && res.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic)
    return done(stop_failure(fail("Pareto II numerical failure: the fitted scale cannot be represented in binary64."),res.stop_detail,outcome_payload(o),{"shape","scale","min"}));
  if(res.status==cppunivariate::Pareto2Status::AccuracyFailure)return done(stop_failure(fail("Pareto II requested coefficient accuracy cannot be established."),res.stop_detail,outcome_payload(o),{"shape","scale","min"}));
  if (res.status == cppunivariate::Pareto2Status::ExponentialLimit) {
    return done(stop_failure(fail("The Lomax maximum likelihood estimator does not exist for data "
               "whose likelihood is maximized on the exponential face. Use `mlexp(x - min)` to fit an "
               "exponential distribution."),res.stop_detail,outcome_payload(o),{"shape","scale","min"}));
  }
  if(res.status==cppunivariate::Pareto2Status::IterationFailure) return done(stop_failure(fail("Pareto II search failed; no accepted maximum. Raising iterlim may resolve a budget stop."),res.stop_detail,outcome_payload(o),{"shape","scale","min"},""));
  if (res.status != cppunivariate::Pareto2Status::Ok || !std::isfinite(res.scale) || !(res.scale>0) ||
      !std::isfinite(res.shape)) {
    return done(fail("Lomax MLE failed to converge (check dispersion/inputs)"));
  }
  return done([&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape",res.shape},{"scale",res.scale},{"min",res.min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
