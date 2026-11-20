#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/invburr/mle.hpp"
namespace umvl_binding {
inline FitResult fit_invburr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double tol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return invalid_argument(fail("`shape1` must be positive and finite."));
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return invalid_argument(fail("`shape2` must be positive and finite."));
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return invalid_argument(fail("invburr requires a positive finite fixed scale."));

 auto start = [](const Held &v) {
   return v.supplied ? v.value : NAN;
 };
 auto res = (shape1.supplied || shape2.supplied) ?
 cppunivariate::invburr_mle_holds(x,start(shape1),start(shape2),start(scale),tol,iterlim) : scale.supplied ?
 cppunivariate::invburr_mle_fixed_scale(x,start(scale),tol,iterlim) :
 cppunivariate::invburr_mle(x,tol,iterlim);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (res.status == cppunivariate::InvBurrStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"scale",scale_arg.supplied}});
  };
 if (res.status == cppunivariate::InvBurrStatus::Unbounded) return done(stop_failure(fail("Likelihood is unbounded for this constant sample; no MLE."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::InvBurrStatus::AllLeOne) return done(stop_failure(fail("The fixed-scale invburr MLE does not exist when all observations <= scale. Use `mlpower` to fit this model with endpoint scale."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"power"));
 if (res.status == cppunivariate::InvBurrStatus::InvWeibullBoundary) return done(stop_failure(fail("The invburr likelihood is maximised at a limit. Use `mlinvweibull` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"invweibull"));
 if (res.status == cppunivariate::InvBurrStatus::PowerBoundary) return done(stop_failure(fail("The invburr likelihood is maximised at a limit. Use `mlpower` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},"power"));
 if (res.status == cppunivariate::InvBurrStatus::CertificateTie) return done(comparison_payload(stop_failure(fail("invburr certificate unresolved at a tie or sign-uncertain seam; no verified MLE."),res.stop_detail,PayloadKind::Comparison,{"shape1","shape2","scale"},""),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"scale",scale_arg}}));
 if (res.status == cppunivariate::InvBurrStatus::CertificateBudget) return done(stop_failure(fail("invburr certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::InvBurrStatus::NumericalFailure) return done(stop_failure(fail("invburr certificate numerical failure; no verified MLE."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::InvBurrStatus::IterationFailure) return done(stop_failure(fail("Constrained likelihood search failed to converge."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::InvBurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 return done(fail("invburr MLE requires positive finite nonconstant data and a positive finite fixed scale"));
 std::vector<std::string> fixed;
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 if(scale.supplied) fixed.push_back("scale");
 return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
