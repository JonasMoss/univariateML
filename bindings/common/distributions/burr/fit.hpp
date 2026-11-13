#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/burr/mle.hpp"
namespace umvl_binding {
inline FitResult fit_burr(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, double gradtol = 1e-8, int iterlim = 1000, Held shape10 = {}, Held shape20 = {}, Held scale0 = {}, double reltol = 1e-12, double abstol = 1.220703e-4) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return invalid_argument(fail("`shape1` must be positive and finite."));
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return invalid_argument(fail("`shape2` must be positive and finite."));
 if(scale.supplied && (!(scale.value>0)||!std::isfinite(scale.value))) return invalid_argument(fail("burr requires a positive finite fixed scale."));

 auto start = [](const Held &v) {
   return v.supplied ? v.value : NAN;
 };
 auto res = (shape1.supplied || shape2.supplied) ?
 cppunivariate::burr_mle_holds(x,start(shape1),start(shape2),start(scale),gradtol,iterlim) : scale.supplied ?
 cppunivariate::burr_mle_fixed_scale(x,start(scale),abstol,iterlim) :
 cppunivariate::burr_mle(x,gradtol,iterlim,
 start(shape10),start(shape20),start(scale0),reltol);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if (!out.error.empty() && o.kind == cppunivariate::OutcomeKind::Estimate && out.error_kind != "tie") {
      if (res.status == cppunivariate::BurrStatus::Ok)
        return overridden(std::move(out), static_cast<int>(res.status));
      // A legacy classifier can suppress a tie kind using caller-only information.
      return with_outcome(std::move(out), cppunivariate::FitOutcome::failure(cppunivariate::OutcomeReason::NoConvergence), static_cast<int>(res.status));
    }
    return with_outcome(std::move(out), o, static_cast<int>(res.status));
  };
 if (res.status == cppunivariate::BurrStatus::Unbounded) return done(stop_failure(fail("Likelihood is unbounded for this constant sample; no MLE."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::BurrStatus::AllGeOne) return done(stop_failure(fail("The fixed-scale burr MLE does not exist when all observations >= scale. Use `mlpareto` to fit this model with endpoint scale."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},"pareto"));
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) return done(stop_failure(fail("The burr likelihood is maximised at a limit. Use `mlweibull` to fit this model."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},"weibull"));
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) return done(stop_failure(fail("The burr likelihood is maximised at a limit. Use `mlpareto` to fit this model."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},"pareto"));
 if (res.status == cppunivariate::BurrStatus::CertificateTie) return done(stop_failure(fail("Burr global certificate unresolved: finite/face tie or sign-uncertain seam; no verified MLE."),res.stop_detail,"tie",{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::BurrStatus::CertificateBudget) return done(stop_failure(fail("Burr global certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::BurrStatus::NumericalFailure) return done(stop_failure(fail("Burr global certificate numerical failure (domain, bracket, nonfinite arithmetic or linear solve); no verified MLE."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::BurrStatus::IterationFailure) return done(stop_failure(fail("Constrained likelihood search failed to converge."),res.stop_detail,outcome_kind(o),{"shape1","shape2","scale"},""));
 if (res.status == cppunivariate::BurrStatus::InvalidInit ||
 !std::isfinite(res.shape1) || !std::isfinite(res.shape2) || !std::isfinite(res.scale))
 return done(fail("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale"));
 std::vector<std::string> fixed;
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 if(scale.supplied) fixed.push_back("scale");
 return done([&]() { FitResult out; out.extra.emplace_back("certified",res.certified?1.:0.); out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
