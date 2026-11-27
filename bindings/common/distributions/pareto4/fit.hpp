#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_pareto4(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held scale_arg = {}, Held min_arg = {}, double tol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held scale = scale_arg;
  const double min = min_arg.supplied ? min_arg.value : NAN;

 if(shape1.supplied && (!(shape1.value>0)||!std::isfinite(shape1.value))) return invalid_argument(fail("`shape1` must be positive and finite."));
 if(shape2.supplied && (!(shape2.value>0)||!std::isfinite(shape2.value))) return invalid_argument(fail("`shape2` must be positive and finite."));

 if (min_arg.supplied && !std::isfinite(min)) return invalid_argument(fail("Held bounds must be finite and ordered."));
 std::vector<std::string> fixed;
 if(min_arg.supplied)fixed.push_back("min");
 if(scale.supplied) { if(!(scale.value>0) || !std::isfinite(scale.value)) return invalid_argument(fail("Held parameters must be positive and finite.")); fixed.push_back("scale"); }
 if(shape1.supplied) fixed.push_back("shape1");
 if(shape2.supplied) fixed.push_back("shape2");
 auto res=cppunivariate::pareto4_mle(x,min,tol,iterlim,scale.supplied ? scale.value : std::numeric_limits<double>::quiet_NaN(),shape1.supplied?shape1.value:NAN,shape2.supplied?shape2.value:NAN);
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"scale",scale_arg.supplied},{"min",min_arg.supplied}});
  };
  if (o.reason==cppunivariate::OutcomeReason::Unsupported) return done(failure("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values."));
  if(o.reason==cppunivariate::OutcomeReason::Unattained)
    return done(stop_failure(fail("Pareto IV has a finite unattained supremum as scale decreases to zero at the critical endpoint multiplicity."),res.stop_detail,outcome_payload(o),{}));
  if(res.incompatible_support)return done(fail("Observations must lie strictly inside the held support."));
  if(res.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return done(stop_failure(failure("pareto4 numerical accuracy failure: requested coefficient accuracy cannot be established."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"}));
 if (res.status == cppunivariate::BurrStatus::Unbounded && std::isnan(res.min))
   return done(stop_failure(fail("The Pareto IV likelihood is unbounded with these free-threshold holds; no MLE."),res.stop_detail,outcome_payload(o),{}));
 if (res.status == cppunivariate::BurrStatus::Unbounded) return done(stop_failure(fail("Likelihood is unbounded for this constant sample; no MLE."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},""));
 if (res.status == cppunivariate::BurrStatus::AllGeOne) return done(stop_failure(fail("The fixed-scale Pareto IV MLE does not exist when all observations >= min + scale. The limit is a shifted Pareto with its endpoint held at min + scale; use `mlpareto(x - min, b = scale)` to fit that held endpoint."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"pareto":""));
 if (res.status == cppunivariate::BurrStatus::WeibullBoundary) return done(stop_failure(fail("The burr likelihood is maximised at a limit. Use `mlweibull(x - min)` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"weibull":""));
 if (res.status == cppunivariate::BurrStatus::ParetoBoundary) {
   auto result=stop_failure(fail("The burr likelihood is maximised at a limit. Use `mlpareto(x - min)` to fit this model."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"pareto":"");
   if(res.limit_endpoint_low!=0) {
     result.limit_estimates.emplace_back("b_low",res.limit_endpoint_low);
     for(auto& candidate:result.candidates)
       if(candidate.model==result.model)candidate.estimates.emplace_back("b_low",res.limit_endpoint_low);
   }
   return done(std::move(result));
 }

 if (res.status == cppunivariate::BurrStatus::CertificateTie) return done(comparison_payload(stop_failure(fail("Pareto IV certificate unresolved at a tie or sign-uncertain seam; no verified MLE."),res.stop_detail,PayloadKind::Comparison,{"shape1","shape2","scale","min"},""),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"scale",scale_arg},{"min",min_arg}}));
 if (res.status == cppunivariate::BurrStatus::CertificateBudget) return done(stop_failure(fail("Pareto IV certificate budget exhausted; no verified MLE. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},""));
 if (res.status == cppunivariate::BurrStatus::NumericalFailure) return done(stop_failure(fail("Pareto IV certificate numerical failure; no verified MLE."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},""));
 if (res.status == cppunivariate::BurrStatus::IterationFailure) return done(stop_failure(fail("Pareto IV delegate search failed to converge."),res.stop_detail,outcome_payload(o),{"shape1","shape2","scale","min"},""));
 if (res.status == cppunivariate::BurrStatus::InvalidInit)
 return done(fail("burr MLE requires positive finite nonconstant data and positive starts and a positive finite fixed scale"));
 return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"scale",res.scale},{"min",res.min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
