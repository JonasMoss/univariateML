#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_fpareto(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, Held min_arg = {}, double tol = 1e-10, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held shape3 = shape3_arg;
  const Held scale = scale_arg;
  const double min = min_arg.supplied?min_arg.value:NAN;

 if (min_arg.supplied && !std::isfinite(min)) return invalid_argument(fail("Held bounds must be finite and ordered."));
 std::vector<std::string> fixed{"min"};
 if(shape1.supplied) { if(!(shape1.value>0) || !std::isfinite(shape1.value)) return invalid_argument(fail("Held parameters must be positive and finite.")); fixed.push_back("shape1"); }
 if(shape2.supplied) { if(!(shape2.value>0) || !std::isfinite(shape2.value)) return invalid_argument(fail("Held parameters must be positive and finite.")); fixed.push_back("shape2"); }
 if(shape3.supplied) { if(!(shape3.value>0) || !std::isfinite(shape3.value)) return invalid_argument(fail("Held parameters must be positive and finite.")); fixed.push_back("shape3"); }
 if(scale.supplied) { if(!(scale.value>0) || !std::isfinite(scale.value)) return invalid_argument(fail("Held parameters must be positive and finite.")); fixed.push_back("scale"); }
 auto res=cppunivariate::fpareto_mle(x,min,tol,iterlim,shape1.supplied ? shape1.value : std::numeric_limits<double>::quiet_NaN(),shape2.supplied ? shape2.value : std::numeric_limits<double>::quiet_NaN(),shape3.supplied ? shape3.value : std::numeric_limits<double>::quiet_NaN(),scale.supplied ? scale.value : std::numeric_limits<double>::quiet_NaN());
  const auto o = cppunivariate::outcome(res);
  const auto done = [&](FitResult out) {
    if(o.kind==cppunivariate::OutcomeKind::Limit && out.limit_estimates.empty())
      out.error+=" The limiting coefficients are outside the representable range.";
    if (res.status == cppunivariate::FparetoStatus::Ok && o.kind == cppunivariate::OutcomeKind::Estimate && !out.error.empty() && out.payload_kind != PayloadKind::Comparison)
      return overridden(std::move(out), static_cast<int>(res.status));
    return with_outcome(std::move(out), o, static_cast<int>(res.status),{{"shape1",shape1_arg.supplied},{"shape2",shape2_arg.supplied},{"shape3",shape3_arg.supplied},{"scale",scale_arg.supplied},{"min",min_arg.supplied}});
  };
  if(res.status==cppunivariate::FparetoStatus::Unbounded)return done(stop_failure(fail("Likelihood is unbounded for this constant sample; no MLE."),res.stop_detail,outcome_payload(o),{}));
  if(res.status==cppunivariate::FparetoStatus::UnsupportedThreshold)return done(fail("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values."));
  switch(res.status) {
    case cppunivariate::FparetoStatus::FaceRefuted:return done(stop_failure(fail("A finite feasible Feller-Pareto point refutes the selected lognormal face; the search has no accepted maximum."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"}));
    case cppunivariate::FparetoStatus::Ok: break;
    case cppunivariate::FparetoStatus::WeibullBoundary: return done(stop_failure(fail("GB2 selects a limiting model. Use `mlweibull(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"weibull":""));
    case cppunivariate::FparetoStatus::InvWeibullBoundary: return done(stop_failure(fail("GB2 selects a limiting model. Use `mlinvweibull(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"invweibull":""));
    case cppunivariate::FparetoStatus::CertificateTie: return done(comparison_payload(stop_failure(fail("GB2 certified delegate tie stop; no verified maximum."),res.stop_detail,PayloadKind::Comparison,{"shape1","shape2","shape3","scale","min"},""),o,cppunivariate::comparison_choice(res),{{"shape1",shape1_arg},{"shape2",shape2_arg},{"shape3",shape3_arg},{"scale",scale_arg},{"min",min_arg}}));
    case cppunivariate::FparetoStatus::CertificateBudget: return done(stop_failure(fail("GB2 certified delegate budget stop; no verified maximum. Raising iterlim may resolve this stop."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},""));
    case cppunivariate::FparetoStatus::NumericalFailure: return done(stop_failure(fail("GB2 certified delegate numerical stop; no verified maximum."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},""));
    case cppunivariate::FparetoStatus::TrGammaBoundary: return done(stop_failure(fail("GB2 selects a limiting model. Use `mltrgamma(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"trgamma":""));
    case cppunivariate::FparetoStatus::InvTrGammaBoundary: return done(stop_failure(fail("GB2 selects a limiting model. Use `mlinvtrgamma(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"invtrgamma":""));
    case cppunivariate::FparetoStatus::LognormalBoundary: return done(stop_failure(fail("GB2 selects a limiting model. Use `mllnorm(x - min)`."),res.stop_detail,outcome_payload(o),{"meanlog","sdlog"}));
    case cppunivariate::FparetoStatus::ParetoBoundary:
      if(scale.supplied) return done(stop_failure(fail("Feller-Pareto selects a shifted Pareto. Use `mlpareto(x - min, b = scale)` with its endpoint held at min + scale."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"pareto":""));
      return done(stop_failure(fail("GB2 selects a limiting model. Use `mlpareto(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"pareto":""));
    case cppunivariate::FparetoStatus::PowerBoundary:
      if(scale.supplied) return done(stop_failure(fail("Feller-Pareto selects a shifted power distribution. Use `mlpower(x - min, alpha = scale)` to retain both endpoints held."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"power":""));
      return done(stop_failure(fail("GB2 selects a limiting model. Use `mlpower(x - min)`."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},res.stop_detail.limit_model==cppunivariate::StopModel::None?"power":""));
    case cppunivariate::FparetoStatus::DoubleParetoBoundary: return done(stop_failure(fail("GB2 selects the double Pareto limit; this package has no double Pareto fitter."),res.stop_detail,outcome_payload(o),{"kink","left_rate","right_rate","min"},"unavailable:shifted-double-pareto"));
    case cppunivariate::FparetoStatus::InvalidData: return done(fail("Feller-Pareto fitting requires nonempty finite observations strictly inside the held support and valid controls."));
    case cppunivariate::FparetoStatus::InvalidFixed: return done(fail("GB2 fixed parameters must be positive and finite."));
    case cppunivariate::FparetoStatus::ScaleOutOfRange: return done(fail("GB2 scale is outside the representable range."));
    default: return done(stop_failure(fail("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models."),res.stop_detail,outcome_payload(o),{"shape1","shape2","shape3","scale","min"},""));
  }
  return done([&]() { FitResult out;  out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"shape3",res.shape3},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }());

}
}
