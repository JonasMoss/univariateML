#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
#include <cmath>
#include <algorithm>
namespace umvl_binding {
inline FitResult fit_fpareto(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, Held min_arg = {}, double reltol = 1e-7, int iterlim = 1000) {
  std::vector<std::string> warnings;
  const auto fail = [&](const std::string& format, const auto&... arguments) { return failure(binding_message(format, arguments...), warnings); };
  const Held shape1 = shape1_arg;
  const Held shape2 = shape2_arg;
  const Held shape3 = shape3_arg;
  const Held scale = scale_arg;
  if (!min_arg.supplied) return failure("Estimating min is not supported. The fully free likelihood is unbounded. Hold min to fit this family; existence with other coefficients held depends on their values.");
  const double min = min_arg.value;

 if (!std::isfinite(min)) return fail("Held bounds must be finite and ordered.");
 for(double v:x) if (!(v>min)) return fail("Observations must lie strictly inside the held support.");
 std::vector<std::string> fixed{"min"};
 if(shape1.supplied) { if(!(shape1.value>0) || !std::isfinite(shape1.value)) return fail("Held parameters must be positive and finite."); fixed.push_back("shape1"); }
 if(shape2.supplied) { if(!(shape2.value>0) || !std::isfinite(shape2.value)) return fail("Held parameters must be positive and finite."); fixed.push_back("shape2"); }
 if(shape3.supplied) { if(!(shape3.value>0) || !std::isfinite(shape3.value)) return fail("Held parameters must be positive and finite."); fixed.push_back("shape3"); }
 if(scale.supplied) { if(!(scale.value>0) || !std::isfinite(scale.value)) return fail("Held parameters must be positive and finite."); fixed.push_back("scale"); }
 auto res=cppunivariate::fpareto_mle(x,min,reltol,iterlim,shape1.supplied ? shape1.value : std::numeric_limits<double>::quiet_NaN(),shape2.supplied ? shape2.value : std::numeric_limits<double>::quiet_NaN(),shape3.supplied ? shape3.value : std::numeric_limits<double>::quiet_NaN(),scale.supplied ? scale.value : std::numeric_limits<double>::quiet_NaN());
  switch(res.status) {
    case cppunivariate::Gb2Status::Ok: break;
    case cppunivariate::Gb2Status::WeibullBoundary: return stop_failure(fail("GB2 selects a limiting model. Use `mlweibull(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"weibull");
    case cppunivariate::Gb2Status::InvWeibullBoundary: return stop_failure(fail("GB2 selects a limiting model. Use `mlinvweibull(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"invweibull");
    case cppunivariate::Gb2Status::CertificateTie: return stop_failure(fail("GB2 certified delegate tie stop; no verified maximum."),res.stop_detail,"tie",{"shape1","shape2","shape3","scale","min"},"");
    case cppunivariate::Gb2Status::CertificateBudget: return stop_failure(fail("GB2 certified delegate budget stop; no verified maximum. Raising iterlim may resolve this stop."),res.stop_detail,"budget",{"shape1","shape2","shape3","scale","min"},"");
    case cppunivariate::Gb2Status::NumericalFailure: return stop_failure(fail("GB2 certified delegate numerical stop; no verified maximum."),res.stop_detail,"numerical",{"shape1","shape2","shape3","scale","min"},"");
    case cppunivariate::Gb2Status::TrGammaBoundary: return stop_failure(fail("GB2 selects a limiting model. Use `mltrgamma(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"trgamma");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: return stop_failure(fail("GB2 selects a limiting model. Use `mlinvtrgamma(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"invtrgamma");
    case cppunivariate::Gb2Status::LognormalBoundary: return fail("GB2 selects a limiting model. Use `mllnorm(x - min)`.");
    case cppunivariate::Gb2Status::ParetoBoundary:
      if(scale.supplied) return stop_failure(fail("Feller-Pareto selects a shifted Pareto. Use `mlpareto(x - min, b = scale)` with its endpoint held at min + scale."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"pareto");
      return stop_failure(fail("GB2 selects a limiting model. Use `mlpareto(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"pareto");
    case cppunivariate::Gb2Status::PowerBoundary:
      if(scale.supplied) return stop_failure(fail("Feller-Pareto selects a shifted power distribution. Use `mlpower(x - min, alpha = scale)` to retain both endpoints held."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"power");
      return stop_failure(fail("GB2 selects a limiting model. Use `mlpower(x - min)`."),res.stop_detail,"limit",{"shape1","shape2","shape3","scale","min"},"power");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: return fail("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: return fail("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: return fail("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: return fail("GB2 scale is outside the representable range.");
    default: return stop_failure(fail("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models."),res.stop_detail,"unresolved",{"shape1","shape2","shape3","scale","min"},"");
  }
  return [&]() { FitResult out; if(!res.certified)out.extra.push_back({"certified",0}); out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"shape3",res.shape3},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
