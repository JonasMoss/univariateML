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
  if (!min_arg.supplied) return failure("Estimating the threshold is not supported: endpoint singularities or unattained limit models prevent an unrestricted MLE (P0, A4).");
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
    case cppunivariate::Gb2Status::TrGammaBoundary: return fail("GB2 selects a limiting model. Use `mltrgamma(x - min)`.");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: return fail("GB2 selects a limiting model. Use `mlinvtrgamma(x - min)`.");
    case cppunivariate::Gb2Status::LognormalBoundary: return fail("GB2 selects a limiting model. Use `mllnorm(x - min)`.");
    case cppunivariate::Gb2Status::ParetoBoundary:
      if(scale.supplied) return fail("Feller-Pareto selects a shifted Pareto with its endpoint held at min + scale; no held-endpoint Pareto fitter is available.");
      return fail("GB2 selects a limiting model. Use `mlpareto(x - min)`.");
    case cppunivariate::Gb2Status::PowerBoundary:
      if(scale.supplied) return fail("Feller-Pareto selects a shifted power distribution on (min, min + scale) with both endpoints held; no fitter for this held-upper-endpoint limit is available.");
      return fail("GB2 selects a limiting model. Use `mlpower(x - min)`.");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: return fail("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: return fail("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: return fail("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: return fail("GB2 scale is outside the representable range.");
    default: return fail("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models.");
  }
  return [&]() { FitResult out; out.warnings = warnings; out.estimates = {{"shape1",res.shape1},{"shape2",res.shape2},{"shape3",res.shape3},{"scale",res.scale},{"min",min}}; out.logLik = res.logLik; out.iterations = res.iterations; out.has_iterations = true; out.fixed = fixed; out.has_fixed = true; return out; }();

}
}
