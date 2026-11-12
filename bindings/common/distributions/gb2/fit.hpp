#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gb2/mle.hpp"
namespace umvl_binding {
inline FitResult fit_gb2(cppunivariate::span x, Held shape1_arg = {}, Held shape2_arg = {}, Held shape3_arg = {}, Held scale_arg = {}, double gradtol = 1e-10, int iterlim = 300) {

  if (shape1_arg.supplied && (!(shape1_arg.value > 0) || !std::isfinite(shape1_arg.value))) return failure("`shape1` must be a positive finite number.");
  double shape1=shape1_arg.supplied ? shape1_arg.value : NAN;
  if (shape2_arg.supplied && (!(shape2_arg.value > 0) || !std::isfinite(shape2_arg.value))) return failure("`shape2` must be a positive finite number.");
  double shape2=shape2_arg.supplied ? shape2_arg.value : NAN;
  if (shape3_arg.supplied && (!(shape3_arg.value > 0) || !std::isfinite(shape3_arg.value))) return failure("`shape3` must be a positive finite number.");
  double shape3=shape3_arg.supplied ? shape3_arg.value : NAN;
  if (scale_arg.supplied && (!(scale_arg.value > 0) || !std::isfinite(scale_arg.value))) return failure("`scale` must be a positive finite number.");
  double scale=scale_arg.supplied ? scale_arg.value : NAN;
  auto r=cppunivariate::gb2_mle(cppunivariate::span{x.data(),x.size()},gradtol,iterlim,shape1,shape2,shape3,scale);
  if(r.stop_detail.accuracy_stop!=cppunivariate::AccuracyStop::None)return stop_failure(failure("gb2 numerical accuracy failure: requested coefficient accuracy cannot be established."),r.stop_detail,"numerical",{"shape1","shape2","shape3","scale"});
  if(r.accuracy_failure)return stop_failure(failure("gb2 numerical accuracy failure: final Newton refinement did not converge to the requested coefficient tolerance; increase iterlim or inspect conditioning."),r.stop_detail,"numerical",{"shape1","shape2","shape3","scale"});
  switch(r.status) {
    case cppunivariate::Gb2Status::Ok: break;
    case cppunivariate::Gb2Status::WeibullBoundary: return stop_failure(failure("GB2 selects a limiting model. Use `mlweibull`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"weibull");
    case cppunivariate::Gb2Status::InvWeibullBoundary: return stop_failure(failure("GB2 selects a limiting model. Use `mlinvweibull`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"invweibull");
    case cppunivariate::Gb2Status::CertificateTie: return stop_failure(failure("GB2 certified delegate tie stop; no verified maximum."),r.stop_detail,"tie",{"shape1","shape2","shape3","scale"},"");
    case cppunivariate::Gb2Status::CertificateBudget: return stop_failure(failure("GB2 certified delegate budget stop; no verified maximum. Raising iterlim may resolve this stop."),r.stop_detail,"budget",{"shape1","shape2","shape3","scale"},"");
    case cppunivariate::Gb2Status::NumericalFailure: return stop_failure(failure("GB2 certified delegate numerical stop; no verified maximum."),r.stop_detail,"numerical",{"shape1","shape2","shape3","scale"},"");
    case cppunivariate::Gb2Status::TrGammaBoundary: return stop_failure(failure("GB2 selects a limiting model. Use `mltrgamma`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"trgamma");
    case cppunivariate::Gb2Status::InvTrGammaBoundary: return stop_failure(failure("GB2 selects a limiting model. Use `mlinvtrgamma`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"invtrgamma");
    case cppunivariate::Gb2Status::LognormalBoundary: return failure("GB2 selects a limiting model. Use `mllnorm`.");
    case cppunivariate::Gb2Status::ParetoBoundary:
      if(scale_arg.supplied) return stop_failure(failure("GB2 selects a Pareto limit. Use `mlpareto(x, b = scale)` with the endpoint held."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"pareto");
      return stop_failure(failure("GB2 selects a limiting model. Use `mlpareto`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"pareto");
    case cppunivariate::Gb2Status::PowerBoundary:
      if(scale_arg.supplied) return stop_failure(failure("GB2 selects a power limit. Use `mlpower(x, alpha = scale)` with the upper endpoint held."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"power");
      return stop_failure(failure("GB2 selects a limiting model. Use `mlpower`."),r.stop_detail,"limit",{"shape1","shape2","shape3","scale"},"power");
    case cppunivariate::Gb2Status::DoubleParetoBoundary: return failure("GB2 selects the double Pareto limit; this package has no double Pareto fitter.");
    case cppunivariate::Gb2Status::InvalidData: return failure("GB2 requires positive finite nonconstant observations; constant data have an unbounded free likelihood.");
    case cppunivariate::Gb2Status::InvalidFixed: return failure("GB2 fixed parameters must be positive and finite.");
    case cppunivariate::Gb2Status::ScaleOutOfRange: return failure("GB2 scale is outside the representable range.");
    default: return stop_failure(failure("GB2 search unresolved: no verified interior maximum; increase iterlim or inspect the limiting models."),r.stop_detail,"unresolved",{"shape1","shape2","shape3","scale"},"");
  }
  FitResult out;
  out.extra.push_back({"certified",r.certified?1.:0.});
  out.estimates.emplace_back("shape1",r.shape1); if(shape1_arg.supplied) out.fixed.push_back("shape1");
  out.estimates.emplace_back("shape2",r.shape2); if(shape2_arg.supplied) out.fixed.push_back("shape2");
  out.estimates.emplace_back("shape3",r.shape3); if(shape3_arg.supplied) out.fixed.push_back("shape3");
  out.estimates.emplace_back("scale",r.scale); if(scale_arg.supplied) out.fixed.push_back("scale");
  out.logLik=r.logLik; out.iterations=r.iterations; return out;
}
}
