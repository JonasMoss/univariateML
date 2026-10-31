#pragma once
#include "../../fit.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
namespace umvl_binding {
inline FitResult fit_gamma(cppunivariate::span x, Held shape_arg = {}, Held scale_arg = {}, double abstol = 1.220703e-4, int iterlim = 100) {

  if (shape_arg.supplied && (!(shape_arg.value > 0) || !std::isfinite(shape_arg.value))) return failure("Held `shape` must be positive and finite.");
  double shape=shape_arg.supplied ? shape_arg.value : NAN;
  if (scale_arg.supplied && (!(scale_arg.value > 0) || !std::isfinite(scale_arg.value))) return failure("Held `scale` must be positive and finite.");
  double scale=scale_arg.supplied ? scale_arg.value : NAN;
  auto r=cppunivariate::gamma_mle(cppunivariate::span{x.data(),x.size()},abstol,iterlim,shape,scale);
  if (r.status == cppunivariate::GammaStatus::InvalidInput) {
    // Retain the empty-input diagnostic exposed by the existing JS API.
    if (x.empty()) return failure("Gamma MLE failed");
    return failure("Gamma requires nonempty positive finite data and valid held parameters.");
  }
  if(r.status==cppunivariate::GammaStatus::TransformFailure) return failure("Gamma transformed observations exceed double precision.");
  if(r.status==cppunivariate::GammaStatus::IterationFailure) return failure("Gamma delegate optimizer IterationFailure; increase iterlim.");
  if(r.status==cppunivariate::GammaStatus::Degenerate) return failure("The gamma maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as the shape grows.");
  if(!std::isfinite(r.shape)||!std::isfinite(r.scale)||!std::isfinite(r.logLik)) return failure("Gamma MLE failed");
  FitResult out;
  out.estimates.emplace_back("shape",r.shape); if(shape_arg.supplied) out.fixed.push_back("shape");
  out.estimates.emplace_back("scale",r.scale); if(scale_arg.supplied) out.fixed.push_back("scale");
  out.logLik=r.logLik; out.iterations=r.iterations; return out;
}
}
