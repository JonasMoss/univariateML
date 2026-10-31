// Model-only root-n mode Wald interval; endpoints are KNOWN constants.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
#include "mle.hpp"
namespace cppunivariate {
inline InferenceInterval triang_mode_wald_interval(span x,double a,double b,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha) || !(std::isfinite(a) && std::isfinite(b) && a<b))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  const auto f=triang_mle(x,1e-12,100,a,b,NAN);
  if(f.status!=TriangStatus::Ok || !(f.c>a && f.c<b)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double se=std::sqrt((f.c-a)/x.size())*std::sqrt(b-f.c);
  if(!(se*se>0 && std::isfinite(se*se))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return inference_wald_interval(f.c,{InferenceStatus::Ok,se*se,se},special::qnorm(alpha/2,0,1,false,false));
}

struct TriangModeTests {
  ScalarTestResult score{InferenceStatus::NumericalFailure,NAN};
  ScalarTestResult likelihood_ratio{InferenceStatus::NumericalFailure,NAN};
};
// Pointwise tests only: do not assume connected inverted sets. Caller chooses
// asymptotic chi-square or independently simulated null critical values.
inline TriangModeTests triang_mode_tests(span x,double a,double b,double null_c) {
  TriangModeTests out;
  if(!(std::isfinite(a) && std::isfinite(b) && a<null_c && null_c<b)) {
    out.score.status=out.likelihood_ratio.status=InferenceStatus::InvalidParameter;return out;
  }
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) {out.score.status=out.likelihood_ratio.status=s;return out;}
  double score=0;
  for(double v:x) {
    if(!(v>a && v<b) || v==null_c) {
      out.score.status=out.likelihood_ratio.status=InferenceStatus::InvalidObservation;return out;
    }
    score+=v<null_c?-1/(null_c-a):1/(b-null_c);
  }
  const double z=score*std::sqrt((null_c-a)/x.size())*std::sqrt(b-null_c);
  out.score=std::isfinite(z*z)?ScalarTestResult{InferenceStatus::Ok,z*z}:
    ScalarTestResult{InferenceStatus::NumericalFailure,NAN};
  const auto f=triang_mle(x,1e-12,100,a,b,NAN),g=triang_mle(x,1e-12,100,a,b,null_c);
  if(f.status==TriangStatus::Ok && g.status==TriangStatus::Ok)
    out.likelihood_ratio=inference_lr(f.logLik,g.logLik);
  return out;
}
} // namespace cppunivariate
