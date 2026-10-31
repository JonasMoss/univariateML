// Fixed origin zero, rate free. Exact Gamma(n,1) pivot rate*sum(x).
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
inline InferenceInterval exp_rate_exact_interval(span x,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  double total=0;
  for(double v:x) {
    if(v<0) return {InferenceStatus::InvalidObservation,NAN,NAN};
    total+=v;
  }
  if(!(total>0 && std::isfinite(total))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return inference_detail::positive_interval(special::qgamma(alpha/2,x.size(),1,true,false)/total,
    special::qgamma(alpha/2,x.size(),1,false,false)/total);
}
} // namespace cppunivariate
