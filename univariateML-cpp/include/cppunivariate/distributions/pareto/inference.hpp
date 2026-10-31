// Exact shape pivot, lower endpoint known or fitted as the sample minimum.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
inline InferenceInterval pareto_shape_exact_interval(span x,double alpha=.05,double known_b=NAN) {
  if(!inference_detail::valid_alpha(alpha) || (!std::isnan(known_b) && !(known_b>0 && std::isfinite(known_b))))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  if(std::isnan(known_b) && x.size()<2) return {InferenceStatus::InvalidParameter,NAN,NAN};
  double b=known_b;
  if(std::isnan(b)) b=*std::min_element(x.begin(),x.end());
  if(!(b>0)) return {InferenceStatus::InvalidObservation,NAN,NAN};
  double t=0;
  for(double v:x) {
    if(v<b) return {InferenceStatus::InvalidObservation,NAN,NAN};
    // Difference of logs avoids overflowing v/b; log1p retains close spacings.
    const double d=(v-b)/b;
    t+=std::isfinite(d)?std::log1p(d):std::log(v)-std::log(b);
  }
  if(!(t>0 && std::isfinite(t))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double shape=static_cast<double>(x.size())-(std::isnan(known_b)?1:0);
  return inference_detail::positive_interval(special::qgamma(alpha/2,shape,1,true,false)/t,
    special::qgamma(alpha/2,shape,1,false,false)/t);
}
} // namespace cppunivariate
