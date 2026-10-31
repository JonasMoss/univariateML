// Exact variance inference with mean fitted, or supplied as a known constant.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
inline InferenceInterval norm_variance_exact_interval(span x,double alpha=.05,double known_mean=NAN) {
  if(!inference_detail::valid_alpha(alpha) || (!std::isnan(known_mean) && !std::isfinite(known_mean)))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  if(std::isnan(known_mean) && x.size()<2) return {InferenceStatus::InvalidParameter,NAN,NAN};
  // Centered online sum of squares avoids cancellation from sum(x^2)-n*xbar^2.
  double mean=0,ss=0;std::size_t k=0;
  for(double v:x) {
    ++k;
    if(std::isnan(known_mean)) {const double d=v-mean;mean+=d/k;ss+=d*(v-mean);}
    else {const double d=v-known_mean;ss+=d*d;}
  }
  if(!(ss>0 && std::isfinite(ss))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double df=x.size()-(std::isnan(known_mean)?1.:0.);
  return inference_detail::positive_interval(ss/special::qgamma(alpha/2,df/2,2,false,false),
    ss/special::qgamma(alpha/2,df/2,2,true,false));
}
} // namespace cppunivariate
