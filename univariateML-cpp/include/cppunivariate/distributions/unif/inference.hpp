// Marginal exact intervals under iid Uniform(a,b); no joint Wald assertion.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
enum class UniformTarget { Minimum, Maximum, Width };
inline InferenceInterval unif_exact_interval(span x,UniformTarget target,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  if(x.size()<2) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto extremes=std::minmax_element(x.begin(),x.end());
  const double l=*extremes.first,u=*extremes.second,r=u-l,n=x.size();
  if(!(r>0 && std::isfinite(r))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  if(target==UniformTarget::Width) {
    // R/(b-a) ~ Beta(n-1,2).
    return inference_detail::positive_interval(r/special::ibetac_inv(n-1,2,alpha/2),
      r/special::ibeta_inv(n-1,2,alpha/2));
  }
  // (min-a)/R and (b-max)/R each have survival (1+t)^(-(n-1)).
  const double qlo=std::expm1(-std::log1p(-alpha/2)/(n-1));
  const double qhi=std::expm1(-std::log(alpha/2)/(n-1));
  if(target==UniformTarget::Minimum) return inference_detail::finite_interval(l-r*qhi,l-r*qlo);
  if(target==UniformTarget::Maximum) return inference_detail::finite_interval(u+r*qlo,u+r*qhi);
  return {InferenceStatus::InvalidParameter,NAN,NAN};
}
// Target is the unknown endpoint; the opposite endpoint is known. The same
// interval, translated/reflected, also supplies its width interval.
inline InferenceInterval unif_endpoint_known_opposite_interval(span x,bool upper,double opposite,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha) || !std::isfinite(opposite))
    return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  double r=0;
  for(double v:x) {
    const double d=upper?v-opposite:opposite-v;
    if(d<0) return {InferenceStatus::InvalidObservation,NAN,NAN};
    r=std::max(r,d);
  }
  if(!(r>0 && std::isfinite(r))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double lo=r/std::exp(std::log1p(-alpha/2)/x.size());
  const double hi=r/std::exp(std::log(alpha/2)/x.size());
  return upper?inference_detail::finite_interval(opposite+lo,opposite+hi):
    inference_detail::finite_interval(opposite-hi,opposite-lo);
}
} // namespace cppunivariate
