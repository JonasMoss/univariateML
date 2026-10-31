// Median inference; scale is fitted, never treated as known.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
#include "mle.hpp"
namespace cppunivariate {
inline InferenceInterval laplace_location_wald_interval(span x,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  const auto f=laplace_mle(x);
  if(f.status!=LaplaceStatus::Ok) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double se=f.sigma/std::sqrt(static_cast<double>(x.size()));
  if(!(se*se>0 && std::isfinite(se*se))) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return inference_wald_interval(f.mu,{InferenceStatus::Ok,se*se,se},special::qnorm(alpha/2,0,1,false,false));
}
// Distribution-free for iid continuous data with a unique median. Largest k
// with 2 Pr[Bin(n,1/2)<=k-1] <= alpha; [X_(k),X_(n-k+1)].
// k=0 returns the whole real line, with exact coverage 1.
inline InferenceInterval median_sign_interval(span x,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto s=inference_detail::finite_sample(x);
  if(s!=InferenceStatus::Ok) return {s,NAN,NAN};
  std::vector<double> y(x.begin(),x.end()); std::sort(y.begin(),y.end());
  std::size_t lo=0,hi=x.size()/2+1;
  // Monotone binomial CDF: binary search is certified by monotonicity.
  while(hi-lo>1) {
    const auto k=lo+(hi-lo)/2;
    const double tail=special::ibeta(static_cast<double>(x.size()-k+1),static_cast<double>(k),.5);
    if(!std::isfinite(tail)) return {InferenceStatus::NumericalFailure,NAN,NAN};
    if(tail<=alpha/2) lo=k;else hi=k;
  }
  if(lo==0) return {InferenceStatus::Ok,-INFINITY,INFINITY};
  return {InferenceStatus::Ok,y[lo-1],y[y.size()-lo]};
}
struct MedianSandwichResult {
  InferenceStatus status=InferenceStatus::Ok;
  double median=NAN,density=NAN,bandwidth=NAN;
  ContrastResult uncertainty{InferenceStatus::NumericalFailure,NAN,NAN};
};
// Gaussian KDE, h=1.06*(MAD/log(2))*n^(-1/5). Here MAD is the median
// absolute deviation from the upper sample median. No automatic fallback.
inline MedianSandwichResult median_sandwich(span x) {
  MedianSandwichResult r;
  r.status=inference_detail::finite_sample(x);
  if(r.status!=InferenceStatus::Ok) return r;
  std::vector<double> y(x.begin(),x.end()); std::sort(y.begin(),y.end());
  r.median=y[y.size()/2];
  for(auto& v:y) v=std::abs(v-r.median);
  std::sort(y.begin(),y.end());
  r.bandwidth=1.06*(y[y.size()/2]/std::log(2.0))*std::pow(static_cast<double>(y.size()),-.2);
  if(!(r.bandwidth>0 && std::isfinite(r.bandwidth))) {r.status=InferenceStatus::NumericalFailure;return r;}
  double total=0;
  for(double v:x) {
    const double z=(v-r.median)/r.bandwidth;
    total+=std::exp(-.5*z*z);
  }
  r.density=total/y.size()/r.bandwidth/std::sqrt(2*std::acos(-1.0));
  const double se=.5/r.density/std::sqrt(static_cast<double>(y.size()));
  if(!(r.density>0 && se>0 && std::isfinite(r.density) && std::isfinite(se) && std::isfinite(se*se) && se*se>0)) {
    r.status=InferenceStatus::NumericalFailure;return r;
  }
  r.uncertainty={InferenceStatus::Ok,se*se,se};return r;
}
inline InferenceInterval median_sandwich_interval(span x,double alpha=.05) {
  if(!inference_detail::valid_alpha(alpha)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto r=median_sandwich(x);
  if(r.status!=InferenceStatus::Ok) return {r.status,NAN,NAN};
  return inference_wald_interval(r.median,r.uncertainty,special::qnorm(alpha/2,0,1,false,false));
}
} // namespace cppunivariate
