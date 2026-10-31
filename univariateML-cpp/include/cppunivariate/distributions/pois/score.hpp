// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct PoisModel {
  static constexpr std::size_t count=1;
  double lambda;
  std::array<std::string,2> names() const { return {"lambda"}; }
  std::string conditions() const {return "lambda > 0 finite (zero-rate boundary excluded); nonnegative integer x; independent data";}
  bool valid() const {return lambda>0 && std::isfinite(lambda);}
  bool support(double x) const {return x>=0 && std::isfinite(x) && std::floor(x)==x;}
  std::array<double,4> fisher() const { return {1/lambda,0,0,0}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    s={(x-lambda)/lambda,0}; h={x/lambda/lambda,0,0,0};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult pois_inference(span x, double lambda,
    unsigned free_mask=1, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::PoisModel{lambda},free_mask,emit);
}
// Integer-span counterpart matching the Poisson fitter's input boundary.
template<class Emit = inference_detail::Discard>
inline InferenceResult pois_inference(ispan x, double lambda,
    unsigned free_mask=1, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::PoisModel{lambda},free_mask,emit);
}
} // namespace cppunivariate

#include "mle.hpp"
namespace cppunivariate {
// Garwood remains available at zero total count, outside interior derivatives.
inline InferenceInterval pois_rate_exact_interval(span x,double alpha=.05) {
  if(x.empty()) return {InferenceStatus::EmptySample,NAN,NAN};
  if(!(alpha>0 && alpha<1)) return {InferenceStatus::InvalidParameter,NAN,NAN};
  double total=0;
  for(auto v:x) {
    if(!(v>=0 && std::floor(v)==v && std::isfinite(v)))
      return {InferenceStatus::InvalidObservation,NAN,NAN};
    total+=v;
  }
  if(!std::isfinite(total)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double lo=total==0?0:special::qgamma(alpha/2,total,1,true,false)/x.size();
  const double hi=special::qgamma(alpha/2,total+1,1,false,false)/x.size();
  if(!std::isfinite(lo) || !std::isfinite(hi)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,lo,hi};
}
inline TargetInferenceResult pois_rate_inference(span x,double null_rate,
    CovarianceKind kind=CovarianceKind::Fisher) {
  TargetInferenceResult failure;failure.status=InferenceStatus::NumericalFailure;
  const auto valid=pois_inference(x,null_rate);
  if(valid.status!=InferenceStatus::Ok) {failure.status=valid.status;return failure;}
  double total=0;for(auto v:x) total+=v;
  const double rate=total/x.size();
  if(!(rate>0 && std::isfinite(rate))) return failure;
  double ll=0,cl=0;for(auto v:x) {
    ll+=v*std::log(rate)-rate-special::lgamma(v+1);
    cl+=v*std::log(null_rate)-null_rate-special::lgamma(v+1);
  }
  std::array<double,inference_max_parameters> g{};g[0]=1;
  return inference_detail::target_result(rate,null_rate,g,pois_inference(x,rate),valid,ll,cl,kind);
}
inline InferenceInterval pois_rate_score_interval(span x,double critical) {
  if(!(critical>0 && std::isfinite(critical))) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto valid=pois_inference(x,1.);
  if(valid.status!=InferenceStatus::Ok) return {valid.status,NAN,NAN};
  double total=0;for(auto v:x) total+=v;
  const double root=std::sqrt(critical*(4*total+critical));
  if(!std::isfinite(root)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,(total/x.size())*(2*total/(2*total+critical+root)),
    (2*total+critical+root)/(2*x.size())};
}
inline InferenceInterval pois_rate_lr_interval(span x,double critical) {
  const auto valid=pois_inference(x,1.);
  if(valid.status!=InferenceStatus::Ok) return {valid.status,NAN,NAN};
  double total=0;for(auto v:x) total+=v;
  if(total==0 && critical>0 && std::isfinite(critical)) return {InferenceStatus::Ok,0,critical/(2*x.size())};
  return inference_detail::profile_lr_interval(std::log(total/x.size()),
    [&](double t){return pois_rate_inference(x,t).likelihood_ratio;},
    [](double t){return std::exp(t);},critical);
}
} // namespace cppunivariate
