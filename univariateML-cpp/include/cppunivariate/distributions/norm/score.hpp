// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct NormModel {
  static constexpr std::size_t count=2;
  double mean, sd;
  std::array<std::string,2> names() const { return {"mean","sd"}; }
  std::string conditions() const {return "finite mean; sd > 0 finite; finite real observations; independent data";}
  bool valid() const {return std::isfinite(mean) && sd>0 && std::isfinite(sd);}
  bool support(double x) const {return std::isfinite(x);}
  std::array<double,4> fisher() const {
    const double v=1/sd/sd;
    // Positive mathematical information underflow is an arithmetic failure.
    return {v==0 ? NAN : v,0,0,v==0 ? NAN : 2/sd/sd};
  }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    const double z=(x-mean)/sd, inv=1/sd;
    s={z*inv,(z*z-1)*inv};
    h={inv*inv,2*z*inv*inv,2*z*inv*inv,(3*z*z-1)*inv*inv};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult norm_inference(span x, double mean, double sd,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::NormModel{mean, sd},free_mask,emit);
}
} // namespace cppunivariate

#include "mle.hpp"
namespace cppunivariate {
inline TargetInferenceResult norm_mean_inference(span x,double null_mean,
    CovarianceKind kind=CovarianceKind::Fisher) {
  const auto f=norm_mle(x),c=norm_mle(x,null_mean,NAN);
  if(f.status!=NormStatus::Ok || c.status!=NormStatus::Ok) {
    TargetInferenceResult r;r.status=InferenceStatus::NumericalFailure;return r;
  }
  std::array<double,inference_max_parameters> g{};g[0]=1;
  return inference_detail::target_result(f.mean,null_mean,g,
    norm_inference(x,f.mean,f.sd),norm_inference(x,c.mean,c.sd),f.logLik,c.logLik,kind);
}
inline InferenceInterval norm_mean_exact_interval(span x,double alpha=.05) {
  if(!(alpha>0 && alpha<1) || x.size()<2) return {InferenceStatus::InvalidParameter,NAN,NAN};
  const auto f=norm_mle(x);
  if(f.status!=NormStatus::Ok) return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double q=special::qt(alpha/2,x.size()-1,false,false);
  const double width=q*f.sd/std::sqrt(x.size()-1.);
  if(!std::isfinite(width)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,f.mean-width,f.mean+width};
}
// Normal fitted-variance Rao inversion is bounded only if n > critical.
inline InferenceInterval norm_mean_score_interval(span x,double critical) {
  const auto f=norm_mle(x);
  if(f.status!=NormStatus::Ok || !(critical>0 && x.size()>critical))
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double w=f.sd*std::sqrt(critical/(x.size()-critical));
  if(!std::isfinite(w) || !std::isfinite(f.mean-w) || !std::isfinite(f.mean+w))
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,f.mean-w,f.mean+w};
}
inline InferenceInterval norm_mean_lr_interval(span x,double critical) {
  const auto f=norm_mle(x);
  if(f.status!=NormStatus::Ok || !(critical>0 && std::isfinite(critical)))
    return {InferenceStatus::NumericalFailure,NAN,NAN};
  const double w=f.sd*std::sqrt(std::expm1(critical/x.size()));
  if(!std::isfinite(w)) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return {InferenceStatus::Ok,f.mean-w,f.mean+w};
}
} // namespace cppunivariate
