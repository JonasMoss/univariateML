// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
#include "mle.hpp"
namespace cppunivariate {
namespace inference_detail {
struct BetaModel {
  static constexpr std::size_t count=2;
  double shape1, shape2;
  std::array<std::string,2> names() const { return {"shape1","shape2"}; }
  std::string conditions() const {return "shape1, shape2 > 0 finite; 0 < x < 1; independent data";}
  bool valid() const {return shape1>0 && shape2>0 && std::isfinite(shape1) && std::isfinite(shape2);}
  bool support(double x) const {return x>0 && x<1 && std::isfinite(x);}
  std::array<double,4> fisher() const { const double t=special::trigamma(shape1+shape2);
    return {beta_detail::tri_difference(shape1,shape2),-t,-t,beta_detail::tri_difference(shape2,shape1)}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    s={std::log(x)+beta_detail::psi_increment(shape1,shape2),std::log1p(-x)+beta_detail::psi_increment(shape2,shape1)};
    h=fisher();
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult beta_inference(span x, double shape1, double shape2,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::BetaModel{shape1, shape2},free_mask,emit);
}
} // namespace cppunivariate

#include "mle.hpp"
namespace cppunivariate {
inline TargetInferenceResult beta_mean_inference(span x,double null_mean,
    CovarianceKind kind=CovarianceKind::Fisher) {
  TargetInferenceResult failure;failure.status=InferenceStatus::NumericalFailure;
  if(!(null_mean>0 && null_mean<1 && std::isfinite(null_mean))) {failure.status=InferenceStatus::InvalidParameter;return failure;}
  const auto f=beta_mle(x,1e-11,2000);
  if(f.status!=BetaStatus::Ok) return failure;
  double l=0,r=0;for(auto v:x){l+=std::log(v);r+=std::log1p(-v);}l/=x.size();r/=x.size();
  // Along the positive ray (a,b)=k*(m,1-m), strict concavity of the beta
  // exponential-family likelihood gives a strictly decreasing k score.
  const auto root=inference_detail::positive_concave_root([&](double k) {
    return null_mean*(l+beta_detail::psi_increment(null_mean*k,(1-null_mean)*k))+
      (1-null_mean)*(r+beta_detail::psi_increment((1-null_mean)*k,null_mean*k));
  });
  if(root.status!=InferenceStatus::Ok) return failure;
  const double a=null_mean*root.statistic,b=(1-null_mean)*root.statistic;
  const auto c=beta_mle(x,1e-11,2000,a,b);
  if(c.status!=BetaStatus::Ok) return failure;
  std::array<double,inference_max_parameters> g{};const double sum=f.alpha+f.beta;
  g[0]=(f.beta/sum)/sum;g[1]=-(f.alpha/sum)/sum;
  return inference_detail::target_result(f.alpha/sum,null_mean,g,beta_inference(x,f.alpha,f.beta),
    beta_inference(x,a,b),f.logLik,c.logLik,kind);
}
inline InferenceInterval beta_mean_lr_interval(span x,double critical) {
  const auto f=beta_mle(x,1e-11,2000);
  if(f.status!=BetaStatus::Ok) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return inference_detail::profile_lr_interval(std::log(f.alpha/f.beta),
    [&](double t){return beta_mean_inference(x,t).likelihood_ratio;},
    [](double t){return t<0?std::exp(t)/(1+std::exp(t)):1/(1+std::exp(-t));},critical);
}
} // namespace cppunivariate
