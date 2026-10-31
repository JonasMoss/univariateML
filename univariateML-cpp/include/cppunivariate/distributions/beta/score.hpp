// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
// Extend the held-shape fitter's digamma-increment expansion through z^-8.
// 1-q^n is evaluated as (1-q)*sum(q^k), preserving small increments.
// These are asymptotic numerical formulas, not certified uniform error bounds.
inline double beta_power_difference(double q,double d,unsigned n) {
  double sum=1,term=1;
  for(unsigned k=1;k<n;++k) {term*=q;sum+=term;}
  return d*sum;
}
inline double beta_psi_increment(double z,double h) {
  if(z<20) {
    if(h>=.01*z) return special::digamma(z+h)-special::digamma(z);
    // Exact digamma recurrence transports a small increment to z >= 20.
    double correction=0;
    while(z<20) {correction+=(h/(z+h))/z;z+=1;}
    return correction+beta_psi_increment(z,h);
  }
  const double r=h/z, q=1/(1+r), inv=1/z, d=r/(1+r), v=inv*inv;
  return std::log1p(r)+.5*inv*d
    +v*beta_power_difference(q,d,2)/12
    -v*v*beta_power_difference(q,d,4)/120
    +v*v*v*beta_power_difference(q,d,6)/252
    -v*v*v*v*beta_power_difference(q,d,8)/240;
}
inline double beta_tri_difference(double z,double h) {
  if(z<20) {
    if(h>=.01*z) return special::trigamma(z)-special::trigamma(z+h);
    double correction=0;
    while(z<20) {
      correction+=(h/(z+h))/z*(1/z+1/(z+h));z+=1;
    }
    return correction+beta_tri_difference(z,h);
  }
  const double r=h/z, q=1/(1+r), inv=1/z, d=r/(1+r), v=inv*inv;
  return inv*d+.5*v*beta_power_difference(q,d,2)
    +inv*v*beta_power_difference(q,d,3)/6
    -inv*v*v*beta_power_difference(q,d,5)/30
    +inv*v*v*v*beta_power_difference(q,d,7)/42
    -inv*v*v*v*v*beta_power_difference(q,d,9)/30;
}
struct BetaModel {
  static constexpr std::size_t count=2;
  double shape1, shape2;
  std::array<std::string,2> names() const { return {"shape1","shape2"}; }
  std::string conditions() const {return "shape1, shape2 > 0 finite; 0 < x < 1; independent data";}
  bool valid() const {return shape1>0 && shape2>0 && std::isfinite(shape1) && std::isfinite(shape2);}
  bool support(double x) const {return x>0 && x<1 && std::isfinite(x);}
  std::array<double,4> fisher() const { const double t=special::trigamma(shape1+shape2);
    return {beta_tri_difference(shape1,shape2),-t,-t,beta_tri_difference(shape2,shape1)}; }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    s={std::log(x)+beta_psi_increment(shape1,shape2),std::log1p(-x)+beta_psi_increment(shape2,shape1)};
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
    return null_mean*(l+inference_detail::beta_psi_increment(null_mean*k,(1-null_mean)*k))+
      (1-null_mean)*(r+inference_detail::beta_psi_increment((1-null_mean)*k,null_mean*k));
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
