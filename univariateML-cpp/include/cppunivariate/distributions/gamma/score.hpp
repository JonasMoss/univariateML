// Analytic README-coordinate derivatives; see core/inference.hpp for the contract.
#pragma once
#include "../../core/inference.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace inference_detail {
struct GammaModel {
  static constexpr std::size_t count=2;
  double shape, scale;
  std::array<std::string,2> names() const { return {"shape","scale"}; }
  std::string conditions() const {return "shape, scale > 0 finite; x > 0 finite; independent data";}
  bool valid() const {return shape>0 && scale>0 && std::isfinite(shape) && std::isfinite(scale);}
  bool support(double x) const {return x>0 && std::isfinite(x);}
  std::array<double,4> fisher() const {
    const double v=shape/scale/scale;
    return {special::trigamma(shape),1/scale,1/scale,v==0 ? NAN : v};
  }
  void observation(double x, std::array<double,2>& s, std::array<double,4>& h) const {
    const double inv=1/scale;
    const double ratio=x/scale;
    const double log_ratio=ratio>0 && std::isfinite(ratio)
      ? std::log(ratio) : std::log(x)-std::log(scale);
    s={log_ratio-special::digamma(shape),(x/scale-shape)*inv};
    h={special::trigamma(shape),inv,inv,(2*x/scale-shape)*inv*inv};
  }
};
} // namespace inference_detail
// Callback receives each free-coordinate score; default discards rows.
template<class Emit = inference_detail::Discard>
inline InferenceResult gamma_inference(span x, double shape, double scale,
    unsigned free_mask=3, Emit emit={}) {
  return inference_detail::accumulate(x,inference_detail::GammaModel{shape, scale},free_mask,emit);
}
} // namespace cppunivariate

#include "mle.hpp"
namespace cppunivariate {
enum class GammaTarget { Mean, Shape };
inline TargetInferenceResult gamma_target_inference(span x,GammaTarget target,double null_value,
    CovarianceKind kind=CovarianceKind::Fisher) {
  TargetInferenceResult failure;failure.status=InferenceStatus::NumericalFailure;
  if(!(null_value>0 && std::isfinite(null_value))) {failure.status=InferenceStatus::InvalidParameter;return failure;}
  const auto f=gamma_mle(x,1e-11,2000);
  if(f.status!=GammaStatus::Ok) return failure;
  GammaResult c;
  if(target==GammaTarget::Shape) c=gamma_mle(x,1e-11,2000,null_value,NAN);
  else {
    // In (mean,shape), mean fixed: per-observation shape derivative
    // log(a)-psi(a)+1+mean(log x)-log(m)-xbar/m; derivative 1/a-psi1(a)<0.
    const double q=1+mean_log(x)-std::log(null_value)-mean(x)/null_value;
    const auto root=inference_detail::positive_concave_root([q](double a) {
      return gamma_detail::log_minus_digamma(a)+q;
    });
    if(root.status!=InferenceStatus::Ok) return failure;
    c=gamma_mle(x,1e-11,2000,root.statistic,null_value/root.statistic);
  }
  if(c.status!=GammaStatus::Ok) return failure;
  std::array<double,inference_max_parameters> g{};
  if(target==GammaTarget::Mean) {g[0]=f.scale;g[1]=f.shape;}else g[0]=1;
  return inference_detail::target_result(target==GammaTarget::Mean?f.shape*f.scale:f.shape,
    null_value,g,gamma_inference(x,f.shape,f.scale),gamma_inference(x,c.shape,c.scale),f.logLik,c.logLik,kind);
}
inline InferenceInterval gamma_target_lr_interval(span x,GammaTarget target,double critical) {
  const auto f=gamma_mle(x,1e-11,2000);
  if(f.status!=GammaStatus::Ok) return {InferenceStatus::NumericalFailure,NAN,NAN};
  return inference_detail::profile_lr_interval(std::log(target==GammaTarget::Mean?f.shape*f.scale:f.shape),
    [&](double t){return gamma_target_inference(x,target,t).likelihood_ratio;},
    [](double t){return std::exp(t);},critical);
}
} // namespace cppunivariate
