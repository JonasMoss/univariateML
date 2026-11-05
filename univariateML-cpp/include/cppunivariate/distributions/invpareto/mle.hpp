// Actuar (shape, scale) maps to Lomax (kappa, lambda) on 1/x.
#pragma once
#include "../lomax/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
// Exp on the reciprocal is inverse exponential with scale=rate.
inline StopDetail invpareto_stop_detail(StopDetail d,double jac) {
 for(auto& c:d.candidates) {
  if(c.model==StopModel::None && c.coef.size()==2)c.coef={c.coef[1],c.coef[0]};
  else if(c.model==StopModel::Exp && c.coef.size()==1){c.model=StopModel::InvExp;c.coef={c.coef[0]};}
  c.loglik_lower+=jac;c.loglik_upper+=jac;
 }
 d.limit_loglik+=jac;d.loglik_lower+=jac;d.loglik_upper+=jac;d.segment_loglik_upper+=jac;
 return d;
}

struct InvParetoResult { double shape, scale; double logLik; int iterations; LomaxStatus status; StopDetail stop_detail{}; LomaxCertificateFailure certificate_failure = LomaxCertificateFailure::None; };
inline InvParetoResult invpareto_mle(span x, double tol, int maxit, double scale0 = std::numeric_limits<double>::quiet_NaN()) {
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v) || !std::isfinite(1/v)) {
      const double nan=std::numeric_limits<double>::quiet_NaN();
      return {nan,nan,nan,0,LomaxStatus::InvalidStart};
    }
  }
  std::vector<double> y(x.size());
  double jacobian = 0;
  for (std::size_t i=0; i<x.size(); ++i) { y[i]=1.0/x[i]; jacobian -= 2*std::log(x[i]); }
  auto r = lomax_mle(span{y.data(), y.size()}, tol, maxit, scale0);
  return {r.kappa, r.lambda, r.logLik+jacobian, r.iterations, r.status,invpareto_stop_detail(r.stop_detail,jacobian),r.certificate_failure};
}

inline InvParetoResult invpareto_mle(span x,double tol,int maxit,double scale0,double shape,double scale) {
  if(std::isnan(shape) && std::isnan(scale))return invpareto_mle(x,tol,maxit,scale0);
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto r=lomax_mle(span{y.data(),y.size()},tol,maxit,scale0,scale,shape);
  return {r.kappa,r.lambda,r.logLik+jac,r.iterations,r.status,invpareto_stop_detail(r.stop_detail,jac),r.certificate_failure};
}

} // namespace cppunivariate
