// Nakagami MLE: y = x^2, then Gamma MLE on y. Shape parameter is
// preserved; scale is rederived from the gamma fit as scale = shape * gamma_scale
// (gamma_scale = 1/rate).

#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct NakaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
};

inline NakaResult naka_mle(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, GammaStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && std::isfinite(v))) return {NAN,NAN,NAN,0,GammaStatus::InvalidInput};

  // With both coordinates free, a positive constant sample has an
  // unbounded shape likelihood, independently of transformation range.
  bool constant=true;
  for (double v : x) constant=constant && v==x[0];
  if (constant) return {NAN,NAN,NAN,0,GammaStatus::Degenerate};

  std::vector<double> x2(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    x2[i] = x[i] * x[i];
    if (!(x2[i] > 0 && std::isfinite(x2[i]))) return {NAN,NAN,NAN,0,GammaStatus::TransformFailure};
    sum_log_x += std::log(x[i]);
  }
  const double dn = static_cast<double>(n);

  GammaResult g = gamma_mle(span{x2.data(), n}, tol, maxit);
  const double shape = g.shape;
  const double scale = shape * g.scale;
  // X^2 is gamma: log f_naka(x) = log f_gamma(x^2) + log 2 + log x.
  const double logLik = g.logLik + dn * std::log(2.0) + sum_log_x;
  const GammaStatus status=g.status==GammaStatus::Degenerate ? GammaStatus::TransformFailure : g.status;
  return {shape, scale, logLik, g.iterations, status};
}

inline NakaResult naka_mle(span x,double tol,int maxit,double shape,double scale) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(shape) && std::isnan(scale))return naka_mle(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(scale) && !(scale>0 && std::isfinite(scale))))return {nan,nan,nan,0,GammaStatus::IterationFailure};
  CPPUNIV_EXTENDED_REAL sum=0,logs=0;
  for(double v:x){if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,GammaStatus::IterationFailure};sum+=static_cast<CPPUNIV_EXTENDED_REAL>(v)*v;logs+=std::log(v);}
  const double n=static_cast<double>(x.size());
  if(std::isnan(scale)) {
    scale=static_cast<double>(sum/n);
    if (!(scale>0 && std::isfinite(scale))) return {shape,scale,nan,0,GammaStatus::TransformFailure};
  }
  int iterations=0;
  if(std::isnan(shape)) {
    // s = mean(z - 1 - log z), with phi stable near z=1.
    double s=0;for(double v:x){const double z=v*v/scale;s+=gamma_detail::phi((v*v-scale)/scale,z)/n;}
    if(!(s>0)) {
      bool constant=true;for(double v:x)constant=constant && v==x[0];
      return {nan,scale,nan,0,s==0 && constant ? GammaStatus::Degenerate : GammaStatus::TransformFailure};
    }
    double lo=std::log(0.25/s),hi=std::log(2/s);
    for(int j=0;j<maxit && hi-lo>1e-12;++j){double mid=(lo+hi)/2;if(gamma_detail::log_minus_digamma(std::exp(mid))>s)lo=mid;else hi=mid;++iterations;}
    if(hi-lo>1e-12)return {nan,scale,nan,iterations,GammaStatus::IterationFailure};
    shape=std::exp((lo+hi)/2);
  }
  const double ll=static_cast<double>(n*(std::log(2.0)+shape*(std::log(shape)-std::log(scale))-std::lgamma(shape))+(2*shape-1)*logs-shape*sum/scale);
  return {shape,scale,ll,iterations,shape>0 && scale>0 && std::isfinite(shape) && std::isfinite(scale) && std::isfinite(ll) ? GammaStatus::Ok : GammaStatus::TransformFailure};
}

}  // namespace cppunivariate
