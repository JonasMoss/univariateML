// Weibull MLE via Gumbel transform on -log(x). Parameters recovered as
// shape = 1/sigma, scale = exp(-mu + s) where s = -mean(log_x).

#pragma once
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../gumbel/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class WeibullStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct WeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  WeibullStatus status = WeibullStatus::Ok;
};

// shape0 is an optional positive starting shape; NaN keeps the automatic start.
inline WeibullResult weibull_mle(span x, double tol, int maxit,
                                  double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, WeibullStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && std::isfinite(v)))
    return {NAN,NAN,NAN,0,WeibullStatus::InvalidInput};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = -std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit,
      std::isnan(shape0) ? shape0 : -std::log(shape0));
  const double shape = 1.0 / g.sigma;
  const double scale = std::exp(-g.mu + s);
  // Transform the fitted Gumbel likelihood, avoiding shape times
  // a difference of rounded log scales near a point mass.
  const double logLik = g.logLik + sum_log_x;
  return {shape, scale, logLik, g.iterations,
      g.status == GumbelStatus::Ok ? WeibullStatus::Ok :
      g.status == GumbelStatus::Unbounded ? WeibullStatus::Unbounded :
      g.status == GumbelStatus::InvalidInput ? WeibullStatus::InvalidInput :
      WeibullStatus::IterationFailure};
}

// Strictly decreasing fixed-scale shape score. Powers and the profiled
// scale use log coordinates to avoid overflow on ordinary wide samples.
inline WeibullResult weibull_mle(span x,double tol,int maxit,double shape0,double shape,double scale) {
  if (std::isnan(shape) && std::isnan(scale)) return weibull_mle(x,tol,maxit,shape0);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.size()==0 || !(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(shape) && !(shape>0 && std::isfinite(shape))) ||
     (!std::isnan(scale) && !(scale>0 && std::isfinite(scale)))) return {nan,nan,nan,0,WeibullStatus::InvalidInput};
  std::vector<double> logs;
  for(double v:x) {if(!(v>0 && std::isfinite(v)))return {nan,nan,nan,0,WeibullStatus::InvalidInput};logs.push_back(std::log(v));}
  const double n=static_cast<double>(x.size());
  int iterations=0;
  if(std::isnan(scale)) {
    double top=logs[0]; for(double v:logs) top=std::max(top,v);
    CPPUNIV_EXTENDED_REAL shifted=0;
    for(double v:logs)shifted+=std::expm1(static_cast<CPPUNIV_EXTENDED_REAL>(shape)*(v-top));
    scale=std::exp(top+static_cast<double>(std::log1p(shifted/n)/shape));
  }
  const double ls=std::log(scale);
  if(std::isnan(shape)) {
    bool equal=true;for(double v:x)equal=equal && v==scale;
    if(equal)return {nan,scale,nan,0,WeibullStatus::Unbounded};
    auto score=[&](double k) {CPPUNIV_EXTENDED_REAL s=n/k;for(double v:logs){const double z=v-ls;s+=z-z*std::exp(k*z);}return s;};
    double lo=-745,hi=710;
    for(int j=0;j<maxit && hi-lo>1e-12;++j){const double mid=(lo+hi)/2;if(score(std::exp(mid))>0)lo=mid;else hi=mid;++iterations;}
    if(hi-lo>1e-12)return {nan,scale,nan,iterations,WeibullStatus::IterationFailure};
    shape=std::exp((lo+hi)/2);
  }
  CPPUNIV_EXTENDED_REAL ll=0;
  for(double v:logs){const double z=v-ls;ll+=std::log(shape)-v+shape*z-std::exp(shape*z);}
  return {shape,scale,static_cast<double>(ll),iterations};
}

}  // namespace cppunivariate
