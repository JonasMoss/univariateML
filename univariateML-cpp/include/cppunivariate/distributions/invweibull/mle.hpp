// Inverse Weibull MLE via Gumbel transform on log(x). Parameters recovered
// as shape = 1/sigma, scale = 1/exp(-mu + s) where s = -mean(log x).

#pragma once

#include "../../core/span.hpp"
#include "../gumbel/mle.hpp"
#include "../weibull/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class InvWeibullStatus { Ok, InvalidInput, Unbounded, IterationFailure };

struct InvWeibullResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  InvWeibullStatus status = InvWeibullStatus::Ok;
};

// shape0 is an optional positive starting shape; NaN keeps the automatic start.
inline InvWeibullResult invweibull_mle(span x, double tol, int maxit,
                                  double shape0 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> log_x(n);
  double sum_log_x = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    log_x[i] = std::log(x[i]);
    sum_log_x += log_x[i];
  }
  const double dn = static_cast<double>(n);
  const double s = -sum_log_x / dn;
  for (auto &v : log_x) v += s;

  GumbelResult g = fit_gumbel_newton(span{log_x.data(), n}, tol, maxit,
      std::isnan(shape0) ? shape0 : -std::log(shape0));
  const double rate_log = -g.mu + s;
  const double rate = std::exp(rate_log);
  const double shape = 1.0 / g.sigma;
  // Transform the fitted Gumbel likelihood, avoiding shape times
  // a difference of rounded log scales near a point mass.
  const double logLik = g.logLik + dn * s;
  return {shape, 1.0 / rate, logLik, g.iterations,
      g.status == GumbelStatus::Ok ? InvWeibullStatus::Ok :
      g.status == GumbelStatus::Unbounded ? InvWeibullStatus::Unbounded :
      InvWeibullStatus::IterationFailure};
}

// `scale` is actuar's inverse-Weibull scale (1/rate); the weibull delegate
// on 1/x uses its own `scale` field directly equal to the old actuar rate,
// so a held scale is converted once and a held weibull scale is converted
// back once, never round-tripped.
inline InvWeibullResult invweibull_mle(span x,double tol,int maxit,double shape0,double shape,double scale) {
  if(std::isnan(shape) && std::isnan(scale))return invweibull_mle(x,tol,maxit,shape0);
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const bool scale_held = !std::isnan(scale);
  const double rate = scale_held ? 1.0/scale : scale;
  const auto r=weibull_mle(span{y.data(),y.size()},tol,maxit,shape0,shape,rate);
  const double scale_out = scale_held ? scale : (std::isnan(r.scale) ? r.scale : 1.0/r.scale);
  return {r.shape,scale_out,r.logLik+jac,r.iterations,
    r.status==WeibullStatus::Ok ? InvWeibullStatus::Ok :
    r.status==WeibullStatus::Unbounded ? InvWeibullStatus::Unbounded :
    r.status==WeibullStatus::IterationFailure ? InvWeibullStatus::IterationFailure : InvWeibullStatus::InvalidInput};
}

}  // namespace cppunivariate
