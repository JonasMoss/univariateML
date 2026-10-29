// Inverse Weibull MLE via Gumbel transform on log(x). Parameters recovered
// as shape = 1/sigma, rate = exp(-mu + s) where s = -mean(log x).

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
  double rate;
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
  return {shape, rate, logLik, g.iterations};
}

inline InvWeibullResult invweibull_mle(span x,double tol,int maxit,double shape0,double shape,double rate) {
  if(std::isnan(shape) && std::isnan(rate))return invweibull_mle(x,tol,maxit,shape0);
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto r=weibull_mle(span{y.data(),y.size()},tol,maxit,shape0,shape,rate);
  return {r.shape,r.scale,r.logLik+jac,r.iterations,
    r.status==WeibullStatus::Ok ? InvWeibullStatus::Ok :
    r.status==WeibullStatus::Unbounded ? InvWeibullStatus::Unbounded :
    r.status==WeibullStatus::IterationFailure ? InvWeibullStatus::IterationFailure : InvWeibullStatus::InvalidInput};
}

}  // namespace cppunivariate
