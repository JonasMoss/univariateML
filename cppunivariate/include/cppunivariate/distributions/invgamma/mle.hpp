// Inverse Gamma MLE: y = 1/x, then Gamma MLE on y. Result fields
// (shape, scale) follow the standard shape-scale parameterisation:
//   f_X(x; shape, scale) = scale^shape / Gamma(shape) * x^(-shape-1)
//                        * exp(-scale / x).
// Implementation reuses GammaResult{shape, rate} on Y = 1/X; scale is
// numerically equal to GammaResult.rate via the inverse transform.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct InvGammaResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  GammaStatus status = GammaStatus::Ok;
};

inline InvGammaResult invgamma_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> invx(n);
  double sum_log = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    sum_log += std::log(x[i]);
  }
  GammaResult g = gamma_mle(span{invx.data(), n}, tol, maxit);
  // log f_invgamma(x) = log f_gamma(1/x) - 2 log x, so the gamma fit's
  // log-likelihood (stable for huge shapes) carries over.
  const double logLik = g.logLik - 2.0 * sum_log;
  return {g.shape, g.rate, logLik, g.iterations, g.status};
}

inline InvGammaResult invgamma_mle(span x,double tol,int maxit,double alpha,double beta) {
  if(std::isnan(alpha) && std::isnan(beta))return invgamma_mle(x,tol,maxit);
  std::vector<double> y;double jac=0;
  for(double v:x){y.push_back(1/v);jac-=2*std::log(v);}
  const auto g=gamma_mle(span{y.data(),y.size()},tol,maxit,alpha,beta);
  return {g.shape,g.rate,g.logLik+jac,g.iterations,g.status};
}

}  // namespace cppunivariate
