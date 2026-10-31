// Inverse Gamma MLE: y = 1/x, then Gamma MLE on y. Result fields
// (shape, scale) follow the standard shape-scale parameterisation:
//   f_X(x; shape, scale) = scale^shape / Gamma(shape) * x^(-shape-1)
//                        * exp(-scale / x).
// Implementation reuses GammaResult{shape, scale} on Y = 1/X; this
// InvGammaResult.scale is numerically equal to the gamma fit's rate,
// i.e. the reciprocal of GammaResult.scale, via the inverse transform.

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
  if (n == 0) return {0.0, 0.0, kNaN, 0, GammaStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && std::isfinite(v)))
    return {NAN,NAN,NAN,0,GammaStatus::InvalidInput};

  // With both coordinates free, a positive constant sample has an
  // unbounded shape likelihood, independently of transformation range.
  bool constant=true;
  for (double v : x) constant=constant && v==x[0];
  if (constant) return {NAN,NAN,NAN,0,GammaStatus::Degenerate};

  std::vector<double> invx(n);
  double sum_log = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    invx[i] = 1.0 / x[i];
    if (!std::isfinite(invx[i])) return {NAN,NAN,NAN,0,GammaStatus::TransformFailure};
    sum_log += std::log(x[i]);
  }
  GammaResult g = gamma_mle(span{invx.data(), n}, tol, maxit);
  // log f_invgamma(x) = log f_gamma(1/x) - 2 log x, so the gamma fit's
  // log-likelihood (stable for huge shapes) carries over.
  const double logLik = g.logLik - 2.0 * sum_log;
  return {g.shape, 1.0 / g.scale, logLik, g.iterations,
    g.status==GammaStatus::Degenerate ? GammaStatus::TransformFailure : g.status};
}

inline InvGammaResult invgamma_mle(span x,double tol,int maxit,double alpha,double beta) {
  if(std::isnan(alpha) && std::isnan(beta))return invgamma_mle(x,tol,maxit);
  std::vector<double> y;double jac=0;
  for(double v:x){
    if (!(v>0 && std::isfinite(v))) return {NAN,NAN,NAN,0,GammaStatus::InvalidInput};
    if (!std::isfinite(1/v)) return {NAN,NAN,NAN,0,GammaStatus::TransformFailure};
    y.push_back(1/v);jac-=2*std::log(v);
  }
  // invgamma's beta is the gamma fit's rate; gamma_mle's 5th argument is
  // the scale (1/rate). A held beta is passed through exactly, never
  // round-tripped through a double reciprocal.
  const bool beta_held = !std::isnan(beta);
  const auto g=gamma_mle(span{y.data(),y.size()},tol,maxit,alpha,beta_held ? 1.0/beta : beta);
  const double scale_out = beta_held ? beta : (std::isnan(g.scale) ? g.scale : 1.0/g.scale);
  return {g.shape,scale_out,g.logLik+jac,g.iterations,g.status};
}

}  // namespace cppunivariate
