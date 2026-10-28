// Actuar (shape, scale) maps to Lomax (kappa, lambda) on 1/x.
#pragma once
#include "../lomax/mle.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
struct InvParetoResult { double shape, scale; double logLik; int iterations; LomaxStatus status; };
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
  return {r.kappa, r.lambda, r.logLik+jacobian, r.iterations, r.status};
}
} // namespace cppunivariate
