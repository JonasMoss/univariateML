// Lomax distribution (Pareto Type II, extraDistr parameterisation).
// Parameters: rate lambda > 0, shape kappa > 0; support x > 0.
//   f(x)   = lambda*kappa / (1 + lambda*x)^(kappa+1)
//   F(x)   = 1 - (1 + lambda*x)^(-kappa)
//   F^-1(p)= ((1-p)^(-1/kappa) - 1) / lambda
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dlomax(double x, double lambda, double kappa, bool log) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double log_density = std::log(lambda) + std::log(kappa) -
                             std::log1p(lambda * x) * (kappa + 1.0);
  return log ? log_density : std::exp(log_density);
}

inline double plomax(double q, double lambda, double kappa, bool lower_tail,
                     bool log_p) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = 0.0;
  } else {
    prob = 1.0 - std::exp(std::log1p(lambda * q) * (-kappa));
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qlomax(double p, double lambda, double kappa, bool lower_tail,
                     bool log_p) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  return (std::pow(1.0 - p, -1.0 / kappa) - 1.0) / lambda;
}

template <typename Rng>
inline double rlomax(Rng &rng, double lambda, double kappa) {
  const double u = rng.runif(0.0, 1.0);
  return (std::pow(1.0 - u, -1.0 / kappa) - 1.0) / lambda;
}

} // namespace distributions
} // namespace cppunivariate
