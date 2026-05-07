// Power distribution (extraDistr parameterisation).
// Parameters: alpha > 0 (upper bound), beta > 0 (shape); support 0 < x < alpha.
//   f(x)   = beta * x^(beta-1) / alpha^beta
//   F(x)   = (x / alpha)^beta
//   F^-1(p)= alpha * p^(1/beta)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dpower(double x, double alpha, double beta, bool log) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0 || x >= alpha) {
    return log ? kNegInf : 0.0;
  }
  const double log_density =
      std::log(beta) + std::log(x) * (beta - 1.0) - std::log(alpha) * beta;
  return log ? log_density : std::exp(log_density);
}

inline double ppower(double q, double alpha, double beta, bool lower_tail,
                     bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = 0.0;
  } else if (q >= alpha) {
    prob = 1.0;
  } else {
    prob = std::exp(std::log(q) * beta - std::log(alpha) * beta);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qpower(double p, double alpha, double beta, bool lower_tail,
                     bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  return alpha * std::pow(p, 1.0 / beta);
}

template <typename Rng>
inline double rpower(Rng &rng, double alpha, double beta) {
  const double u = rng.runif(0.0, 1.0);
  return alpha * std::pow(u, 1.0 / beta);
}

} // namespace distributions
} // namespace mlcore
