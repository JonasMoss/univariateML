// Rayleigh distribution: scalar d/p/q/r.
// Parameter: scale sigma > 0; support x >= 0.
//   f(x)   = (x/sigma^2) * exp(-x^2/(2*sigma^2))
//   F(x)   = 1 - exp(-x^2/(2*sigma^2))
//   F^-1(p)= sigma * sqrt(-2 * log(1-p))
// (Equivalent to Weibull(shape=2, scale=sigma*sqrt(2)).)

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double drayleigh(double x, double sigma, bool log) {
  if (!(sigma > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double s2 = sigma * sigma;
  const double log_density = std::log(x) - std::log(s2) - 0.5 * x * x / s2;
  return log ? log_density : std::exp(log_density);
}

inline double prayleigh(double q, double sigma, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < 0.0) {
    prob = 0.0;
  } else {
    prob = 1.0 - std::exp(-(q * q) / (2.0 * sigma * sigma));
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qrayleigh(double p, double sigma, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  return sigma * std::sqrt(-2.0 * std::log(1.0 - p));
}

template <typename Rng>
inline double rrayleigh(Rng &rng, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  return sigma * std::sqrt(-2.0 * std::log(1.0 - u));
}

} // namespace distributions
} // namespace mlcore
