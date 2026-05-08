// Gumbel distribution (extraDistr parameterisation; max convention).
// Parameters: location mu, scale sigma > 0.
//   z      = (x - mu) / sigma
//   f(x)   = (1/sigma) * exp(-(z + exp(-z)))
//   F(x)   = exp(-exp(-z))
//   F^-1(p)= mu - sigma * log(-log(p))
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dgumbel(double x, double mu, double sigma, bool log) {
  if (!(sigma > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  const double z = (x - mu) / sigma;
  const double log_density = -(z + std::exp(-z)) - std::log(sigma);
  return log ? log_density : std::exp(log_density);
}

inline double pgumbel(double q, double mu, double sigma, bool lower_tail,
                      bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  const double z = (q - mu) / sigma;
  double prob = std::exp(-std::exp(-z));
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qgumbel(double p, double mu, double sigma, bool lower_tail,
                      bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  return mu - sigma * std::log(-std::log(p));
}

template <typename Rng>
inline double rgumbel(Rng &rng, double mu, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  return mu - sigma * std::log(-std::log(u));
}

} // namespace distributions
} // namespace cppunivariate
