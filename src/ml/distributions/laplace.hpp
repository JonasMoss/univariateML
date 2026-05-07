// Laplace (double-exponential) distribution: scalar d/p/q/r.
// Parameters: location mu, scale sigma > 0.
//   f(x)   = (1/(2*sigma)) * exp(-|x-mu|/sigma)
//   F(x)   = (x < mu) ? 0.5*exp((x-mu)/sigma) : 1 - 0.5*exp(-(x-mu)/sigma)
//   F^-1(p)= mu + sigma * log(2p)         if p <= 0.5
//          = mu - sigma * log(2(1-p))     otherwise
// Sampler: inverse-CDF on U ~ Uniform(0, 1).

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dlaplace(double x, double mu, double sigma, bool log) {
  if (!(sigma > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  const double z = std::fabs(x - mu) / sigma;
  const double log_density = -z - std::log(2.0) - std::log(sigma);
  return log ? log_density : std::exp(log_density);
}

inline double plaplace(double q, double mu, double sigma, bool lower_tail,
                       bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  const double z = (q - mu) / sigma;
  double prob;
  if (q < mu) {
    prob = 0.5 * std::exp(z);
  } else {
    prob = 1.0 - 0.5 * std::exp(-z);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qlaplace(double p, double mu, double sigma, bool lower_tail,
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
  if (p < 0.5) {
    return mu + sigma * std::log(2.0 * p);
  }
  return mu - sigma * std::log(2.0 * (1.0 - p));
}

template <typename Rng>
inline double rlaplace(Rng &rng, double mu, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  if (u < 0.5) {
    return mu + sigma * std::log(2.0 * u);
  }
  return mu - sigma * std::log(2.0 * (1.0 - u));
}

} // namespace distributions
} // namespace mlcore
