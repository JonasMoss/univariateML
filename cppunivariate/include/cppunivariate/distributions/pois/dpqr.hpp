// Poisson distribution: scalar d/p/q/r.
// Parameter: lambda > 0; support x in {0, 1, 2, ...}.
//   p(k)    = lambda^k * exp(-lambda) / k!
//
// dpois closed-form (already in cppunivariate::special::dpois).
// p/q via Boost.Math poisson_distribution with a discrete-quantile policy
// matching R's "smallest k with P(X<=k) >= p" convention.

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dpois(double x, double lambda, bool log) {
  return cppunivariate::special::dpois(x, lambda, log);
}

inline double ppois(double q, double lambda, bool lower_tail, bool log_p) {
  if (!(lambda > 0.0)) return kNaN;
  if (!std::isfinite(q)) return kNaN;
  double prob;
  if (q < 0.0) {
    prob = 0.0;
  } else {
    prob = cppunivariate::special::poisson_cdf(std::floor(q), lambda, lower_tail);
  }
  if (q < 0.0 && !lower_tail) prob = 1.0;
  return log_p ? std::log(prob) : prob;
}

inline double qpois(double p, double lambda, bool lower_tail, bool log_p) {
  if (!(lambda > 0.0)) return kNaN;
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  return cppunivariate::special::poisson_quantile(p, lambda, true);
}

template <typename Rng>
inline double rpois(Rng &rng, double lambda) {
  return rng.rpois(lambda);
}

}  // namespace distributions
}  // namespace cppunivariate
