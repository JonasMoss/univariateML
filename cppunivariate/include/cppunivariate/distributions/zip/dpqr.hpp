// Zero-inflated Poisson (ZIP) distribution (extraDistr).
// Parameters: lambda > 0, 0 <= pi <= 1; support x in {0, 1, 2, ...}.
//   p(0)   = pi + (1-pi) * exp(-lambda)
//   p(k)   = (1-pi) * dpois(k; lambda)         for k >= 1
//   F(x)   = pi + (1-pi) * ppois(x; lambda)
//   F^-1(p)= 0                                  if p < pi
//          = qpois((p - pi)/(1-pi); lambda)     otherwise
// Sample: U ~ Uniform(0, 1); return 0 if U<pi else rpois(lambda).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dzip(double x, double lambda, double pi, bool log) {
  if (!(lambda > 0.0) || !(pi >= 0.0 && pi <= 1.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0 || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  double pmf;
  if (x == 0.0) {
    pmf = pi + std::exp(std::log1p(-pi) - lambda);
  } else {
    pmf = std::exp(std::log1p(-pi) +
                   cppunivariate::special::dpois(x, lambda, /*log=*/true));
  }
  return log ? std::log(pmf) : pmf;
}

inline double pzip(double q, double lambda, double pi, bool lower_tail,
                   bool log_p) {
  if (!(lambda > 0.0) || !(pi >= 0.0 && pi <= 1.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < 0.0) {
    prob = 0.0;
  } else {
    const double pp = cppunivariate::special::poisson_cdf(std::floor(q), lambda, true);
    prob = pi + std::exp(std::log1p(-pi) + std::log(pp));
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qzip(double p, double lambda, double pi, bool lower_tail,
                   bool log_p) {
  if (!(lambda > 0.0) || !(pi >= 0.0 && pi <= 1.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p < pi) {
    return 0.0;
  }
  if (p >= 1.0) {
    return std::numeric_limits<double>::infinity();
  }
  const double scaled = (p - pi) / (1.0 - pi);
  return cppunivariate::special::poisson_quantile(scaled, lambda, true);
}

template <typename Rng>
inline double rzip(Rng &rng, double lambda, double pi) {
  // Bit-for-bit port of extraDistr's rng_zip: one runif, then conditionally
  // one rpois.
  const double u = rng.runif(0.0, 1.0);
  if (u < pi) {
    return 0.0;
  }
  return rng.rpois(lambda);
}

} // namespace distributions
} // namespace cppunivariate
