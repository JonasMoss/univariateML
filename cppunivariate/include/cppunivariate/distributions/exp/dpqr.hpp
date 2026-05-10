// Exponential distribution (rate parameterisation).
//
// Single parameter: rate > 0. Support: x >= 0.
//   f(x; rate)      = rate * exp(-rate * x)
//   F(x; rate)      = 1 - exp(-rate * x)
//   F^{-1}(p; rate) = -log1p(-p) / rate
//
// Sampler: inverse CDF on U ~ Uniform(0, 1). Matches the same
// inverse-CDF pattern as the other lightweight distributions in
// cppunivariate.

#pragma once

#include "../../special/constants.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dexp(double x, double rate, bool log) {
  if (!(rate > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double log_density = std::log(rate) - rate * x;
  return log ? log_density : std::exp(log_density);
}

inline double pexp(double q, double rate, bool lower_tail, bool log_p) {
  if (!(rate > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = 0.0;
  } else {
    // log(1 - F) = -rate * q; expressed via expm1 for numerical stability.
    prob = -std::expm1(-rate * q);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qexp(double p, double rate, bool lower_tail, bool log_p) {
  if (!(rate > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  // log1p(-p) is more accurate than log(1 - p) when p is small.
  return -std::log1p(-p) / rate;
}

template <typename Rng>
inline double rexp(Rng &rng, double rate) {
  const double u = rng.runif(0.0, 1.0);
  return -std::log1p(-u) / rate;
}

}  // namespace distributions
}  // namespace cppunivariate
