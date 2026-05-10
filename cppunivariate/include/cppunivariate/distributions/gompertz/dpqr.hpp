// Gompertz distribution (extraDistr).
// Parameters: a > 0, b > 0; support x >= 0.
//   f(x)   = a * exp(b*x - (a/b) * (exp(b*x) - 1))
//   F(x)   = 1 - exp(-(a/b) * (exp(b*x) - 1))
//   F^-1(p)= log(1 - (b/a) * log(1-p)) / b
// Sample: inverse-CDF on U ~ Uniform(0, 1). Note: extraDistr's sampler uses
// log(1 - b/a*log(u)) / b directly (without the 1-u substitution).

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dgompertz(double x, double a, double b, bool log) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double log_density =
      std::log(a) + b * x - (a / b) * (std::exp(b * x) - 1.0);
  return log ? log_density : std::exp(log_density);
}

inline double pgompertz(double q, double a, double b, bool lower_tail,
                        bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    prob = 1.0 - std::exp(-(a / b) * (std::exp(b * q) - 1.0));
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qgompertz(double p, double a, double b, bool lower_tail,
                        bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  return std::log(1.0 - (b / a) * std::log(1.0 - p)) / b;
}

template <typename Rng>
inline double rgompertz(Rng &rng, double a, double b) {
  // extraDistr's rng_gompertz: log(1 - b/a * log(u)) / b on U ~ Uniform(0, 1).
  const double u = rng.runif(0.0, 1.0);
  return std::log(1.0 - (b / a) * std::log(u)) / b;
}

} // namespace distributions
} // namespace cppunivariate
