// Pareto distribution (Type I, extraDistr parameterisation).
// Parameters: shape a > 0, scale (lower bound) b > 0; support x >= b.
//   f(x)   = a * b^a / x^(a+1)
//   F(x)   = 1 - (b/x)^a
//   F^-1(p)= b / (1-p)^(1/a)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dpareto(double x, double a, double b, bool log) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < b) {
    return log ? kNegInf : 0.0;
  }
  const double log_density =
      std::log(a) + std::log(b) * a - std::log(x) * (a + 1.0);
  return log ? log_density : std::exp(log_density);
}

inline double ppareto(double q, double a, double b, bool lower_tail,
                      bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < b) {
    prob = 0.0;
  } else {
    prob = 1.0 - std::pow(b / q, a);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qpareto(double p, double a, double b, bool lower_tail,
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
  return b / std::pow(1.0 - p, 1.0 / a);
}

template <typename Rng>
inline double rpareto(Rng &rng, double a, double b) {
  const double u = rng.runif(0.0, 1.0);
  return b / std::pow(1.0 - u, 1.0 / a);
}

} // namespace distributions
} // namespace mlcore
