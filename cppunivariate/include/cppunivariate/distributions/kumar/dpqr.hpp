// Kumaraswamy distribution (extraDistr parameterisation).
// Parameters: a > 0, b > 0; support x in [0, 1].
//   f(x)   = a*b * x^(a-1) * (1 - x^a)^(b-1)
//   F(x)   = 1 - (1 - x^a)^b
//   F^-1(p)= (1 - (1-p)^(1/b))^(1/a)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dkumar(double x, double a, double b, bool log) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0 || x > 1.0) {
    return log ? kNegInf : 0.0;
  }
  if (log) {
    return std::log(a) + std::log(b) + std::log(x) * (a - 1.0) +
           std::log1p(-std::pow(x, a)) * (b - 1.0);
  }
  return a * b * std::pow(x, a - 1.0) * std::pow(1.0 - std::pow(x, a), b - 1.0);
}

inline double pkumar(double q, double a, double b, bool lower_tail,
                     bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < 0.0) {
    prob = 0.0;
  } else if (q >= 1.0) {
    prob = 1.0;
  } else {
    prob = 1.0 - std::pow(1.0 - std::pow(q, a), b);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qkumar(double p, double a, double b, bool lower_tail,
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
  return std::pow(1.0 - std::pow(1.0 - p, 1.0 / b), 1.0 / a);
}

template <typename Rng>
inline double rkumar(Rng &rng, double a, double b) {
  const double u = rng.runif(0.0, 1.0);
  return std::pow(1.0 - std::pow(1.0 - u, 1.0 / b), 1.0 / a);
}

} // namespace distributions
} // namespace cppunivariate
