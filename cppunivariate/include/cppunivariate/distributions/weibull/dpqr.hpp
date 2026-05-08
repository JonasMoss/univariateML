// Weibull distribution: scalar d/p/q/r.
// Parameters: shape > 0, scale > 0; support x >= 0.
//   f(x)    = (shape/scale) * (x/scale)^(shape-1) * exp(-(x/scale)^shape)
//   F(x)    = 1 - exp(-(x/scale)^shape)
//   F^-1(p) = scale * (-log1p(-p))^(1/shape)

#pragma once

#include "../../special/constants.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dweibull(double x, double shape, double scale, bool log) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  if (x == 0.0) {
    if (shape < 1.0) return std::numeric_limits<double>::infinity();
    if (shape > 1.0) return log ? kNegInf : 0.0;
    // shape == 1: density at 0 is 1/scale (matches stats::dweibull).
    const double density = 1.0 / scale;
    return log ? std::log(density) : density;
  }
  const double xs = x / scale;
  const double log_density = std::log(shape) - std::log(scale) +
                             (shape - 1.0) * std::log(xs) -
                             std::pow(xs, shape);
  return log ? log_density : std::exp(log_density);
}

inline double pweibull(double q, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape > 0.0 && scale > 0.0)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = 0.0;
  } else {
    // -expm1(-(q/scale)^shape) is more accurate near 0 than 1 - exp(...).
    prob = -std::expm1(-std::pow(q / scale, shape));
  }
  if (!lower_tail) prob = 1.0 - prob;
  return log_p ? std::log(prob) : prob;
}

inline double qweibull(double p, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape > 0.0 && scale > 0.0)) {
    return kNaN;
  }
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  return scale * std::pow(-std::log1p(-p), 1.0 / shape);
}

template <typename Rng>
inline double rweibull(Rng &rng, double shape, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale * std::pow(-std::log1p(-u), 1.0 / shape);
}

}  // namespace distributions
}  // namespace cppunivariate
