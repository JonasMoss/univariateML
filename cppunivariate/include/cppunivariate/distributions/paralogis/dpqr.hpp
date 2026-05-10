// Paralogistic distribution (actuar). Burr family with shape1 == shape2 == shape.
// Parameters: shape > 0, scale > 0; support x > 0.
//   v       = (x/scale)^shape
//   u       = 1/(1+v)
//   F(x)    = 1 - u^shape
//   f(x)    = shape^2 * u^shape * (1-u) / x
//   F^-1(p) = scale * ((1-p)^(-1/shape) - 1)^(1/shape)
// Sample: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dparalogis(double x, double shape, double scale, bool log) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(x) ||
      !std::isfinite(shape)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  if (x == 0.0) {
    if (shape < 1.0) return std::numeric_limits<double>::infinity();
    if (shape > 1.0) return log ? kNegInf : 0.0;
    return log ? -std::log(scale) : 1.0 / scale;
  }
  const double logv = shape * (std::log(x) - std::log(scale));
  const double logu = -std::log1p(std::exp(logv));
  const double log1mu = -std::log1p(std::exp(-logv));
  const double log_density =
      2.0 * std::log(shape) + shape * logu + log1mu - std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double pparalogis(double q, double shape, double scale,
                         bool lower_tail, bool log_p) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const double zlog = shape * (std::log(q) - std::log(scale));
    const double u = std::exp(-std::log1p(std::exp(zlog)));
    if (lower_tail) {
      prob = 1.0 - std::pow(u, shape);
    } else {
      prob = std::pow(u, shape);
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qparalogis(double p, double shape, double scale,
                         bool lower_tail, bool log_p) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p <= 0.0) return 0.0;
  if (p >= 1.0) return std::numeric_limits<double>::infinity();
  return scale *
         std::pow(std::pow(1.0 - p, -1.0 / shape) - 1.0, 1.0 / shape);
}

template <typename Rng>
inline double rparalogis(Rng &rng, double shape, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale *
         std::pow(std::pow(1.0 - u, -1.0 / shape) - 1.0, 1.0 / shape);
}

} // namespace distributions
} // namespace cppunivariate
