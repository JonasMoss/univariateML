// Inverse Burr / Dagum distribution (actuar parameterisation: shape1, shape2,
// scale). mlinvburr uses (shape1, shape2) with scale=1.
//   v       = (x/scale)^shape2
//   u       = v/(1+v) = 1/(1 + 1/v)
//   F(x)    = u^shape1
//   f(x)    = shape1*shape2 * u^shape1 * (1-u) / x
//   F^-1(p) = scale * (p^(-1/shape1) - 1)^(-1/shape2)
//          = scale * (p^(1/shape1) / (1 - p^(1/shape1)))^(1/shape2)
// Sample: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dinvburr(double x, double shape1, double shape2, double scale,
                       bool log) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0) || !std::isfinite(x) ||
      !std::isfinite(shape1) || !std::isfinite(shape2)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  if (x == 0.0) {
    if (shape1 * shape2 < 1.0) return std::numeric_limits<double>::infinity();
    if (shape1 * shape2 > 1.0) return log ? kNegInf : 0.0;
    return log ? -std::log(scale) : 1.0 / scale;
  }
  const double logv = shape2 * (std::log(x) - std::log(scale));
  // logu = log(v/(1+v)) = -log1pexp(-logv)
  const double logu = -std::log1p(std::exp(-logv));
  // log(1-u) = -log1pexp(logv)
  const double log1mu = -std::log1p(std::exp(logv));
  const double log_density = std::log(shape1) + std::log(shape2) +
                             shape1 * logu + log1mu - std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double pinvburr(double q, double shape1, double shape2, double scale,
                       bool lower_tail, bool log_p) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const double zlog = shape2 * (std::log(scale) - std::log(q));
    const double u = std::exp(-std::log1p(std::exp(zlog)));
    if (lower_tail) {
      prob = std::pow(u, shape1);
    } else {
      prob = 1.0 - std::pow(u, shape1);
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qinvburr(double p, double shape1, double shape2, double scale,
                       bool lower_tail, bool log_p) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0) || !std::isfinite(p)) {
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
  // F^-1(p) = scale * (p^(-1/shape1) - 1)^(-1/shape2)
  return scale * std::pow(std::pow(p, -1.0 / shape1) - 1.0, -1.0 / shape2);
}

template <typename Rng>
inline double rinvburr(Rng &rng, double shape1, double shape2, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale * std::pow(std::pow(u, -1.0 / shape1) - 1.0, -1.0 / shape2);
}

} // namespace distributions
} // namespace mlcore
