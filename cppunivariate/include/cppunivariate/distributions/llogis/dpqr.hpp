// Log-logistic distribution (actuar R-level: shape + rate; scale = 1/rate).
//   v = (x*rate)^shape = (x/scale)^shape
//   f(x)   = shape * v / (x * (1+v)^2)
//   F(x)   = v / (1+v) = 1 / (1 + (rate*x)^(-shape))
//   F^-1(p)= scale * (p/(1-p))^(1/shape)

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dllogis(double x, double shape, double rate, bool log) {
  if (!(shape > 0.0 && rate > 0.0) || !std::isfinite(x) ||
      !std::isfinite(shape)) {
    return kNaN;
  }
  if (x < 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double scale = 1.0 / rate;
  if (x == 0.0) {
    if (shape > 1.0) return log ? kNegInf : 0.0;
    if (shape < 1.0) return std::numeric_limits<double>::infinity();
    return log ? -std::log(scale) : 1.0 / scale;
  }
  // log(f) = log(shape) - log(x) - log1pexp(-logv) - log1pexp(logv)
  // where logv = shape*(log(x) - log(scale)) = shape*(log(x*rate)).
  const double logv = shape * (std::log(x) - std::log(scale));
  const double log1pexp_pos = std::log1p(std::exp(logv));
  const double log1pexp_neg = std::log1p(std::exp(-logv));
  const double log_density =
      std::log(shape) - log1pexp_neg - log1pexp_pos - std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double pllogis(double q, double shape, double rate, bool lower_tail,
                      bool log_p) {
  if (!(shape > 0.0 && rate > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const double scale = 1.0 / rate;
    const double zlog = shape * (std::log(scale) - std::log(q));
    prob = std::exp(-std::log1p(std::exp(zlog)));
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qllogis(double p, double shape, double rate, bool lower_tail,
                      bool log_p) {
  if (!(shape > 0.0 && rate > 0.0) || !std::isfinite(p)) {
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
  const double scale = 1.0 / rate;
  return scale * std::pow(p / (1.0 - p), 1.0 / shape);
}

template <typename Rng>
inline double rllogis(Rng &rng, double shape, double rate) {
  const double u = rng.runif(0.0, 1.0);
  const double scale = 1.0 / rate;
  return scale * std::pow(u / (1.0 - u), 1.0 / shape);
}

} // namespace distributions
} // namespace cppunivariate
