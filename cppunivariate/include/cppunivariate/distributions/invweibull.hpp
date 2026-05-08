// Inverse Weibull (Frechet) distribution (actuar R-level: shape + rate;
// scale = 1/rate).
//   u = (rate*x)^(-shape) = (1/(rate*x))^shape, equivalently (scale/x)^shape.
//   f(x)   = shape * u * exp(-u) / x   with scale = 1/rate
//   F(x)   = exp(-(scale/x)^shape) = exp(-1/(rate*x)^shape)
//   F^-1(p)= scale / (-log(p))^(1/shape)

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dinvweibull(double x, double shape, double rate, bool log) {
  if (!(shape > 0.0 && rate > 0.0) || !std::isfinite(x) ||
      !std::isfinite(shape) || !std::isfinite(rate)) {
    return kNaN;
  }
  if (x <= 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double scale = 1.0 / rate;
  const double logu = shape * (std::log(scale) - std::log(x));
  const double log_density =
      std::log(shape) + logu - std::exp(logu) - std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double pinvweibull(double q, double shape, double rate,
                          bool lower_tail, bool log_p) {
  if (!(shape > 0.0 && rate > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const double scale = 1.0 / rate;
    const double u = std::exp(shape * (std::log(scale) - std::log(q)));
    prob = std::exp(-u);
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qinvweibull(double p, double shape, double rate,
                          bool lower_tail, bool log_p) {
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
  return scale / std::pow(-std::log(p), 1.0 / shape);
}

template <typename Rng>
inline double rinvweibull(Rng &rng, double shape, double rate) {
  const double u = rng.runif(0.0, 1.0);
  const double scale = 1.0 / rate;
  return scale / std::pow(-std::log(u), 1.0 / shape);
}

} // namespace distributions
} // namespace cppunivariate
