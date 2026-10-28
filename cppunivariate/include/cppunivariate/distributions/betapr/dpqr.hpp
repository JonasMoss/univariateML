#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dbetapr(double x, double shape1, double shape2, bool log, double scale = 1.0) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0 && std::isfinite(scale)) || !std::isfinite(x)) {
    return cppunivariate::special::kNaN;
  }
  if (x <= 0.0 || !std::isfinite(x)) {
    return log ? cppunivariate::special::kNegInf : 0.0;
  }
  const double log_density = (shape1 - 1.0) * std::log(x / scale) - std::log(scale) +
                             (-shape1 - shape2) * std::log1p(x / scale) -
                             cppunivariate::special::lbeta(shape1, shape2);
  return log ? log_density : std::exp(log_density);
}

inline double pbetapr(double q, double shape1, double shape2,
                      bool lower_tail, bool log_p, double scale = 1.0) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0 && std::isfinite(scale)) || !std::isfinite(q)) {
    return cppunivariate::special::kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    prob = cppunivariate::special::beta_cdf(q / (scale + q), shape1, shape2, true);
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qbetapr(double p, double shape1, double shape2,
                      bool lower_tail, bool log_p, double scale = 1.0) {
  if (!(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0 && std::isfinite(scale)) || !std::isfinite(p)) {
    return cppunivariate::special::kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p <= 0.0) {
    return 0.0;
  }
  if (p >= 1.0) {
    return std::numeric_limits<double>::infinity();
  }
  const double y = cppunivariate::special::beta_quantile(p, shape1, shape2, true);
  return scale * y / (1.0 - y);
}

template <typename Rng>
inline double rbetapr(Rng &rng, double shape1, double shape2, double scale = 1.0) {
  // X = Y/(1-Y) for Y ~ Beta(shape1, shape2). Sample Y as G1/(G1+G2) with
  // G1 ~ Gamma(shape1, 1), G2 ~ Gamma(shape2, 1) -- two rgamma calls.
  const double g1 = rng.rgamma(shape1, 1.0);
  const double g2 = rng.rgamma(shape2, 1.0);
  const double y = g1 / (g1 + g2);
  return scale * y / (1.0 - y);
}

} // namespace distributions
} // namespace cppunivariate
