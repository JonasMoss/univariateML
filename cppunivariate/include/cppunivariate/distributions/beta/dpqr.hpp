// Beta distribution: scalar d/p/q/r.
// Parameters: shape1 > 0, shape2 > 0; support x in [0, 1].
//   f(x)    = x^(shape1-1) * (1-x)^(shape2-1) / B(shape1, shape2)
//   F(x)    = I_x(shape1, shape2)             (regularised incomplete beta)
// Sampler: gamma-ratio: g1 / (g1 + g2),
//   g1 ~ Gamma(shape1, 1), g2 ~ Gamma(shape2, 1).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dbeta(double x, double shape1, double shape2, bool log) {
  if (!(shape1 > 0.0 && shape2 > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0 || x > 1.0) {
    return log ? kNegInf : 0.0;
  }
  if (log) {
    // log f = (a-1)*log(x) + (b-1)*log1p(-x) - lbeta(a, b).
    // Use closed form for tail stability.
    if (x == 0.0) {
      if (shape1 < 1.0) return std::numeric_limits<double>::infinity();
      if (shape1 > 1.0) return kNegInf;
      // shape1 == 1: density at 0 is shape2.
    }
    if (x == 1.0) {
      if (shape2 < 1.0) return std::numeric_limits<double>::infinity();
      if (shape2 > 1.0) return kNegInf;
    }
    return (shape1 - 1.0) * std::log(x) + (shape2 - 1.0) * std::log1p(-x) -
           (std::lgamma(shape1) + std::lgamma(shape2) -
            std::lgamma(shape1 + shape2));
  }
  return cppunivariate::special::beta_pdf(x, shape1, shape2);
}

inline double pbeta(double q, double shape1, double shape2, bool lower_tail,
                    bool log_p) {
  if (!(shape1 > 0.0 && shape2 > 0.0)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) prob = lower_tail ? 0.0 : 1.0;
  else if (q >= 1.0) prob = lower_tail ? 1.0 : 0.0;
  else prob = cppunivariate::special::beta_cdf(q, shape1, shape2, lower_tail);
  return log_p ? std::log(prob) : prob;
}

inline double qbeta(double p, double shape1, double shape2, bool lower_tail,
                    bool log_p) {
  if (!(shape1 > 0.0 && shape2 > 0.0)) {
    return kNaN;
  }
  if (log_p) p = std::exp(p);
  return cppunivariate::special::beta_quantile(p, shape1, shape2, lower_tail);
}

template <typename Rng>
inline double rbeta(Rng &rng, double shape1, double shape2) {
  const double g1 = rng.rgamma(shape1, 1.0);
  const double g2 = rng.rgamma(shape2, 1.0);
  return g1 / (g1 + g2);
}

}  // namespace distributions
}  // namespace cppunivariate
