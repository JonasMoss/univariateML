// Beta-prime distribution (univariateML / extraDistr R-level: shape1,
// shape2; the optional `scale` parameter from extraDistr is fixed to 1
// here because that's what mlbetapr uses).
//
//   f(x)   = x^(shape1-1) * (1+x)^(-shape1-shape2) / B(shape1, shape2)
//   F(x)   = pbeta(x/(1+x); shape1, shape2)
//   F^-1(p)= y/(1-y) where y = qbeta(p; shape1, shape2)
// Sample: Y ~ Beta(shape1, shape2) -> X = Y/(1-Y); we draw via two gammas
//         (G1 ~ Gamma(shape1, 1), G2 ~ Gamma(shape2, 1), Y = G1/(G1+G2))
//         to keep the duck-typed Rng (no rbeta primitive) happy.

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <boost/math/distributions/beta.hpp>
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dbetapr(double x, double shape1, double shape2, bool log) {
  if (!(shape1 > 0.0 && shape2 > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0 || !std::isfinite(x)) {
    return log ? kNegInf : 0.0;
  }
  const double log_density = (shape1 - 1.0) * std::log(x) +
                             (-shape1 - shape2) * std::log1p(x) -
                             mlcore::special::lbeta(shape1, shape2);
  return log ? log_density : std::exp(log_density);
}

inline double pbetapr(double q, double shape1, double shape2,
                      bool lower_tail, bool log_p) {
  using namespace boost::math;
  if (!(shape1 > 0.0 && shape2 > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const beta_distribution<> d(shape1, shape2);
    prob = cdf(d, q / (1.0 + q));
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qbetapr(double p, double shape1, double shape2,
                      bool lower_tail, bool log_p) {
  using namespace boost::math;
  if (!(shape1 > 0.0 && shape2 > 0.0) || !std::isfinite(p)) {
    return kNaN;
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
  const beta_distribution<> d(shape1, shape2);
  const double y = quantile(d, p);
  return y / (1.0 - y);
}

template <typename Rng>
inline double rbetapr(Rng &rng, double shape1, double shape2) {
  // X = Y/(1-Y) for Y ~ Beta(shape1, shape2). Sample Y as G1/(G1+G2) with
  // G1 ~ Gamma(shape1, 1), G2 ~ Gamma(shape2, 1) -- two rgamma calls.
  const double g1 = rng.rgamma(shape1, 1.0);
  const double g2 = rng.rgamma(shape2, 1.0);
  const double y = g1 / (g1 + g2);
  return y / (1.0 - y);
}

} // namespace distributions
} // namespace mlcore
