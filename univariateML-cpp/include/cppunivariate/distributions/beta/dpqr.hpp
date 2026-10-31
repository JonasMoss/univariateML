// Beta distribution: scalar d/p/q/r.
// Parameters: shape1 > 0, shape2 > 0; support x in [0, 1].
//   f(x)    = x^(shape1-1) * (1-x)^(shape2-1) / B(shape1, shape2)
//   F(x)    = I_x(shape1, shape2)             (regularised incomplete beta)
// Sampler: gamma-ratio: g1 / (g1 + g2),
//   g1 ~ Gamma(shape1, 1), g2 ~ Gamma(shape2, 1).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include "../../special/beta_tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dbeta(double x, double shape1, double shape2, bool log) {
  if (!(shape1 > 0 && shape2 > 0) || !std::isfinite(shape1) ||
      !std::isfinite(shape2) || std::isnan(x)) return special::kNaN;
  double ld;
  if (x < 0 || x > 1 || std::isinf(x)) ld = special::kNegInf;
  else if (x == 0) ld = shape1 == 1 ? std::log(shape2) :
      (shape1 < 1 ? special::kPosInf : special::kNegInf);
  else if (x == 1) ld = shape2 == 1 ? std::log(shape1) :
      (shape2 < 1 ? special::kPosInf : special::kNegInf);
  else ld = (shape1-1)*std::log(x)+(shape2-1)*std::log1p(-x)
      -special::lbeta(shape1,shape2);
  return log ? ld : std::exp(ld);
}
inline double pbeta(double q, double shape1, double shape2, bool lower_tail,
                    bool log_p) {
  if (!(shape1 > 0 && shape2 > 0) || !std::isfinite(shape1) ||
      !std::isfinite(shape2) || std::isnan(q)) return special::kNaN;
  if (q <= 0 || q >= 1) return special::tail_probability(
      special::tails_from_log(special::kNegInf,q <= 0),lower_tail,log_p);
  const double lp = missing_detail::beta_log(std::log(q),std::log1p(-q),
                                            shape1,shape2,lower_tail);
  return log_p ? lp : std::exp(lp);
}
inline double qbeta(double p, double shape1, double shape2, bool lower_tail,
                    bool log_p) {
  if (!(shape1 > 0 && shape2 > 0) || !std::isfinite(shape1) ||
      !std::isfinite(shape2)) return special::kNaN;
  const auto tails = special::quantile_log_tails(p,lower_tail,log_p);
  if (!tails.valid) return special::kNaN;
  const double t = missing_detail::beta_logit_q(log_p ? p : std::log(p),
                                              shape1,shape2,lower_tail);
  return std::exp(-missing_detail::softplus(-t));
}

template <typename Rng>
inline double rbeta(Rng &rng, double shape1, double shape2) {
  const double g1 = rng.rgamma(shape1, 1.0);
  const double g2 = rng.rgamma(shape2, 1.0);
  return g1 / (g1 + g2);
}

}  // namespace distributions
}  // namespace cppunivariate
