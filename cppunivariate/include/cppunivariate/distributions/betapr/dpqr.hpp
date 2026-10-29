#pragma once

#include "../../special/tails.hpp"
#include "beta_quantile.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dbetapr(double x, double shape1, double shape2, bool log, double scale = 1.0) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape1 < 1.0) return special::kPosInf;
    if (shape1 > 1.0) return log ? special::kNegInf : 0.0;
    const double ld = std::log(shape2)-std::log(scale);
    return log ? ld : std::exp(ld);
  }
  const double z = special::log_positive_ratio(x, scale);
  const double ld = -std::log(x) - special::lbeta(shape1,shape2)
      - shape1 * special::softplus(-z) - shape2 * special::softplus(z);
  return log ? ld : std::exp(ld);
}

inline double pbetapr(double q, double shape1, double shape2, bool lower_tail, bool log_p, double scale = 1.0) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale)) || std::isnan(q)) return special::kNaN;
  if (q <= 0.0) return special::tail_probability(special::tails_from_log(special::kNegInf, true), lower_tail, log_p);
  if (q == special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf, false), lower_tail, log_p);
  const double z = special::log_positive_ratio(q, scale);
  // GB2 continuation retains both beta coordinates beyond probability underflow.
  const double lp = missing_detail::beta_log(-special::softplus(-z), -special::softplus(z), shape1, shape2, lower_tail);
  return log_p ? lp : std::exp(lp);
}

inline double qbetapr(double p, double shape1, double shape2, bool lower_tail, bool log_p, double scale = 1.0) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  const bool lower = t.lower < t.upper;
  return std::exp(std::log(scale) + betapr_detail::beta_logit_quantile(lower ? t.lower : t.upper, shape1, shape2, lower));
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
