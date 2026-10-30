#pragma once
#include "../../special/tails.hpp"
#include <cmath>
namespace cppunivariate { namespace distributions {
inline double dburr(double x, double shape1, double shape2, double scale, bool log) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    const double power = shape2;
    if (power < 1.0) return special::kPosInf;
    if (power > 1.0) return log ? special::kNegInf : 0.0;
    const double ld = std::log(shape1) + std::log(shape2) - std::log(scale);
    return log ? ld : std::exp(ld);
  }
  const double ratio = special::log_positive_ratio(x, scale);
  const double z = shape2 * ratio;
  // Retain log(shape1 * log1p(exp(z))) even if exp(z) underflows.
  const double h = std::exp(std::log(shape1) + special::log_softplus_product(shape2, ratio));
  const double ld = std::log(shape1) + std::log(shape2) - h
      - special::softplus(-z) - std::log(x);
  return log ? ld : std::exp(ld);
}
inline double pburr(double q, double shape1, double shape2, double scale, bool lower_tail, bool log_p) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale)) || std::isnan(q)) return special::kNaN;
  if (q <= 0.0) return special::tail_probability(special::tails_from_log(special::kNegInf, true), lower_tail, log_p);
  const double ratio = special::log_positive_ratio(q, scale);
  const double lh = std::log(shape1) + special::log_softplus_product(shape2, ratio);
  const auto t = special::tails_from_log_hazard(lh, false);
  return special::tail_probability(t, lower_tail, log_p);
}
inline double qburr(double p, double shape1, double shape2, double scale, bool lower_tail, bool log_p) {
  if (!(shape1 > 0.0 && std::isfinite(shape1) && shape2 > 0.0 && std::isfinite(shape2) && scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return 0.0;
  if (t.upper == special::kNegInf) return special::kPosInf;
  const double lv = (t.lower < -0.6931471805599453 ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper)) - std::log(shape1);
  // Above v=36, log(expm1(v)) rounds to v; divide before v can overflow.
  const double displacement = lv > std::log(36.0)
      ? std::exp(lv - std::log(shape2)) : special::log_expm1_from_log(lv) / shape2;
  return std::exp(std::log(scale) + displacement);
}
template <typename Rng>
inline double rburr(Rng &rng, double shape1, double shape2, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale *
         std::pow(std::pow(1.0 - u, -1.0 / shape1) - 1.0, 1.0 / shape2);
}

} }
