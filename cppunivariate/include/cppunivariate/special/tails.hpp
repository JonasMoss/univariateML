// Tail-accurate d/p/q rules: compute the smaller tail directly in log space;
// derive its complement with log1mexp, never 1-p near p=1. Quantiles use
// the accurate log tail, including log inputs -1e4 and below; do not first
// exponentiate them. Handle p=0/1 (log p=-Inf/0) at support endpoints.
// Preserve logarithms of underflowed intermediates analytically when needed.
// Double only: guard range with log-space algebra; long double equals
// double on some CRAN platforms. Extended precision may improve rounding,
// but must never be required to keep an intermediate in range.
// Example: Lomax log_upper=-kappa*log1p_product(lambda,x);
// q=expm1(-log_upper/kappa)/lambda (guard intermediate overflow).
#pragma once
#include "constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace special {

// Domain x <= 0; endpoints -Inf -> 0, 0 -> -Inf; invalid -> NaN.
template <typename Real>
inline auto log1mexp(Real x) -> decltype(std::log(x)) {
  if (!(x <= 0.0)) return kNaN;
  return x > -0.69314718055994530942 ? std::log(-std::expm1(x))
                                     : std::log1p(-std::exp(x));
}
inline double log_sum_exp(double x, double y) {
  if (std::isnan(x) || std::isnan(y)) return kNaN;
  if (x == kPosInf || y == kPosInf) return kPosInf;
  if (x == kNegInf) return y;
  if (y == kNegInf) return x;
  const double hi = x > y ? x : y;
  return hi + std::log1p(std::exp(-std::fabs(x-y)));
}
// log(exp(x)-exp(y)); equal finite inputs (also -Inf) give -Inf.
// Inf-Inf and y>x are undefined.
inline double log_diff_exp(double x, double y) {
  if (std::isnan(x) || std::isnan(y) || y > x || y == kPosInf) return kNaN;
  if (x == y) return kNegInf;
  if (y == kNegInf || x == kPosInf) return x;
  return x + log1mexp(y-x);
}
// log(-log(1-exp(x))) without underflowing exp(x). The omitted relative
// correction below -36 is < 1.2e-16. Domain x <= 0; -Inf -> -Inf,
// 0 -> +Inf; invalid -> NaN.
template <typename Real>
inline auto log_neg_log1mexp(Real x) -> decltype(std::log(x)) {
  if (!(x <= 0.0)) return kNaN;
  return x < -36.0 ? x : std::log(-log1mexp(x));
}

// Positive finite factors (zero/infinity also obey their usual limits).
// Keep the product in log space when multiplication overflows.
inline double log1p_product(double a, double b) {
  if (b > 1.0 && a >= std::numeric_limits<double>::max() / b)
    return std::log(a) + std::log(b);
  return std::log1p(a * b);
}
// log(log(1+a*b)), retaining a product below the subnormal range.
inline double log_log1p_product(double a, double b) {
  const double product = a * b;
  return product < std::numeric_limits<double>::min()
      ? std::log(a) + std::log(b) : std::log(log1p_product(a, b));
}
// Standardization with finite location and positive finite scale. If the
// subtraction overflows, the operands have opposite signs: dividing first
// avoids that overflow without introducing cancellation or Inf-Inf.
inline double scaled_difference(double x, double location, double scale) {
  if (!std::isfinite(x)) return x;
  const double difference = x - location;
  return std::isfinite(difference) ? difference / scale
                                 : x / scale - location / scale;
}

// Preserve relative differences near equal positive arguments.
inline double log_positive_ratio(double a, double b) {
  return a > b * 0.5 && a * 0.5 < b
      ? std::log1p((a-b)/b) : std::log(a)-std::log(b);
}
// Stable log(1+exp(x)) and its logarithm, including underflowed exp(x).
inline double softplus(double x) {
  return x > 0.0 ? x + std::log1p(std::exp(-x)) : std::log1p(std::exp(x));
}
inline double log_softplus(double x) {
  return x < -36.0 ? x : std::log(softplus(x));
}
// log(log(1+exp(a*b))), with a positive. A large positive product may
// overflow even when a later shape factor rescues the resulting hazard.
inline double log_softplus_product(double a, double b) {
  const double product = a*b;
  return product == kPosInf ? std::log(a)+std::log(b)
                           : log_softplus(product);
}
// log(expm1(exp(lv))) without forming an overflowing or underflowing expm1.
inline double log_expm1_from_log(double lv) {
  if (lv < -36.0) return lv;
  const double v = std::exp(lv);
  return v == kPosInf ? v : v + log1mexp(-v);
}

struct LogTails {
  double lower;
  double upper;
  bool valid;
};
inline LogTails tails_from_log(double log_tail, bool lower_tail) {
  if (!(log_tail <= 0.0)) return {kNaN, kNaN, false};
  const double other = log1mexp(log_tail);
  return lower_tail ? LogTails{log_tail, other, true}
                    : LogTails{other, log_tail, true};
}
// One tail is exp(-h). Keep log(h) until choosing the smaller tail so a
// tiny shape/hazard can still supply an accurate log complement.
inline LogTails tails_from_log_hazard(double log_h, bool exp_tail_lower) {
  return log_h < -36.0 ? tails_from_log(log_h, !exp_tail_lower)
                      : tails_from_log(-std::exp(log_h), exp_tail_lower);
}
// Supply both accurately computed logs, or construct them from the smaller
// tail with tails_from_log. This function never subtracts probabilities.
inline double tail_probability(const LogTails &tails, bool lower_tail, bool log_p) {
  if (!tails.valid) return kNaN;
  const double lp = lower_tail ? tails.lower : tails.upper;
  return log_p ? lp : std::exp(lp);
}
// No support/rounding semantics here: discrete callers still own flooring
// and smallest-support-point inversion, continuous callers own endpoints.
inline LogTails quantile_log_tails(double p, bool lower_tail, bool log_p) {
  if (log_p) return tails_from_log(p, lower_tail);
  if (!(p >= 0.0 && p <= 1.0)) return {kNaN, kNaN, false};
  const double lp = std::log(p);
  const double lq = std::log1p(-p);
  return lower_tail ? LogTails{lp, lq, true} : LogTails{lq, lp, true};
}

} // namespace special
} // namespace cppunivariate
