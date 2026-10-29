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

// Probability minus one half, for quantiles near the median. The low part
// of log(2) retains the offset of a rounded log(0.5) input from the true median.
// Do not use this centered representation to reconstruct extreme tails.
inline double probability_minus_half(double p, bool lower_tail, bool log_p) {
  const double offset = log_p ? 0.5 * std::expm1(
      (p + 0.69314718055994528623) + 2.3190468138462995584e-17) : p - 0.5;
  return lower_tail ? offset : -offset;
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
