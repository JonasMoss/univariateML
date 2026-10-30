// Kumaraswamy distribution (extraDistr parameterisation).
// Parameters: a > 0, b > 0; support x in [0, 1].
//   f(x)   = a*b * x^(a-1) * (1 - x^a)^(b-1)
//   F(x)   = 1 - (1 - x^a)^b
//   F^-1(p)= (1 - (1-p)^(1/b))^(1/a)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dkumar(double x, double a, double b, bool log) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(a) || !std::isfinite(b) ||
      std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x > 1.0) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    const double ld = a == 1.0 ? std::log(b) : (a < 1.0 ? special::kPosInf : special::kNegInf);
    return log ? ld : std::exp(ld);
  }
  if (x == 1.0) {
    const double ld = b == 1.0 ? std::log(a) : (b < 1.0 ? special::kPosInf : special::kNegInf);
    return log ? ld : std::exp(ld);
  }
  const double lx = std::log(x);
  const double ax = a * lx;
  // If a*log(x) underflows, log(1-x^a) ~ log(a)+log(-log(x)).
  const double lc = ax == 0.0 ? std::log(a) + std::log(-lx)
                             : special::log1mexp(ax);
  const double ld = std::log(a) + std::log(b) + (a-1.0)*lx + (b-1.0)*lc;
  return log ? ld : std::exp(ld);
}

inline double pkumar(double q, double a, double b, bool lower_tail,
                     bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(a) || !std::isfinite(b) ||
      std::isnan(q)) return special::kNaN;
  if (q <= 0.0 || q >= 1.0) return special::tail_probability(
      special::tails_from_log(special::kNegInf, q <= 0.0), lower_tail, log_p);
  const double lx = std::log(q);
  const double ax = a * lx;
  const double lc = ax == 0.0 ? std::log(a) + std::log(-lx)
                             : special::log1mexp(ax);
  // log(-log upper) retains x^a even when that power underflows.
  const double lv = std::log(b) + (ax == 0.0 ? std::log(-lc)
                                            : special::log_neg_log1mexp(ax));
  const double h = -lc >= std::numeric_limits<double>::min()
      ? -b * lc : std::exp(lv);
  const auto t = lv < -36.0 ? special::tails_from_log(lv, true)
                           : special::tails_from_log(-h, false);
  return special::tail_probability(t, lower_tail, log_p);
}

inline double qkumar(double p, double a, double b, bool lower_tail,
                     bool log_p) {
  if (!(a > 0.0 && b > 0.0) || !std::isfinite(a) || !std::isfinite(b)) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return 0.0;
  if (t.upper == special::kNegInf) return 1.0;
  const double lv = (t.lower < -36.0 ? special::log_neg_log1mexp(t.lower)
                                    : std::log(-t.upper)) - std::log(b);
  if (lv < -36.0) return std::exp(lv / a);
  // v>1500 gives a quantile rounded to 1 even for the smallest positive a.
  if (lv > std::log(1500.0)) return 1.0;
  const double v = -t.upper >= std::numeric_limits<double>::min()
      ? -t.upper / b : std::exp(lv);
  // Keep the final shape transform in log space too: exp(-v) may underflow
  // before division by a brings the log quantile back into range.
  const double log_minus_lx = special::log_neg_log1mexp(-v) - std::log(a);
  return std::exp(-std::exp(log_minus_lx));
}

template <typename Rng>
inline double rkumar(Rng &rng, double a, double b) {
  const double u = rng.runif(0.0, 1.0);
  return std::pow(1.0 - std::pow(1.0 - u, 1.0 / b), 1.0 / a);
}

} // namespace distributions
} // namespace cppunivariate
