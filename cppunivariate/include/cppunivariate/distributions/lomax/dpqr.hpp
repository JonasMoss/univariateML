// Lomax distribution (Pareto Type II, extraDistr parameterisation).
// Parameters: rate lambda > 0, shape kappa > 0; support x > 0.
//   f(x)   = lambda*kappa / (1 + lambda*x)^(kappa+1)
//   F(x)   = 1 - (1 + lambda*x)^(-kappa)
//   F^-1(p)= ((1-p)^(-1/kappa) - 1) / lambda
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dlomax(double x, double lambda, double kappa, bool log) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(lambda) ||
      !std::isfinite(kappa) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  const double lp = special::log1p_product(lambda, x);
  const double log_h = std::log(kappa) + special::log_log1p_product(lambda, x);
  const double h = lambda * x >= std::numeric_limits<double>::min()
      ? kappa * lp : std::exp(log_h);
  const double ld = std::log(lambda) + std::log(kappa) - lp - h;
  return log ? ld : std::exp(ld);
}

inline double plomax(double q, double lambda, double kappa, bool lower_tail,
                     bool log_p) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(lambda) ||
      !std::isfinite(kappa) || std::isnan(q)) return special::kNaN;
  if (q <= 0.0) return special::tail_probability(
      special::tails_from_log(special::kNegInf, true), lower_tail, log_p);
  const double lp = special::log1p_product(lambda, q);
  const double log_h = std::log(kappa) + special::log_log1p_product(lambda, q);
  // Preserve a tiny product until multiplication by shape can rescue it.
  const double h = lambda * q >= std::numeric_limits<double>::min()
      ? kappa * lp : std::exp(log_h);
  const auto t = log_h < -36.0 ? special::tails_from_log(log_h, true)
                              : special::tails_from_log(-h, false);
  return special::tail_probability(t, lower_tail, log_p);
}

inline double qlomax(double p, double lambda, double kappa, bool lower_tail,
                     bool log_p) {
  if (!(lambda > 0.0 && kappa > 0.0) || !std::isfinite(lambda) ||
      !std::isfinite(kappa)) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return 0.0;
  if (t.upper == special::kNegInf) return special::kPosInf;
  // Keep log(-log upper) even when log upper rounds to negative zero.
  const double lv = (t.lower < -0.6931471805599453 ?
      special::log_neg_log1mexp(t.lower) : std::log(-t.upper)) - std::log(kappa);
  if (lv < -36.0) {
    // Keep ordinary division's precision when both the input tail and ratio
    // are normal; otherwise retain the ratio until the rate can rescue it.
    if (-t.upper >= std::numeric_limits<double>::min()) {
      const double v = -t.upper / kappa;
      if (v >= std::numeric_limits<double>::min()) return std::expm1(v) / lambda;
    }
    return std::exp(lv - std::log(lambda));
  }
  // v>1500 implies overflow even after division by the largest finite rate.
  if (lv > std::log(1500.0)) return special::kPosInf;
  // A subnormal complement has lost relative precision: use its log instead.
  const double v = -t.upper >= std::numeric_limits<double>::min()
      ? -t.upper / kappa : std::exp(lv);
  if (v > 36.0) return std::exp(v - std::log(lambda));
  return std::expm1(v) / lambda;
}

template <typename Rng>
inline double rlomax(Rng &rng, double lambda, double kappa) {
  const double u = rng.runif(0.0, 1.0);
  return (std::pow(1.0 - u, -1.0 / kappa) - 1.0) / lambda;
}

} // namespace distributions
} // namespace cppunivariate
