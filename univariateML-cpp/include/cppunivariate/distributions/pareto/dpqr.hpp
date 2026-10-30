// Pareto distribution (Type I, extraDistr parameterisation).
// Parameters: shape a > 0, scale (lower bound) b > 0; support x >= b.
//   f(x)   = a * b^a / x^(a+1)
//   F(x)   = 1 - (b/x)^a
//   F^-1(p)= b / (1-p)^(1/a)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dpareto(double x, double a, double b, bool log) {
  if (!(a > 0.0 && std::isfinite(a) && b > 0.0 && std::isfinite(b)) || std::isnan(x)) {
    return special::kNaN;
  }
  if (x < b || x == special::kPosInf) {
    return log ? special::kNegInf : 0.0;
  }
  const double log_density =
      std::log(a) - std::log(x) - a * special::log_positive_ratio(x, b);
  return log ? log_density : std::exp(log_density);
}

inline double ppareto(double q, double a, double b, bool lower_tail, bool log_p) {
  if (!(a > 0.0 && std::isfinite(a) && b > 0.0 && std::isfinite(b)) || std::isnan(q)) return special::kNaN;
  const double lh = q <= b ? special::kNegInf :
      std::log(a) + std::log(special::log_positive_ratio(q, b));
  return special::tail_probability(special::tails_from_log_hazard(lh, false), lower_tail, log_p);
}

inline double qpareto(double p, double a, double b, bool lower_tail, bool log_p) {
  if (!(a > 0.0 && std::isfinite(a) && b > 0.0 && std::isfinite(b))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return b;
  if (t.upper == special::kNegInf) return special::kPosInf;
  const double lh = (t.lower < -0.6931471805599453
      ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper)) - std::log(a);
  return std::exp(std::log(b) + std::exp(lh));
}

template <typename Rng>
inline double rpareto(Rng &rng, double a, double b) {
  const double u = rng.runif(0.0, 1.0);
  return b / std::pow(1.0 - u, 1.0 / a);
}

} // namespace distributions
} // namespace cppunivariate
