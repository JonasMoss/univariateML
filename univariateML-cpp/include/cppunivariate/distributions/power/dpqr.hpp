// Power distribution (extraDistr parameterisation).
// Parameters: alpha > 0 (upper bound), beta > 0 (shape); support 0 < x < alpha.
//   f(x)   = beta * x^(beta-1) / alpha^beta
//   F(x)   = (x / alpha)^beta
//   F^-1(p)= alpha * p^(1/beta)
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dpower(double x, double alpha, double beta, bool log) {
  if (!(alpha > 0.0 && std::isfinite(alpha) && beta > 0.0 && std::isfinite(beta)) || std::isnan(x)) {
    return special::kNaN;
  }
  if (x < 0.0 || x > alpha) {
    return log ? special::kNegInf : 0.0;
  }
  if (x == 0.0) {
    if (beta < 1.0) return special::kPosInf;
    if (beta > 1.0) return log ? special::kNegInf : 0.0;
    return log ? -std::log(alpha) : 1.0/alpha;
  }
  const double log_density =
      std::log(beta) - std::log(x) + beta * special::log_positive_ratio(x, alpha);
  return log ? log_density : std::exp(log_density);
}

inline double ppower(double q, double alpha, double beta, bool lower_tail, bool log_p) {
  if (!(alpha > 0.0 && std::isfinite(alpha) && beta > 0.0 && std::isfinite(beta)) || std::isnan(q)) return special::kNaN;
  const double lh = q <= 0.0 ? special::kPosInf : (q >= alpha ? special::kNegInf :
      std::log(beta) + std::log(-special::log_positive_ratio(q, alpha)));
  return special::tail_probability(special::tails_from_log_hazard(lh, true), lower_tail, log_p);
}

inline double qpower(double p, double alpha, double beta, bool lower_tail, bool log_p) {
  if (!(alpha > 0.0 && std::isfinite(alpha) && beta > 0.0 && std::isfinite(beta))) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower == special::kNegInf) return 0.0;
  if (t.upper == special::kNegInf) return alpha;
  const double lh = (t.upper < -0.6931471805599453
      ? special::log_neg_log1mexp(t.upper) : std::log(-t.lower)) - std::log(beta);
  return std::exp(std::log(alpha) - std::exp(lh));
}

template <typename Rng>
inline double rpower(Rng &rng, double alpha, double beta) {
  const double u = rng.runif(0.0, 1.0);
  return alpha * std::pow(u, 1.0 / beta);
}

} // namespace distributions
} // namespace cppunivariate
