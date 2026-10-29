// Gumbel distribution (extraDistr parameterisation; max convention).
// Parameters: location mu, scale sigma > 0.
//   z      = (x - mu) / sigma
//   f(x)   = (1/sigma) * exp(-(z + exp(-z)))
//   F(x)   = exp(-exp(-z))
//   F^-1(p)= mu - sigma * log(-log(p))
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dgumbel(double x, double mu, double sigma, bool log) {
  if (!(sigma > 0.0) || !std::isfinite(sigma) || !std::isfinite(mu) ||
      std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x)) return log ? special::kNegInf : 0.0;
  const double z = special::scaled_difference(x, mu, sigma);
  if (!std::isfinite(z)) return log ? special::kNegInf : 0.0;
  const double ld = -z - std::exp(-z) - std::log(sigma);
  return log ? ld : std::exp(ld);
}

inline double pgumbel(double q, double mu, double sigma, bool lower_tail,
                      bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(sigma) || !std::isfinite(mu) ||
      std::isnan(q)) return special::kNaN;
  const double z = special::scaled_difference(q, mu, sigma);
  // Upper log tail tends to -z: do not underflow exp(-z) first.
  const auto t = z > 36.0 ? special::tails_from_log(-z, false)
      : special::tails_from_log(-std::exp(-z), true);
  return special::tail_probability(t, lower_tail, log_p);
}

inline double qgumbel(double p, double mu, double sigma, bool lower_tail,
                      bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(sigma) || !std::isfinite(mu)) return special::kNaN;
  const auto t = special::quantile_log_tails(p, lower_tail, log_p);
  if (!t.valid) return special::kNaN;
  const double log_v = t.upper < -36.0 ? special::log_neg_log1mexp(t.upper)
                                      : std::log(-t.lower);
  // FMA keeps a finite affine result even when sigma*log_v overflows.
  return std::fma(-sigma, log_v, mu);
}

template <typename Rng>
inline double rgumbel(Rng &rng, double mu, double sigma) {
  const double u = rng.runif(0.0, 1.0);
  return mu - sigma * std::log(-std::log(u));
}

} // namespace distributions
} // namespace cppunivariate
