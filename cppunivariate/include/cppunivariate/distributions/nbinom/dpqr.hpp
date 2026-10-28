// Negative binomial distribution: scalar d/p/q/r.
// R's stats::dnbinom (size, prob) convention: x = number of failures before
// `size` successes, prob = success probability per trial; support x in
// {0, 1, 2, ...}. `size` may be non-integer (NB extends naturally).
//
// d closed-form (already in cppunivariate::special::dnbinom).
// p/q via Boost.Math negative_binomial_distribution with R-style discrete
// quantile policy. Sampling via the Gamma-Poisson compound: draw lambda
// ~ Gamma(size, (1-prob)/prob), then K ~ Poisson(lambda).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dnbinom(double x, double size, double prob, bool log) {
  return cppunivariate::special::dnbinom(x, size, prob, log);
}

inline double pnbinom(double q, double size, double prob, bool lower_tail,
                      bool log_p) {
  if (!(size > 0.0) || !(prob > 0.0 && prob <= 1.0)) return kNaN;
  double cdf_val;
  if (q < 0.0) {
    cdf_val = 0.0;
  } else {
    cdf_val = cppunivariate::special::negative_binomial_cdf(std::floor(q), size, prob, lower_tail);
    return log_p ? std::log(cdf_val) : cdf_val;
  }
  double prob_out = lower_tail ? cdf_val : (1.0 - cdf_val);
  return log_p ? std::log(prob_out) : prob_out;
}

inline double qnbinom(double p, double size, double prob, bool lower_tail,
                      bool log_p) {
  if (!(size > 0.0) || !(prob > 0.0 && prob <= 1.0)) return kNaN;
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  if (p <= 0.0) return 0.0;
  if (p >= 1.0) return std::numeric_limits<double>::infinity();
  return cppunivariate::special::negative_binomial_quantile(p, size, prob, true);
}

template <typename Rng>
inline double rnbinom(Rng &rng, double size, double prob) {
  // Gamma-Poisson compound: lambda ~ Gamma(size, scale=(1-prob)/prob),
  // K ~ Poisson(lambda).
  const double lambda = rng.rgamma(size, (1.0 - prob) / prob);
  return rng.rpois(lambda);
}

}  // namespace distributions
}  // namespace cppunivariate
