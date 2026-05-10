// Binomial distribution: scalar d/p/q/r.
// Parameters: size in {0, 1, 2, ...}, prob in [0, 1]; support x in {0, ..., size}.
//   p(k) = C(size, k) * prob^k * (1-prob)^(size-k)
//
// Boost.Math provides the distribution; discrete-quantile policy matches
// R's "smallest k with P(X<=k) >= p" convention.

#pragma once

#include "../../special/constants.hpp"

#include <boost/math/distributions/binomial.hpp>
#include <boost/math/policies/policy.hpp>
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

namespace binom_detail {
using RDiscretePolicy = boost::math::policies::policy<
    boost::math::policies::discrete_quantile<
        boost::math::policies::integer_round_up>>;
}

inline double dbinom(double x, double size, double prob, bool log) {
  if (!(size >= 0.0) || !(prob >= 0.0 && prob <= 1.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 0.0 || x > size || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  // log p(k) = lgamma(size+1) - lgamma(k+1) - lgamma(size-k+1)
  //          + k*log(prob) + (size-k)*log1p(-prob).
  // Handle 0 * log(0) cases manually (R returns 0 contribution).
  const double k = x;
  const double lc = std::lgamma(size + 1.0) - std::lgamma(k + 1.0) -
                    std::lgamma(size - k + 1.0);
  double term1 = (k == 0.0) ? 0.0 : k * std::log(prob);
  double term2 = (k == size) ? 0.0 : (size - k) * std::log1p(-prob);
  if ((k > 0.0 && prob == 0.0) || (k < size && prob == 1.0)) {
    return log ? kNegInf : 0.0;
  }
  const double log_pmf = lc + term1 + term2;
  return log ? log_pmf : std::exp(log_pmf);
}

inline double pbinom(double q, double size, double prob, bool lower_tail,
                     bool log_p) {
  if (!(size >= 0.0) || !(prob >= 0.0 && prob <= 1.0)) return kNaN;
  double cdf_val;
  if (q < 0.0) cdf_val = 0.0;
  else if (q >= size) cdf_val = 1.0;
  else {
    using boost::math::binomial_distribution;
    const binomial_distribution<double, binom_detail::RDiscretePolicy> d(
        size, prob);
    cdf_val = lower_tail ? cdf(d, std::floor(q))
                         : cdf(complement(d, std::floor(q)));
    return log_p ? std::log(cdf_val) : cdf_val;
  }
  double prob_out = lower_tail ? cdf_val : (1.0 - cdf_val);
  return log_p ? std::log(prob_out) : prob_out;
}

inline double qbinom(double p, double size, double prob, bool lower_tail,
                     bool log_p) {
  if (!(size >= 0.0) || !(prob >= 0.0 && prob <= 1.0)) return kNaN;
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  if (p <= 0.0) return 0.0;
  if (p >= 1.0) return size;
  using boost::math::binomial_distribution;
  const binomial_distribution<double, binom_detail::RDiscretePolicy> d(size,
                                                                       prob);
  return quantile(d, p);
}

template <typename Rng>
inline double rbinom(Rng &rng, double size, double prob) {
  // Inverse-CDF via Uniform; matches R's behaviour for our test sizes.
  // For large `size` the sequential-Bernoulli alternative is faster but
  // not bit-equivalent to qbinom-on-uniform; we keep the simple path.
  const double u = rng.runif(0.0, 1.0);
  using boost::math::binomial_distribution;
  const binomial_distribution<double, binom_detail::RDiscretePolicy> d(size,
                                                                       prob);
  return quantile(d, u);
}

}  // namespace distributions
}  // namespace cppunivariate
