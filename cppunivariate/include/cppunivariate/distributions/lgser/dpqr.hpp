// Logarithmic-series distribution (extraDistr).
// Parameter: 0 < theta < 1; support x in {1, 2, 3, ...}.
//   p(k) = (-1/log(1-theta)) * theta^k / k
//   F(k) = sum_{j=1}^{k} p(j)
// Sampler: inverse-CDF via Kemp-style iterative search; matches extraDistr's
// rng_lgser exactly so set.seed bit-equivalence holds.

#pragma once

#include "../../special/constants.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dlgser(double x, double theta, bool log) {
  if (!(theta > 0.0 && theta < 1.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 1.0 || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  const double a = -1.0 / std::log1p(-theta);
  const double log_density = std::log(a) + std::log(theta) * x - std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double plgser(double q, double theta, bool lower_tail, bool log_p) {
  if (!(theta > 0.0 && theta < 1.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < 1.0) {
    prob = 0.0;
  } else {
    const double a = -1.0 / std::log1p(-theta);
    double sum = 0.0;
    const long long n = static_cast<long long>(std::floor(q));
    for (long long k = 1; k <= n; ++k) {
      const double dk = static_cast<double>(k);
      sum += std::pow(theta, dk) / dk;
    }
    prob = a * sum;
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qlgser(double p, double theta, bool lower_tail, bool log_p) {
  if (!(theta > 0.0 && theta < 1.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p == 0.0) return 1.0;
  if (p >= 1.0) return std::numeric_limits<double>::infinity();
  // Iterative inverse CDF (Kemp). Matches extraDistr's invcdf_lgser.
  double pk = -theta / std::log(1.0 - theta);
  double k = 1.0;
  while (p > pk) {
    p -= pk;
    pk *= theta * k / (k + 1.0);
    k += 1.0;
  }
  return k;
}

template <typename Rng>
inline double rlgser(Rng &rng, double theta) {
  // Single runif call per sample. Matches extraDistr's rng_lgser.
  double u = rng.runif(0.0, 1.0);
  double pk = -theta / std::log(1.0 - theta);
  double k = 1.0;
  while (u > pk) {
    u -= pk;
    pk *= theta * k / (k + 1.0);
    k += 1.0;
  }
  return k;
}

} // namespace distributions
} // namespace cppunivariate
