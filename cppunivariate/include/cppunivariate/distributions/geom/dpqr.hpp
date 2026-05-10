// Geometric distribution: scalar d/p/q/r.
// R's stats::dgeom convention: x = number of failures before the first
// success; support x in {0, 1, 2, ...}.
// Parameter: prob in (0, 1].
//   p(k)    = prob * (1 - prob)^k
//   F(k)    = 1 - (1 - prob)^(k+1)            for k >= 0
//   F^-1(p) = ceil(log1p(-p) / log1p(-prob)) - 1   (smallest k with CDF >= p)
//
// Sampler: inverse-CDF on U ~ Uniform(0, 1).

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dgeom(double x, double prob, bool log) {
  return cppunivariate::special::dgeom(x, prob, log);
}

inline double pgeom(double q, double prob, bool lower_tail, bool log_p) {
  if (!(prob > 0.0 && prob <= 1.0)) return kNaN;
  double cdf;
  if (q < 0.0) {
    cdf = 0.0;
  } else {
    // F(k) = 1 - (1-prob)^(k+1); use -expm1 for tail accuracy.
    cdf = -std::expm1((std::floor(q) + 1.0) * std::log1p(-prob));
  }
  double prob_out = lower_tail ? cdf : (1.0 - cdf);
  return log_p ? std::log(prob_out) : prob_out;
}

inline double qgeom(double p, double prob, bool lower_tail, bool log_p) {
  if (!(prob > 0.0 && prob <= 1.0)) return kNaN;
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  if (p <= 0.0) return 0.0;
  if (p >= 1.0) return std::numeric_limits<double>::infinity();
  // Smallest integer k >= log1p(-p) / log1p(-prob) - 1.
  const double k = std::ceil(std::log1p(-p) / std::log1p(-prob)) - 1.0;
  return k < 0.0 ? 0.0 : k;
}

template <typename Rng>
inline double rgeom(Rng &rng, double prob) {
  // Inverse-CDF via Uniform.
  const double u = rng.runif(0.0, 1.0);
  const double k = std::ceil(std::log1p(-u) / std::log1p(-prob)) - 1.0;
  return k < 0.0 ? 0.0 : k;
}

}  // namespace distributions
}  // namespace cppunivariate
