// Logarithmic-series distribution (extraDistr).
// Parameter: 0 < theta < 1; support x in {1, 2, 3, ...}.
//   p(k) = (-1/log(1-theta)) * theta^k / k
//   F(k) = sum_{j=1}^{k} p(j)
// Sampler: inverse-CDF via Kemp-style iterative search; matches extraDistr's
// rng_lgser exactly so set.seed bit-equivalence holds.

#pragma once
#include "../../special/tails.hpp"

#include "../ztpois/dpqr_core.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dlgser(double x,double theta,bool log) {
  if (!(theta>0 && theta<1) || std::isnan(x)) return special::kNaN;
  return zero_detail::density(4,x,theta,0,0,log);
}

inline double plgser(double q,double theta,bool lower_tail,bool log_p) {
  if (!(theta>0 && theta<1)) return special::kNaN;
  return zero_detail::cdf(4,q,theta,0,0,lower_tail,log_p);
}

inline double qlgser(double p,double theta,bool lower_tail,bool log_p) {
  if (!(theta>0 && theta<1)) return special::kNaN;
  return zero_detail::quantile(4,p,theta,0,0,lower_tail,log_p);
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
