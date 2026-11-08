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
#include "../../special/tails.hpp"

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dgeom(double x, double prob, bool log) {
  if (!(prob>0 && prob<=1) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<0 || x!=std::floor(x)) return log?special::kNegInf:0;
  const double lp=prob==1?(x==0?0:special::kNegInf):std::log(prob)+x*std::log1p(-prob);
  return log?lp:std::exp(lp);
}

inline special::LogTails geom_tails(double q,double prob) {
  if (!(prob>0 && prob<=1) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  return special::tails_from_log(q<0?0:(std::floor(q)+1)*std::log1p(-prob),false);
}
inline double pgeom(double q, double prob, bool lower_tail, bool log_p) {
  return special::tail_probability(geom_tails(q,prob),lower_tail,log_p);
}

inline double qgeom(double p, double prob, bool lower_tail, bool log_p) {
  if (!(prob>0 && prob<=1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,
      prob==1?0.0:special::kPosInf,[=](double x){return geom_tails(x,prob);},p,lower_tail,log_p);
}

template <typename Rng>
inline double rgeom(Rng &rng, double prob) {
  // Inverse-CDF via Uniform.
  const double u = rng.runif(0.0, 1.0);
  const double k = std::ceil(std::log1p(-u) / std::log1p(-prob)) - 1.0;
  if (k>=9007199254740992.0) return qgeom(u,prob,true,false);
  return k < 0.0 ? 0.0 : k;
}

}  // namespace distributions
}  // namespace cppunivariate
