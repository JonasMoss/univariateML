// Zero-inflated Poisson (ZIP) distribution (extraDistr).
// Parameters: lambda > 0, 0 <= pi <= 1; support x in {0, 1, 2, ...}.
//   p(0)   = pi + (1-pi) * exp(-lambda)
//   p(k)   = (1-pi) * dpois(k; lambda)         for k >= 1
//   F(x)   = pi + (1-pi) * ppois(x; lambda)
//   F^-1(p)= 0                                  if p < pi
//          = qpois((p - pi)/(1-pi); lambda)     otherwise
// Sample: U ~ Uniform(0, 1); return 0 if U<pi else rpois(lambda).

#pragma once
#include "../../special/tails.hpp"

#include "../pois/dpqr.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dzip(double x, double lambda, double pi, bool log) {
  if (!(lambda>0) || !std::isfinite(lambda) || !(pi>=0 && pi<=1) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<0 || x!=std::floor(x)) return log?special::kNegInf:0;
  double lp=std::log1p(-pi)+dpois(x,lambda,true);
  if (x==0) lp=special::log_sum_exp(std::log(pi),lp);
  return log?lp:std::exp(lp);
}

inline special::LogTails zip_tails(double q,double lambda,double pi) {
  if (!(lambda>0) || !std::isfinite(lambda) || !(pi>=0 && pi<=1) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  if(q<0) return special::tails_from_log(special::kNegInf,true);
  const auto t=pois_tails(q,lambda);
  const double lo=special::log_sum_exp(std::log(pi),std::log1p(-pi)+t.lower);
  const double up=std::log1p(-pi)+t.upper;
  return lo<up?special::tails_from_log(lo,true):special::tails_from_log(up,false);
}
inline double pzip(double q, double lambda, double pi, bool lower_tail, bool log_p) {
  return special::tail_probability(zip_tails(q,lambda,pi),lower_tail,log_p);
}

inline double qzip(double p, double lambda, double pi, bool lower_tail, bool log_p) {
  if (!(lambda>0) || !std::isfinite(lambda) || !(pi>=0 && pi<=1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,
      pi==1?0.0:special::kPosInf,[=](double x){return zip_tails(x,lambda,pi);},p,lower_tail,log_p);
}

template <typename Rng>
inline double rzip(Rng &rng, double lambda, double pi) {
  // Bit-for-bit port of extraDistr's rng_zip: one runif, then conditionally
  // one rpois.
  const double u = rng.runif(0.0, 1.0);
  if (u < pi) {
    return 0.0;
  }
  return rng.rpois(lambda);
}

} // namespace distributions
} // namespace cppunivariate
