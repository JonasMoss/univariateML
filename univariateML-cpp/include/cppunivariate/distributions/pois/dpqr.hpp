// Poisson distribution: scalar d/p/q/r.
// Parameter: lambda > 0; support x in {0, 1, 2, ...}.
//   p(k)    = lambda^k * exp(-lambda) / k!
//
// dpois closed-form (already in cppunivariate::special::dpois).
// p uses the direct smaller Boost tail with a bounded log-series fallback.
// q inverts that log tail to the smallest qualifying integer.

#pragma once
#include "../../special/tails.hpp"

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dpois(double x, double lambda, bool log) {
  if (!(lambda>=0) || !std::isfinite(lambda) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<0 || x!=std::floor(x)) return log?special::kNegInf:0.0;
  if(lambda==0)return x==0?(log?0:1):(log?special::kNegInf:0);
  if(x>=16) {
    const double delta=lambda-x,u=delta/x;
    const double penalty=std::abs(u)<=.5?special::scaled_positive_deviance(x,u)
      :delta-x*special::log_positive_ratio(lambda,x);
    const double lp=-.91893853320467274178-.5*std::log(x)-special::density_stirling_remainder(x)-penalty;
    return log?lp:std::exp(lp);
  }
  return special::dpois(x,lambda,log);
}

inline special::LogTails pois_tails(double q,double lambda) {
  if (!(lambda>=0) || !std::isfinite(lambda) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  if (q<0) return special::tails_from_log(special::kNegInf,true);
  if (q==special::kPosInf || lambda==0) return special::tails_from_log(special::kNegInf,false);
  q=std::floor(q);
  const double lower=special::poisson_cdf(q,lambda,true);
  if (std::isnan(lower)) return {special::kNaN,special::kNaN,false};
  const bool small_lower=lower<=0.5;
  const double small=small_lower ? lower : special::poisson_cdf(q,lambda,false);
  if (std::isnan(small)) return {special::kNaN,special::kNaN,false};
  double lp;
  if (small>1e-280) lp=std::log(small);
  else if (small_lower) {
    // Descend to zero: successive ratios decrease.
    double term=1,total=1,k=q;
    // Structural evaluator guard, as in log_series; no fitting iterlim.
    int count=0;
    for (;k>0 && count<1000000;++count) {
      term*=k/lambda;total+=term;
      if (k-1==k) return {special::kNaN,special::kNaN,false};
      --k;
      if(k<lambda && term*(k/lambda)<=total*2e-16*(1-k/lambda))break;
      if (!std::isfinite(term) || !std::isfinite(total)) return {special::kNaN,special::kNaN,false};
    }
    if (count==1000000) return {special::kNaN,special::kNaN,false};
    lp=dpois(q,lambda,true)+std::log(total);
  } else {
    const double k=q+1;
    lp=special::log_series(dpois(k,lambda,true),k,
        [=](double j){return lambda/(j+1);},[=](double j){return lambda/(j+1);});
  }
  return special::tails_from_log(lp,small_lower);
}
inline double ppois(double q, double lambda, bool lower_tail, bool log_p) {
  return special::tail_probability(pois_tails(q,lambda),lower_tail,log_p);
}

inline double qpois(double p, double lambda, bool lower_tail, bool log_p) {
  if (!(lambda>=0) || !std::isfinite(lambda)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,
      lambda==0?0.0:special::kPosInf,[=](double x){return pois_tails(x,lambda);},p,lower_tail,log_p);
}

template <typename Rng>
inline double rpois(Rng &rng, double lambda) {
  return rng.rpois(lambda);
}

}  // namespace distributions
}  // namespace cppunivariate
