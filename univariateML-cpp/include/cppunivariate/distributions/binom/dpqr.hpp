// Binomial distribution: scalar d/p/q/r.
// Parameters: size in {0, 1, 2, ...}, prob in [0, 1]; support x in {0, ..., size}.
//   p(k) = C(size, k) * prob^k * (1-prob)^(size-k)
//
// Direct smaller tails and bounded log sums retain extreme probabilities;
// integer log-tail inversion selects the smallest qualifying support point.

#pragma once
#include "../../special/tails.hpp"

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dbinom(double x, double size, double prob, bool log) {
  if (!(size >= 0.0) || !std::isfinite(size) || size!=std::floor(size) || !(prob >= 0.0 && prob <= 1.0) || std::isnan(x)) {
    return kNaN;
  }
  if (x < 0.0 || x > size || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  if(size==0 || prob==0 || prob==1) {
    const bool atom=prob==1?x==size:x==0;
    return log?(atom?0:special::kNegInf):(atom?1:0);
  }
  const double other=size-x,small=std::min(x,other);
  if(small<16) {
    double coefficient=0;
    for(int j=0;j<static_cast<int>(small);++j)
      coefficient+=std::log(size-j)-std::log(j+1.0);
    const double lp=coefficient+x*std::log(prob)+other*std::log1p(-prob);
    return log?lp:std::exp(lp);
  }
  if((size+1)-size!=1) {
    // The beta-density identity would round its shifted counts. Center
    // directly at np instead; fma retains np-k before either product rounds.
    const double delta=std::fma(size,prob,-x),u=delta/x,v=-delta/other;
    const double first=std::abs(u)<=.5?special::scaled_positive_deviance(x,u)
      :delta-x*(special::log_positive_ratio(size,x)+std::log(prob));
    const double second=std::abs(v)<=.5?special::scaled_positive_deviance(other,v)
      :-delta-other*(special::log_positive_ratio(size,other)+std::log1p(-prob));
    const double constant=.5*(std::log(size)-std::log(x)-std::log(other))-.91893853320467274178
      +special::density_stirling_remainder(size)-special::density_stirling_remainder(x)-special::density_stirling_remainder(other);
    const double lp=constant-first-second;
    return log?lp:std::exp(lp);
  }
  if(prob>0 && prob<1 && (size+1)-size==1 && (x+1)-x==1 && ((size-x)+1)-(size-x)==1) {
    const double lp=special::beta_log_density_positive(prob,x+1,(size-x)+1)-std::log(size+1);
    return log?lp:std::exp(lp);
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

inline special::LogTails binom_tails(double q,double size,double prob) {
  if (!(size>=0) || !std::isfinite(size) || size!=std::floor(size) || !(prob>=0 && prob<=1) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  if(q<0) return special::tails_from_log(special::kNegInf,true);
  if(q>=size || prob==0) return special::tails_from_log(special::kNegInf,false);
  if(prob==1) return special::tails_from_log(special::kNegInf,true);
  q=std::floor(q);
  const double lower=special::binomial_cdf(q,size,prob,true);
  if (std::isnan(lower)) return {special::kNaN,special::kNaN,false};
  const bool small_lower=lower<=0.5;
  const double small=small_lower?lower:special::binomial_cdf(q,size,prob,false);
  if (std::isnan(small)) return {special::kNaN,special::kNaN,false};
  double lp;
  if(small>1e-280) lp=std::log(small);
  else if(small_lower) {
    double k=q,term=1,total=1;
    // Structural evaluator guard, as in log_series; no fitting iterlim.
    int count=0;
    for (;k>0 && count<1000000;++count) {
      term*=k*(1-prob)/((size-k+1)*prob);total+=term;
      if (k-1==k) return {special::kNaN,special::kNaN,false};
      --k;
      const double r=k*(1-prob)/((size-k+1)*prob);
      if(r<1 && term*r<=total*2e-16*(1-r))break;
      if (!std::isfinite(term) || !std::isfinite(total)) return {special::kNaN,special::kNaN,false};
    }
    if (count==1000000) return {special::kNaN,special::kNaN,false};
    lp=dbinom(q,size,prob,true)+std::log(total);
  } else {
    const double k=q+1;
    lp=special::log_series(dbinom(k,size,prob,true),k,
        [=](double j){return (size-j)*prob/((j+1)*(1-prob));},
        [=](double j){return (size-j)*prob/((j+1)*(1-prob));});
  }
  return special::tails_from_log(lp,small_lower);
}
inline double pbinom(double q, double size, double prob, bool lower_tail, bool log_p) {
  return special::tail_probability(binom_tails(q,size,prob),lower_tail,log_p);
}

inline double qbinom(double p, double size, double prob, bool lower_tail, bool log_p) {
  if (!(size>=0) || !std::isfinite(size) || size!=std::floor(size) || !(prob>=0 && prob<=1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,size,
      [=](double x){return binom_tails(x,size,prob);},p,lower_tail,log_p);
}

template <typename Rng>
inline double rbinom(Rng &rng, double size, double prob) {
  if (!(size>=0) || !std::isfinite(size) || size!=std::floor(size) || !(prob>=0 && prob<=1)) return special::kNaN;
  if (size==0 || prob==0) return 0;
  if (prob==1) return size;
  // Inverse-CDF via Uniform; matches R's behaviour for our test sizes.
  // For large `size` the sequential-Bernoulli alternative is faster but
  // not bit-equivalent to qbinom-on-uniform; we keep the simple path.
  const double u = rng.runif(0.0, 1.0);
  return cppunivariate::special::binomial_quantile(u, size, prob, true);
}

}  // namespace distributions
}  // namespace cppunivariate
