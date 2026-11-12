// Negative binomial distribution: scalar d/p/q/r.
// R's stats::dnbinom (size, prob) convention: x = number of failures before
// `size` successes, prob = success probability per trial; support x in
// {0, 1, 2, ...}. `size` may be non-integer (NB extends naturally).
//
// d closed-form (already in cppunivariate::special::dnbinom).
// p uses the direct smaller Boost tail with a bounded log-series fallback.
// q uses integer inversion of log tails. Sampling draws lambda
// ~ Gamma(size, (1-prob)/prob), then K ~ Poisson(lambda).

#pragma once
#include "../../special/tails.hpp"

#include "../../special/constants.hpp"
#include "../../special/special.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {


inline double dnbinom(double x, double size, double prob, bool log) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1) || std::isnan(x)) return special::kNaN;
  if (!std::isfinite(x) || x<0 || x!=std::floor(x)) return log?special::kNegInf:0;
  if(size==0 || prob==1) return x==0?(log?0:1):(log?special::kNegInf:0);
  if(x==0) {const double lp=size*std::log(prob);return log?lp:std::exp(lp);}
  if((x+1)-x==1 && std::isfinite(size+x)) {
    const double lp=special::beta_log_density_positive(prob,size,x+1)+std::log(prob)-std::log(size+x);
    return log?lp:std::exp(lp);
  }
  if(size>=16 && x>=16) {
    int exponent=0;std::frexp(std::max(size,x),&exponent);
    const double nr=std::scalbn(size,-exponent),nx=std::scalbn(x,-exponent);
    const double d=special::beta_center_residual(prob,nr,nx),delta=std::scalbn(d,exponent);
    const double u=d/nr,v=-d/nx;
    const double first=std::abs(u)<=.5?special::scaled_positive_deviance(size,u)
      :delta-size*(std::log1p(x/size)+std::log(prob));
    const double second=std::abs(v)<=.5?special::scaled_positive_deviance(x,v)
      :-delta-x*(std::log1p(size/x)+std::log1p(-prob));
    const double logsum=std::log(std::max(size,x))+std::log1p(std::min(size,x)/std::max(size,x));
    const double constant=.5*(std::log(size)-std::log(x)-logsum)-.91893853320467274178
      +special::density_stirling_remainder(size+x)-special::density_stirling_remainder(size)-special::density_stirling_remainder(x);
    const double lp=constant-first-second;
    return log?lp:std::exp(lp);
  }
  double lp;
  if (x>1e8*std::max(1.0,size*size)) {
    // log Gamma(x+size)/Gamma(x+1), with O(size^3/x^2) omitted.
    lp=(size-1)*std::log(x)+size*(size-1)/(2*x)-special::lgamma(size)
        +size*std::log(prob)+x*std::log1p(-prob);
  } else lp=special::log_gamma_ratio(size,x,0)-special::lgamma(x+1)
      +size*std::log(prob)+x*std::log1p(-prob);
  return log?lp:std::exp(lp);
}

inline special::LogTails nbinom_tails(double q,double size,double prob) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1) || std::isnan(q)) return {special::kNaN,special::kNaN,false};
  if(q<0) return special::tails_from_log(special::kNegInf,true);
  if(q==special::kPosInf || size==0 || prob==1) return special::tails_from_log(special::kNegInf,false);
  q=std::floor(q);
  // Size one is geometric; its survival has an exact logarithmic form.
  if(size==1) return special::tails_from_log((q+1)*std::log1p(-prob),false);
  const double lower=special::negative_binomial_cdf(q,size,prob,true);
  if (std::isnan(lower)) return {special::kNaN,special::kNaN,false};
  const bool small_lower=lower<=0.5;
  const double small=small_lower?lower:special::negative_binomial_cdf(q,size,prob,false);
  if (std::isnan(small)) return {special::kNaN,special::kNaN,false};
  double lp;
  if(small>1e-280) lp=std::log(small);
  else if(small_lower) {
    double k=q,term=1,total=1;
    // Structural evaluator guard, as in log_series; no fitting iterlim.
    int count=0;
    for (;k>0 && count<1000000;++count) {
      term*=k/((size+k-1)*(1-prob));total+=term;
      if (k-1==k) return {special::kNaN,special::kNaN,false};
      --k;
      const double r=k/((size+k-1)*(1-prob));
      if(size>=1 && r<1 && term*r<=total*2e-16*(1-r))break;
      if (!std::isfinite(term) || !std::isfinite(total)) return {special::kNaN,special::kNaN,false};
    }
    if (count==1000000) return {special::kNaN,special::kNaN,false};
    lp=dnbinom(q,size,prob,true)+std::log(total);
  } else {
    const double k=q+1;
    lp=special::log_series(dnbinom(k,size,prob,true),k,
        [=](double j){return (1+(size-1)/(j+1))*(1-prob);},
        [=](double j){return (size>1?1+(size-1)/(j+1):1)*(1-prob);});
  }
  return special::tails_from_log(lp,small_lower);
}
inline double pnbinom(double q, double size, double prob, bool lower_tail, bool log_p) {
  return special::tail_probability(nbinom_tails(q,size,prob),lower_tail,log_p);
}

inline double qnbinom(double p, double size, double prob, bool lower_tail, bool log_p) {
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),0.0,
      (size==0 || prob==1)?0.0:special::kPosInf,[=](double x){return nbinom_tails(x,size,prob);},p,lower_tail,log_p);
}

template <typename Rng>
inline double rnbinom(Rng &rng, double size, double prob) {
  // Gamma-Poisson compound: lambda ~ Gamma(size, scale=(1-prob)/prob),
  // K ~ Poisson(lambda).
  if (!(size>=0) || !std::isfinite(size) || !(prob>0 && prob<=1)) return special::kNaN;
  if (size==0 || prob==1) return 0;
  const double lambda = rng.rgamma(size, (1.0 - prob) / prob);
  return rng.rpois(lambda);
}

}  // namespace distributions
}  // namespace cppunivariate
