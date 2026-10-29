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
  const bool small_lower=lower<=0.5;
  const double small=small_lower?lower:special::binomial_cdf(q,size,prob,false);
  double lp;
  if(small>1e-280) lp=std::log(small);
  else if(small_lower) {
    double k=q,term=1,total=1;
    while(k>0) {term*=k*(1-prob)/((size-k+1)*prob);total+=term;--k;const double r=k*(1-prob)/((size-k+1)*prob);if(r<1 && term*r<=total*2e-16*(1-r))break;}
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
      [=](double x){return binom_tails(x,size,prob);});
}

template <typename Rng>
inline double rbinom(Rng &rng, double size, double prob) {
  // Inverse-CDF via Uniform; matches R's behaviour for our test sizes.
  // For large `size` the sequential-Bernoulli alternative is faster but
  // not bit-equivalent to qbinom-on-uniform; we keep the simple path.
  const double u = rng.runif(0.0, 1.0);
  return cppunivariate::special::binomial_quantile(u, size, prob, true);
}

}  // namespace distributions
}  // namespace cppunivariate
