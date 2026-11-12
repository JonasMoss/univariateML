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



inline double dlgser(double x,double theta,bool log) {
  if (!(theta>0 && theta<1) || std::isnan(x)) return special::kNaN;
  return zero_detail::density(4,x,theta,0,0,log);
}

inline double plgser(double q,double theta,bool lower_tail,bool log_p) {
  if (!(theta>0 && theta<1)) return special::kNaN;
  if(std::isnan(q)) return special::kNaN;
  if(q>=1 && q<199) {
    const int n=static_cast<int>(std::floor(q));
    double sum=0, correction=0, power=theta;
    for(int j=1;j<=n;++j) {
      const double y=power/j-correction, next=sum+y;
      correction=(next-sum)-y;sum=next;power*=theta;
    }
    const double lo=std::log(sum)-std::log(-std::log1p(-theta));
    if(lo < -1e-4)
      return special::tail_probability(special::tails_from_log(lo,true),lower_tail,log_p);
  }
  if(q>=199 && std::isfinite(q) && theta>.5) {
    const double k=std::floor(q)+1, x=-k*std::log(theta);
    double e1;
    if(x<50) e1=std::exp(x)*special::expint_e1(x);
    else {
      // Stieltjes expansion of exp(x) E1(x): next term bounds error.
      double term=1,total=1;bool closed=false;
      for(int j=1;j<=200;++j) {
        const double next=-term*j/x;
        if(std::abs(next)<std::abs(total)*2e-15) {closed=true;break;}
        if(std::abs(next)>=std::abs(term)) break;
        total+=next;term=next;
      }
      if(!closed) return special::kNaN;
      e1=total/x;
    }
    // Integrate the coth partial-fraction expansion. After B6 the
    // signed remainder is bounded by the B8 term for every x>0.
    const double u=1/k, t=-std::log(theta);
    const double c2=(t+u)*u/12;
    const double c4=-(t*t*t+3*t*t*u+6*t*u*u+6*u*u*u)*u/720;
    const double c6=(std::pow(t,5)+5*std::pow(t,4)*u+20*std::pow(t,3)*u*u+
      60*t*t*std::pow(u,3)+120*t*std::pow(u,4)+120*std::pow(u,5))*u/30240;
    const double bound=std::pow(t+7*u,7)*u/1209600;
    const double total=e1+.5*u+c2+c4+c6;
    if(bound>total*1e-12)
      return zero_detail::cdf(4,q,theta,0,0,lower_tail,log_p);
    const double upper=-x+std::log(total)-std::log(-std::log1p(-theta));
    return special::tail_probability(special::tails_from_log(upper,false),lower_tail,log_p);
  }
  return zero_detail::cdf(4,q,theta,0,0,lower_tail,log_p);
}

inline double qlgser(double p,double theta,bool lower_tail,bool log_p) {
  if (!(theta>0 && theta<1)) return special::kNaN;
  return special::discrete_quantile(special::discrete_log_tails(p,lower_tail,log_p),1,special::kPosInf,
    [&](double x) {
      const double lo=plgser(x,theta,true,true),up=plgser(x,theta,false,true);
      return lo<up?special::tails_from_log(lo,true):special::tails_from_log(up,false);
    },p,lower_tail,log_p);
}

template <typename Rng>
inline double rlgser(Rng &rng, double theta) {
  // Single runif call per sample; preserve the recurrence on ordinary inputs.
  if (!(theta>0 && theta<1)) return special::kNaN;
  double u = rng.runif(0.0, 1.0);
  double pk = -theta / std::log(1.0 - theta);
  double k = 1.0;
  int count=0;
  for (;u>pk && count<1000000;++count) {
    if (!(pk>0) || !std::isfinite(pk) || k+1==k) return special::kNaN;
    u -= pk;
    pk *= theta * k / (k + 1.0);
    k += 1.0;
  }
  return count==1000000 || std::isnan(u) ? special::kNaN : k;
}

} // namespace distributions
} // namespace cppunivariate
