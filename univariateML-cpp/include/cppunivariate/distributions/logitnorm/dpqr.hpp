// Logit-normal law: logit(X) is normal.
#pragma once
#include "../../core/precision.hpp"
#include "../../special/special.hpp"
#include <cmath>
namespace cppunivariate {
namespace distributions {
inline double logitnorm_logtail(double z, bool lower) {
  return special::fs_normal_p(z,lower,true);
}

inline bool logitnorm_valid(double mu,double sigma) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0;
}
inline double dlogitnorm(double x,double mu,double sigma,bool log=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(x)) return special::kNaN;
  if (x<=0 || x>=1) return log ? special::kNegInf : 0;
  const double l=std::log(x), r=std::log1p(-x);
  const double v=special::dnorm(l-r,mu,sigma,true)-l-r;
  return log ? v : std::exp(v);
}
inline double plogitnorm(double q,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(q)) return special::kNaN;
  double v;
  if (q<=0) v=lower_tail ? special::kNegInf : 0;
  else if (q>=1) v=lower_tail ? 0 : special::kNegInf;
  else v=logitnorm_logtail((std::log(q)-std::log1p(-q)-mu)/sigma,lower_tail);
  return log_p ? v : std::exp(v);
}
inline double qlogitnorm(double p,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(p) || (log_p ? p>0 : p<0 || p>1)) return special::kNaN;
  if (p==(log_p ? special::kNegInf : 0)) return lower_tail ? 0 : 1;
  if (p==(log_p ? 0 : 1)) return lower_tail ? 1 : 0;
  // Always invert the smaller tail, without rounding a near-one probability.
  double lp=log_p ? p : std::log(p);
  if (lp>-0.6931471805599453) { lp=std::log(-std::expm1(lp)); lower_tail=!lower_tail; }
  const double median_lp=logitnorm_logtail(0,false);
  if (lp==median_lp)
    return mu>=0 ? 1/(1+std::exp(-mu)) : std::exp(mu)/(1+std::exp(mu));
  double lo=0,hi=std::sqrt(-lp)*2+1;
  if (!(median_lp>=lp && logitnorm_logtail(hi,false)<=lp))
    return special::kNaN;
  bool adjacent=false;
  for (int i=0;i<1100;++i) {
    const double mid=lo+(hi-lo)/2;
    if (mid==lo || mid==hi) {adjacent=true; break;}
    const double v=logitnorm_logtail(mid,false);
    if (std::isnan(v)) return special::kNaN;
    if (v>lp) lo=mid; else hi=mid;
  }
  if (!adjacent) return special::kNaN;
  const double z=mu+sigma*(lower_tail ? -hi : hi);
  return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
}
template <typename Rng>
inline double rlogitnorm(Rng &rng,double mu,double sigma) {
  if (!logitnorm_valid(mu,sigma)) return special::kNaN;
  const double z=mu+sigma*rng.rnorm();
  return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
}
}
}
