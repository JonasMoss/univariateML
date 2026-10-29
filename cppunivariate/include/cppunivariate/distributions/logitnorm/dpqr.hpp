// Logit-normal law: logit(X) is normal.
#pragma once
#include "../../special/special.hpp"
#include <cmath>
namespace cppunivariate {
namespace distributions {
inline double logitnorm_logtail(double z, bool lower) {
  const double t = lower ? -z : z;
  if (t < 0) return std::log1p(-std::exp(logitnorm_logtail(-z,lower)));
  if (t > 100) {
    const double u = 1 / (t*t);
    return -0.5*t*t - std::log(t) - 0.5*std::log(2*M_PI) +
      std::log1p(-u + 3*u*u - 15*u*u*u + 105*u*u*u*u);
  }
  const long double v = special::normal_cdf_extended(z,lower);
  return static_cast<double>(std::log(v));
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
inline double plogitnorm(double q,double mu,double sigma,bool lower=true,bool log=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(q)) return special::kNaN;
  double v;
  if (q<=0) v=lower ? special::kNegInf : 0;
  else if (q>=1) v=lower ? 0 : special::kNegInf;
  else v=logitnorm_logtail((std::log(q)-std::log1p(-q)-mu)/sigma,lower);
  return log ? v : std::exp(v);
}
inline double qlogitnorm(double p,double mu,double sigma,bool lower=true,bool log=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(p) || (log ? p>0 : p<0 || p>1)) return special::kNaN;
  if (p==(log ? special::kNegInf : 0)) return lower ? 0 : 1;
  if (p==(log ? 0 : 1)) return lower ? 1 : 0;
  // Always invert the smaller tail, without rounding a near-one probability.
  double lp=log ? p : std::log(p);
  if (lp>-0.6931471805599453) { lp=std::log(-std::expm1(lp)); lower=!lower; }
  double lo=0,hi=std::sqrt(-lp)*2+1;
  for (int i=0;i<100;++i) {
    const double mid=lo+(hi-lo)/2;
    if (logitnorm_logtail(mid,false)>lp) lo=mid; else hi=mid;
  }
  const double z=mu+sigma*(lower ? -hi : hi);
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
