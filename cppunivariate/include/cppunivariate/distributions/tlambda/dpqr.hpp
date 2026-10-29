#pragma once
#include <algorithm>
#include "../../special/tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace tlambda_detail {
inline bool valid(double mu,double sigma,double lambda) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0 && std::isfinite(lambda);
}
inline double log1mexp(double l) {
  return special::tails_from_log(l,true).upper;
}
inline double logadd(double a,double b) {
  const double m=std::max(a,b);
  return m==INFINITY ? m : m+std::log1p(std::exp(-std::abs(a-b)));
}
// log((exp(z)-1)/z), including its analytic value at zero.
inline double logexprel(double z) {
  if(std::abs(z)<1e-5) return z/2+z*z/24-z*z*z*z/2880;
  if(z>50) return z+std::log1p(-std::exp(-z))-std::log(z);
  return std::log(std::expm1(z)/z);
}
// log |Q(u)| for log(u)<=log(1/2); scale and location omitted.
inline double logmag(double lu,double lambda) {
  const double lv=std::log1p(-std::exp(lu)),delta=lv-lu;
  if(delta==0) return -INFINITY;
  if(lambda*delta>1e-5) return lambda*lv+std::log(-std::expm1(-lambda*delta))-std::log(lambda);
  if(lambda*delta< -1e-5) return lambda*lu+std::log(-std::expm1(lambda*delta))-std::log(-lambda);
  return lambda*lv+std::log(delta)+logexprel(-lambda*delta);
}
inline double logderiv(double lu,double lambda) {
  const double lv=std::log1p(-std::exp(lu));
  if(lambda==1) return std::log(2.0);
  return logadd((lambda-1)*lu,(lambda-1)*lv);
}
// Endpoint deficit 1-lambda*|Q(u)|, without subtracting nearly equal values.
inline double logdeficit(double lu,double lambda) {
  const double a=(lu< -36 && std::log(lambda)+lu< -32)
    ? std::log(lambda)+lu
    : std::log(-std::expm1(lambda*std::log1p(-std::exp(lu))));
  return logadd(a,lambda*lu);
}
// Invert on the smaller tail in t=-log(u). Newton is bracketed in log space.
inline double logsmall_log(double target,double lambda,double logslack=std::numeric_limits<double>::quiet_NaN()) {
  if(target==-INFINITY) return -std::log(2.0);
  if(lambda>0 && !std::isnan(logslack) && logslack==-INFINITY) return -INFINITY;
  if(lambda>0 && std::isnan(logslack) && target>=-std::log(lambda)) return -INFINITY;
  if(lambda==0) {const double z=std::exp(target);return -(z+std::log1p(std::exp(-z)));}
  // Q(u)=(2u-1)/(u(1-u)) at lambda=-1: stable quadratic root.
  if(lambda== -1) return std::log(2.0)-logadd(logadd(target,std::log(2.0)),.5*logadd(2*target,std::log(4.0)));
  if(lambda>0 && std::log(lambda)+target> -std::log(2.0)) {
    const double goal=std::isnan(logslack)?std::log(-std::expm1(std::log(lambda)+target)):logslack;
    double lo=std::log(2.0),hi=2;
    while(logdeficit(-hi,lambda)>goal) {
      if(hi==std::numeric_limits<double>::max()) return -INFINITY;
      hi=hi>std::numeric_limits<double>::max()/2?std::numeric_limits<double>::max():2*hi;
    }
    double t=lo+(hi-lo)/2;
    for(int k=0;k<100;++k) {
      const double d=logdeficit(-t,lambda),f=d-goal;
      if(std::abs(f)<2e-14) break;
      if(f>0)lo=t;else hi=t;
      const double slope=std::exp(std::log(lambda)-t+logderiv(-t,lambda)-d);
      double next=t+f/slope;
      if(!(next>lo && next<hi) || !std::isfinite(next)) next=lo+(hi-lo)/2;
      if(std::abs(next-t)<2e-14*(1+t)) {t=next;break;}
      t=next;
    }
    return -t;
  }
  double lo=std::log(2.0),hi=2;
  while(logmag(-hi,lambda)<target) {
    if(hi==std::numeric_limits<double>::max()) return -INFINITY;
    hi=hi>std::numeric_limits<double>::max()/2?std::numeric_limits<double>::max():2*hi;
  }
  double t=lo+(hi-lo)/2;
  for(int k=0;k<100;++k) {
    const double f=logmag(-t,lambda)-target;
    if(std::abs(f)<2e-14) break;
    if(f>0) hi=t; else lo=t;
    const double slope=std::exp(logderiv(-t,lambda)-t-logmag(-t,lambda));
    double next=t-f/slope;
    if(!(next>lo && next<hi) || !std::isfinite(next)) next=lo+(hi-lo)/2;
    if(std::abs(next-t)<2e-14*(1+t)) {t=next;break;}
    t=next;
  }
  return -t;
}
inline double logsmall(double z,double lambda) {return logsmall_log(std::log(std::abs(z)),lambda);}
inline double slack(double x,double mu,double sigma,double lambda) {
  return sigma*special::support_residual(x,mu,sigma,x>mu?-lambda:lambda);
}
inline double logstandard(double x,double mu,double sigma) {
  const double delta=x-mu;
  return (std::isfinite(delta)?std::log(std::abs(delta)):std::log(std::abs(x/2-mu/2))+std::log(2.0))-std::log(sigma);
}
}
namespace distributions {
inline double qtlambda(double p,double mu=0,double sigma=1,double lambda=0,bool lower=true,bool logp=false) {
  if(!tlambda_detail::valid(mu,sigma,lambda) || std::isnan(p) || (logp ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
  double lp=logp?p:std::log(p),lq=logp?tlambda_detail::log1mexp(p):std::log1p(-p);
  if(!lower) std::swap(lp,lq);
  if(!logp && p==0.5) return mu;
  const double h=logp ? (p+std::log(2.0))+2.3190468138462995584e-17 : 0;
  if((logp && std::abs(h)<1e-8) || (!logp && std::abs(p-0.5)<1e-8)) {
    const double mass=logp ? std::expm1(h) : 2*(p-0.5);
    const double offset=std::exp(std::log(sigma)+(1-lambda)*std::log(2.0)+std::log(std::abs(mass)));
    return mu+((mass>0)==lower?offset:-offset);
  }
  if(lp==lq) return mu;
  const bool left=lp<lq;
  const double lu=std::min(lp,lq);
  if(lu==-INFINITY && lambda>0) {
    const double width=sigma/lambda;
    return std::isfinite(width)?mu+(left?-width:width):std::fma(left?-2.0:2.0,(sigma/2)/lambda,mu);
  }
  const double lm=lu==-INFINITY ? (lambda>0 ? -std::log(lambda) : INFINITY) : tlambda_detail::logmag(lu,lambda);
  const double mag=std::exp(lm);
  if(mag>0 && std::isfinite(mag)) return std::fma(left?-sigma:sigma,mag,mu);
  const double size=std::log(sigma)+lm;
  const double offset=std::exp(size);
  if(std::isfinite(offset)) return mu+(left?-offset:offset);
  return std::fma(left?-2.0:2.0,std::exp(size-std::log(2.0)),mu);
}
inline double ptlambda(double x,double mu=0,double sigma=1,double lambda=0,bool lower=true,bool logp=false) {
  if(!tlambda_detail::valid(mu,sigma,lambda) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  const double z=(x-mu)/sigma;
  const double gap=lambda>0?tlambda_detail::slack(x,mu,sigma,lambda):1;
  const double lu=std::isinf(x) || gap<=0 ? -INFINITY : tlambda_detail::logsmall_log(tlambda_detail::logstandard(x,mu,sigma),lambda,lambda>0?std::log(gap)-std::log(sigma):std::numeric_limits<double>::quiet_NaN());
  const bool small=(z<=0)==lower;
  if(logp) return small ? lu : std::log1p(-std::exp(lu));
  return small ? std::exp(lu) : -std::expm1(lu);
}
inline double dtlambda(double x,double mu=0,double sigma=1,double lambda=0,bool log=false) {
  if(!tlambda_detail::valid(mu,sigma,lambda) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  // Keep the established one-sided density at the rounded q(0/1) endpoints
  // of finite-density members. Do not apply this alias to lambda<1: its
  // near-endpoint log density must retain the exact affine residual.
  if(lambda>=1 && std::isfinite(x)) {
    const double width=sigma/lambda;
    if(width>0 && (x==mu-width || x==mu+width)) {
      const double ld=-std::log(sigma)-(lambda==1?std::log(2.0):0);
      return log?ld:std::exp(ld);
    }
  }
  const double gap=lambda>0?tlambda_detail::slack(x,mu,sigma,lambda):1;
  const double lz=tlambda_detail::logstandard(x,mu,sigma);
  if(!std::isfinite(x) || (lambda>0 && gap<0)) return log?-INFINITY:0;
  double ld;
  if(lambda==1 || lambda==2) ld=-std::log(sigma)-(lambda==1?std::log(2.0):0);
  else if(lambda>0 && gap==0) ld=lambda<1?-INFINITY:-std::log(sigma);
  else ld=-std::log(sigma)-tlambda_detail::logderiv(tlambda_detail::logsmall_log(lz,lambda,lambda>0?std::log(gap)-std::log(sigma):std::numeric_limits<double>::quiet_NaN()),lambda);
  return log?ld:std::exp(ld);
}
template<class RNG>
inline double rtlambda(RNG& rng,double mu=0,double sigma=1,double lambda=0) {
  return qtlambda(rng.runif(0.0,1.0),mu,sigma,lambda);
}
}
}
