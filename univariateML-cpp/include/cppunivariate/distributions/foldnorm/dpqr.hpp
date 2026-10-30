#pragma once
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
namespace cppunivariate {
namespace foldnorm_detail {
inline bool valid(double mu, double sigma) {
  return std::isfinite(mu) && mu>=0 && std::isfinite(sigma) && sigma>0;
}
inline double logadd(double a,double b) {
  if (a==special::kNegInf) return b;
  if (b==special::kNegInf) return a;
  const double hi=std::max(a,b);
  return hi+std::log1p(std::exp(std::min(a,b)-hi));
}
inline double log1mexp(double a) {
  return a < -0.6931471805599453 ? std::log1p(-std::exp(a)) : std::log(-std::expm1(a));
}
// Normal upper tail, with a Mills expansion where erfc underflows.
inline double logupper(double z) {
  if (z<26) return std::log(0.5*std::erfc(z/std::sqrt(2.0)));
  if (!std::isfinite(z)) return special::kNegInf;
  const double t=1/(z*z);
  double sum=1,term=1;
  for (int k=1;k<18;++k) { term*=-(2*k-1)*t; sum+=term; }
  return -0.5*z*z-std::log(z)-0.9189385332046727+std::log(sum);
}
}
namespace distributions {
inline double dfoldnorm(double x,double mu,double sigma,bool log=false) {
  if (!foldnorm_detail::valid(mu,sigma) || std::isnan(x)) return special::kNaN;
  if (x<0 || !std::isfinite(x)) return log ? special::kNegInf : 0;
  const double z=(x-mu)/sigma;
  const double ld=-std::log(sigma)-0.9189385332046727-0.5*z*z+
    (x==0 || mu==0 ? std::log(2.0) : std::log1p(std::exp(-2*(x/sigma)*(mu/sigma))));
  return log ? ld : std::exp(ld);
}
inline double pfoldnorm(double q,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!foldnorm_detail::valid(mu,sigma) || std::isnan(q)) return special::kNaN;
  if (q<=0) return log_p ? (lower_tail ? special::kNegInf : 0) : (lower_tail ? 0 : 1);
  if (!std::isfinite(q)) return log_p ? (lower_tail ? 0 : special::kNegInf) : (lower_tail ? 1 : 0);
  const double h=q/sigma,m=mu/sigma,zlo=(q-mu)/sigma,zhi=h+m;
  double lp;
  if (lower_tail) {
    if (h*(1+m)<0.1) {
      // Eight-point Gauss-Legendre integral on [-h,h], factored at its midpoint.
      const double nodes[4]={.1834346424956498,.525532409916329,.7966664774136267,.9602898564975363};
      const double weights[4]={.362683783378362,.3137066458778873,.2223810344533745,.1012285362903763};
      double s=0;
      for (int k=0;k<4;++k) { const double u=h*nodes[k]; s+=weights[k]*std::exp(-.5*u*u)*2*std::cosh(m*u); }
      lp=std::log(q)-std::log(sigma)-.5*m*m-.9189385332046727+std::log(s);
    } else if (q<=mu) {
      const double a=foldnorm_detail::logupper(-zlo),b=foldnorm_detail::logupper(zhi);
      // When both log tails round to the same huge magnitude, retain
      // their leading log ratio rather than subtracting identical doubles.
      const double difference=b==a ? -2*m*h : b-a;
      lp=a==special::kNegInf ? a : a+foldnorm_detail::log1mexp(difference);
    } else {
      const double u=foldnorm_detail::logadd(foldnorm_detail::logupper(zlo),foldnorm_detail::logupper(zhi));
      lp=foldnorm_detail::log1mexp(std::min(0.0,u));
    }
  } else {
    lp=foldnorm_detail::logadd(foldnorm_detail::logupper(zlo),foldnorm_detail::logupper(zhi));
    // A large tail's logarithm can be tiny even though its probability
    // rounds to one. Complement the directly computed small log tail.
    if (lp>-.6931471805599453)
      lp=foldnorm_detail::log1mexp(pfoldnorm(q,mu,sigma,true,true));
  }
  return log_p ? lp : std::exp(lp);
}
inline double qfoldnorm(double p,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!foldnorm_detail::valid(mu,sigma) || std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return special::kNaN;
  double lp=log_p ? p : std::log(p);
  if (lp==special::kNegInf) return lower_tail ? 0 : special::kPosInf;
  if (lp==0) return lower_tail ? special::kPosInf : 0;
  // Always invert the smaller tail, preserving log probabilities.
  if (lp>-.6931471805599453) { lp=foldnorm_detail::log1mexp(lp); lower_tail=!lower_tail; }
  double lo=0,hi=std::max(mu,sigma);
  while (lower_tail ? pfoldnorm(hi,mu,sigma,true,true)<lp : pfoldnorm(hi,mu,sigma,false,true)>lp) {
    const double largest=std::numeric_limits<double>::max();
    if (hi==largest) return special::kPosInf;
    hi=hi>largest/2 ? largest : hi*2;
  }
  for (int k=0;k<2200;++k) {
    const double mid=lo+(hi-lo)*.5;
    if (mid==lo || mid==hi) break;
    const double v=pfoldnorm(mid,mu,sigma,lower_tail,true);
    if (lower_tail ? v<lp : v>lp) lo=mid; else hi=mid;
  }
  return lo+(hi-lo)*.5;
}
template <typename Rng>
inline double rfoldnorm(Rng &rng,double mu,double sigma) {
  if (!foldnorm_detail::valid(mu,sigma)) return special::kNaN;
  return std::abs(mu+sigma*rng.rnorm());
}
}
}
