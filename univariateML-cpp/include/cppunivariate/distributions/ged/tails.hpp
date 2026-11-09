#pragma once
#include "../../special/special.hpp"
namespace cppunivariate { namespace ged_detail {
// w = log(x)/nu, a = 1/nu. Keeping w avoids overflow before the
// GED power transform. For x<=1 integrate the exponential series:
// P(a,x)=x^a/Gamma(1+a) * [1+a*sum (-x)^n/(n!*(a+n))].
inline double gamma_power_log(double w,double nu,bool lower) {
  const double a=1/nu, lx=w*nu;
  if (!(a>0 && std::isfinite(a)) || std::isnan(w)) return special::kNaN;
  if(w==special::kNegInf) return lower?special::kNegInf:0;
  if(lx<=0) {
    const double x=std::exp(lx);
    double term=1,sum=0;
    bool converged=x==0;
    for(int n=1;n<=10000 && !converged;++n) {
      term*=-x/n;
      const double delta=term/(a+n);sum+=delta;
      converged=std::abs(delta)<=std::max(std::abs(sum),1e-300)*2e-16;
    }
    if(!converged) return special::kNaN;
    const double lp=w-special::lgamma1p(a)+std::log1p(a*sum);
    if(!(lp<=0)) return special::kNaN;
    return lower?lp:special::log1mexp(lp);
  }
  if(lx>std::log(std::numeric_limits<double>::max())) {
    if(a>1 && std::isfinite(lx)) return special::kNaN;
    return lower?0:special::kNegInf;
  }
  const double x=std::exp(lx);
  if(a<=1) {
    // Lentz fraction for the small upper tail, entirely in log space.
    // In particular, do not ask a backend to represent Q for tiny a.
    double b=x+1-a,c=1e300,d=1/b,h=d;
    bool converged=false;
    for(int n=1;n<=10000;++n) {
      const double an=-static_cast<double>(n)*(n-a);b+=2;
      d=an*d+b;if(std::abs(d)<1e-300)d=1e-300;
      c=b+an/c;if(std::abs(c)<1e-300)c=1e-300;
      d=1/d;const double delta=d*c;h*=delta;
      if(std::abs(delta-1)<4e-16) {converged=true;break;}
    }
    if(!converged || !(h>0)) return special::kNaN;
    const double lq=w-x+std::log(a)-special::lgamma1p(a)+std::log(h);
    if(!(lq<=0)) return special::kNaN;
    return lower?special::log1mexp(lq):lq;
  }
  return special::fs_gamma_p(x,a,lower,true);
}
// Monotone inversion in w. Both bracket comparisons must succeed;
// exhaustion or a NaN evaluator never produces an unchecked quantile.
inline double gamma_power_logq(double lp,double nu,bool lower) {
  if(std::isnan(lp)||lp>0) return special::kNaN;
  if(lp==special::kNegInf) return lower?special::kNegInf:special::kPosInf;
  if(lp==0) return lower?special::kPosInf:special::kNegInf;
  double lo=-1,hi=1;
  auto before=[&](double w) {const double v=gamma_power_log(w,nu,lower);
    return std::isnan(v)?-1:static_cast<int>(lower?v<lp:v>lp);};
  int l=before(lo),h=before(hi);
  for(int k=0;k<1024 && l==0;++k) {lo*=2;l=before(lo);}
  for(int k=0;k<1024 && h==1;++k) {hi*=2;h=before(hi);}
  if(l!=1 || h!=0 || !std::isfinite(lo) || !std::isfinite(hi)) return special::kNaN;
  for(int k=0;k<1200;++k) {
    const double mid=lo/2+hi/2;
    if(mid==lo || mid==hi) return mid;
    const int b=before(mid);if(b<0)return special::kNaN;
    if(b)lo=mid;else hi=mid;
  }
  return special::kNaN;
}
} }
