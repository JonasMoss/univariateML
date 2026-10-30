#pragma once
#include "../../special/special.hpp"
namespace cppunivariate { namespace ged_detail {
inline double gamma_log(double lx,double a,bool lower) {
  if(lx==special::kNegInf) return lower?special::kNegInf:0;
  const double x=std::exp(lx);
  if(!std::isfinite(x)) return lower?0:special::kNegInf;
  double lp,lq;
  if(x<a+1) {
    double term=1,sum=1;
    for(int n=1;n<10000;++n) {
      term*=x/(a+n);sum+=term;
      if(term<sum*2e-16) break;
    }
    lp=a*lx-x-std::lgamma(a+1)+std::log(sum);
    lq=special::fs_log1mexp(lp);
  } else {
    double b=x+1-a,c=1e300,d=1/b,h=d;
    for(int n=1;n<10000;++n) {
      const double an=-n*(n-a);b+=2;
      d=an*d+b;if(std::abs(d)<1e-300)d=1e-300;
      c=b+an/c;if(std::abs(c)<1e-300)c=1e-300;
      d=1/d;const double delta=d*c;h*=delta;
      if(std::abs(delta-1)<4e-16)break;
    }
    lq=a*lx-x-std::lgamma(a)+std::log(h);
    lp=special::fs_log1mexp(lq);
  }
  return lower?lp:lq;
}
inline double gamma_logq(double lp,double a,bool lower) {
  if(lp==special::kNegInf) return lower?special::kNegInf:special::kPosInf;
  if(lp==0) return lower?special::kPosInf:special::kNegInf;
  double lo=-1,hi=1;
  while(lower ? gamma_log(lo,a,true)>lp : gamma_log(lo,a,false)<lp) lo*=2;
  while(lower ? gamma_log(hi,a,true)<lp : gamma_log(hi,a,false)>lp) hi*=2;
  for(int n=0;n<120;++n) {
    const double mid=lo/2+hi/2;
    if(mid==lo || mid==hi) break;
    if(lower ? gamma_log(mid,a,true)<lp : gamma_log(mid,a,false)>lp)lo=mid;else hi=mid;
  }
  return lo/2+hi/2;
}
} }
