#pragma once
#include "profile.hpp"
namespace cppunivariate {
namespace zero_detail {
// kind: truncated Poisson, geometric, NB, binomial, logarithmic.
inline bool valid(int kind,double a,double b) {
  if(!std::isfinite(a)||!std::isfinite(b))return false;
  if(kind==0)return a>=0;
  if(kind==1)return a>0 && a<=1;
  if(kind==2)return a>=0 && b>0 && b<=1;
  if(kind==3)return a>=1 && a==std::floor(a) && b>=0 && b<=1;
  return a>=0 && a<1;
}
inline bool point(int kind,double a,double b) {
  return (kind==0 && a==0)||(kind==1 && a==1)||(kind==2 && b==1)||
    (kind==3 && (a==1||b==0))||(kind==4 && a==0);
}
inline double logd(int kind,double x,double a,double b) {
  const double neg=-std::numeric_limits<double>::infinity();
  if(!valid(kind,a,b)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  if(!std::isfinite(x)||x<1||x!=std::floor(x)||(kind==3 && x>a))return neg;
  if(point(kind,a,b))return x==1?0:neg;
  if(kind==0)return -a+x*std::log(a)-special::lgamma(x+1)-std::log(-std::expm1(-a));
  if(kind==1)return std::log(a)+(x-1)*std::log1p(-a);
  if(kind==2 && a==0)return x*std::log1p(-b)-std::log(x)-std::log(-std::log(b));
  if(kind==2) return nb_logpmf(static_cast<int>(x),a,b)-std::log(-std::expm1(a*std::log(b)));
  if(kind==3)return bin_logpmf(static_cast<int>(x),a,b)-std::log(-std::expm1(a*std::log1p(-b)));
  return x*std::log(a)-std::log(x)-std::log(-std::log1p(-a));
}
inline double logsurvival(int kind,double q,double a,double b) {
  if(q<1)return 0;
  if(point(kind,a,b)||std::isinf(q)||(kind==3 && q>=a))return -std::numeric_limits<double>::infinity();
  q=std::floor(q);
  if(kind==1)return q*std::log1p(-a);
  double s=0;
  if(kind==0)s=special::poisson_cdf(q,a,false)/(-std::expm1(-a));
  if(kind==2 && a>0)s=special::negative_binomial_cdf(q,a,b,false)/(-std::expm1(a*std::log(b)));
  if(kind==3)s=special::binomial_cdf(q,a,b,false)/(-std::expm1(a*std::log1p(-b)));
  if(s>0)return std::log(std::min(1.0,s));
  // Log-tail summation also covers logarithmic series, without 1-CDF cancellation.
  double k=q+1, term=1, total=1;
  const double first=logd(kind,k,a,b);
  for(int i=0;i<1000000;++i) {
    double ratio=0;
    if(kind==0)ratio=a/(k+1);
    if(kind==2)ratio=(a+k)*(1-b)/(k+1);
    if(kind==3)ratio=(a-k)*b/((k+1)*(1-b));
    if(kind==4)ratio=a*k/(k+1);
    term*=ratio;total+=term;++k;
    if(term<=total*1e-16)break;
  }
  return std::min(0.0,first+std::log(total));
}
inline double density(int kind,double x,double a,double b,double p0,bool log) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  const double l=x==0 ? std::log(p0) : std::log1p(-p0)+logd(kind,x,a,b);
  return log?l:std::exp(l);
}
inline double cdf(int kind,double q,double a,double b,double p0,bool lower,bool log) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1)||std::isnan(q))return std::numeric_limits<double>::quiet_NaN();
  const double s=q<0 ? 0 : std::log1p(-p0)+logsurvival(kind,q,a,b);
  if(!lower)return log?s:std::exp(s);
  return log?std::log(-std::expm1(s)):-std::expm1(s);
}
inline double quantile(int kind,double p,double a,double b,double p0,bool lower,bool log) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1)||std::isnan(p)|| (log ? p>0 : (p<0||p>1)))return std::numeric_limits<double>::quiet_NaN();
  double ls;
  if(lower)ls=log ? std::log(-std::expm1(p)) : std::log1p(-p);
  else ls=log?p:std::log(p);
  const double start=p0==0?1:0;
  if(ls==0)return start;
  if(p0==1)return 0;
  if(point(kind,a,b))return ls>=std::log1p(-p0)?start:1;
  if(kind==3 && b==1)return ls>=std::log1p(-p0)?start:a;
  if(std::isinf(ls))return kind==3?a:std::numeric_limits<double>::infinity();
  double lo=-1,hi=1;
  while(cdf(kind,hi,a,b,p0,false,true)>ls)hi*=2;
  while(hi-lo>1) {const double mid=std::floor((hi+lo)/2);if(cdf(kind,mid,a,b,p0,false,true)<=ls)hi=mid;else lo=mid;}
  return hi;
}
}
}
