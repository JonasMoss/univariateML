#pragma once
#include "profile.hpp"
#include "../../special/tails.hpp"
namespace cppunivariate { namespace truncated_detail {
inline double evaluate(bool cdf,double x,double parameter,double a,double b,double size,bool lower,bool logarithm) {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(std::isnan(x)||parameter<0||!std::isfinite(parameter)||(size>=0&&parameter>1)||std::isnan(a)||std::isnan(b)||a>=b)return nan;
  const double lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size<0?INFINITY:size);
  if(lo>hi)return nan;
  if(parameter==0 || (size>=0 && parameter==1)) {
    const double point=parameter==0?0:size;
    if(point<lo||point>hi)return nan;
    const double value=cdf ? (lower ? double(x>=point) : double(x<point)) : double(x==point);
    return logarithm?std::log(value):value;
  }
  double t=size<0?std::log(parameter):std::log(parameter)-std::log1p(-parameter);
  auto z=mass(t,lo,hi,size);if(!z.ok)return nan;
  double lp;
  if(!cdf) lp=!std::isfinite(x)||x<lo||x>hi||x!=std::floor(x)?-INFINITY:kernel(x,t,size)-z.logz;
  else {
    x=std::floor(x);
    if(x<lo)lp=lower?-INFINITY:0;
    else if(x>=hi)lp=lower?0:-INFINITY;
    else {
      auto below=mass(t,lo,x,size),above=mass(t,x+1,hi,size);
      if(!below.ok||!above.ok)return nan;
      double l=below.logz-z.logz,u=above.logz-z.logz;
      if(l>u)l=std::log(-std::expm1(std::min(0.,u)));else u=std::log(-std::expm1(std::min(0.,l)));
      lp=lower?l:u;
    }
  }
  return logarithm?lp:std::exp(lp);
}
inline double quantile(double p,double parameter,double a,double b,double size,bool lower,bool logp) {
  const double lp=logp?p:std::log(p),lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size<0?INFINITY:size);
  if(std::isnan(lp)||lp>0||(!logp&&p<0))return std::numeric_limits<double>::quiet_NaN();
  if(std::isnan(evaluate(false,lo,parameter,a,b,size,true,true)))return std::numeric_limits<double>::quiet_NaN();
  if(size<0 && parameter==0 && !std::isfinite(hi))return 0;
  if(lp==-INFINITY)return lower?lo:hi;if(lp==0)return lower?hi:lo;
  return special::discrete_search(lo,hi,[&](double q) {
    if (q>9007199254740992.0) {
      const double f=evaluate(true,q,parameter,a,b,size,lower,logp);
      return std::isnan(f) ? -1 : int(lower ? f>=p : f<=p);
    }
    const double v=evaluate(true,q,parameter,a,b,size,lower,true);
    return std::isnan(v) ? -1 : int(lower ? v>=lp : v<=lp);
  });
}
} }
