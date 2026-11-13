#pragma once
#include "tpois_profile.hpp"
#include "../../special/tails.hpp"
#include "../../special/compensated_log.hpp"
#include "../pois/dpqr.hpp"
#include "../binom/dpqr.hpp"
namespace cppunivariate { namespace truncated_detail {
// Positive masses relative to a retained mode. The geometric remainder is
// checked independently on each side; this evaluator shares no fitter state.
struct RelativeMass {double mode,logsum;bool ok;};
inline double probability_log_ratio(double k,double parameter,double size) {
 if(size<0)return special::log_positive_ratio(parameter,k+1);
 const auto left=special::compensated_log::multiply(special::compensated_log::sum(size,-k),{parameter,0});
 const auto right=special::compensated_log::multiply({k+1,0},special::compensated_log::sum(1,-parameter));
 const auto delta=special::compensated_log::add(left,special::compensated_log::negative(right));
 if(std::abs(delta.high)<=.5*right.high) {
   const auto ratio=special::compensated_log::divide(delta,right);
   return std::log1p(ratio.high+ratio.low);
 }
 const auto difference=special::compensated_log::add(special::compensated_log::logarithm(left),special::compensated_log::negative(special::compensated_log::logarithm(right)));
 return difference.high+difference.low;
}
inline RelativeMass probability_mass(double parameter,double lo,double hi,double size) {
 const double centre=size<0?parameter:std::fma(size,parameter,parameter);
 const double mode=std::floor(std::max(lo,std::min(hi,centre)));
 if(!std::isfinite(mode))return {0,0,false};
 double total=1,total_error=0;
 for(int direction:{-1,1}) {
   double k=mode,logw=0,log_error=0;bool closed=false;
   for(int j=0;j<2000000;++j) {
     if((direction<0 && k<=lo)||(direction>0 && k>=hi)){closed=true;break;}
     const double next=k+direction;
     if(next==k)return {0,0,false};
     const double lr=direction>0?probability_log_ratio(k,parameter,size):-probability_log_ratio(next,parameter,size);
     const double step=lr-log_error,newlog=logw+step;
     log_error=(newlog-logw)-step;logw=newlog;k=next;
     const double w=std::exp(logw),addition=w-total_error,newtotal=total+addition;
     total_error=(newtotal-total)-addition;total=newtotal;
     const double ratio=std::exp(lr);
     if(ratio<1 && w*ratio/(1-ratio)<1e-17*total){closed=true;break;}
   }
   if(!closed && !((direction<0 && k<=lo)||(direction>0 && k>=hi)))return {0,0,false};
 }
 return {mode,std::log(total),std::isfinite(total)};
}
inline double probability_log_weight(double x,double mode,double parameter,double size) {
 if(x==mode)return 0;
 if(std::abs(x-mode)<=2000000 && x<=9007199254740992.0 && mode<=9007199254740992.0) {
   const int direction=x>mode?1:-1;double k=mode,value=0,error=0;
   while(k!=x) {
     const double next=k+direction;
     if(next==k)return special::kNaN;
     const double ratio=direction>0?probability_log_ratio(k,parameter,size):-probability_log_ratio(next,parameter,size);
     const double adjusted=ratio-error,sum=value+adjusted;error=(sum-value)-adjusted;value=sum;k=next;
   }
   return value;
 }
 const double first=size<0?distributions::dpois(x,parameter,true):distributions::dbinom(x,size,parameter,true);
 const double second=size<0?distributions::dpois(mode,parameter,true):distributions::dbinom(mode,size,parameter,true);
 const double difference=first-second;
 if(16*std::numeric_limits<double>::epsilon()*(std::abs(first)+std::abs(second))>1e-11*std::max(1.0,std::abs(difference)))return special::kNaN;
 return difference;
}
inline double probability_interval_ratio(const RelativeMass &part,const RelativeMass &whole,double parameter,double size) {
 if(!part.ok || !whole.ok)return special::kNaN;
 return probability_log_weight(part.mode,whole.mode,parameter,size)+part.logsum-whole.logsum;
}
inline double evaluate(bool cdf,double x,double parameter,double a,double b,double size,bool lower,bool logarithm) {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(x)||parameter<0||!std::isfinite(parameter)||(size>=0&&parameter>1)||std::isnan(a)||std::isnan(b)||a>=b)return nan;
 const double lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size<0?INFINITY:size);
 if(lo>hi)return nan;
 if(parameter==0 || (size>=0 && parameter==1)) {
   const double point=parameter==0?0:size;
   if(point<lo||point>hi)return nan;
   const double value=cdf?(lower?double(x>=point):double(x<point)):double(x==point);
   return logarithm?std::log(value):value;
 }
 if(lo==0 && ((size<0 && hi==INFINITY)||(size>=0 && hi==size)))
   return cdf?(size<0?distributions::ppois(x,parameter,lower,logarithm):distributions::pbinom(x,size,parameter,lower,logarithm))
     :(size<0?distributions::dpois(x,parameter,logarithm):distributions::dbinom(x,size,parameter,logarithm));
 if(!cdf && (!std::isfinite(x)||x<lo||x>hi||x!=std::floor(x)))return logarithm?special::kNegInf:0;
 if(cdf) {
   x=std::floor(x);
   if(x<lo)return logarithm?(lower?special::kNegInf:0):double(!lower);
   if(x>=hi)return logarithm?(lower?0:special::kNegInf):double(lower);
   if(size>=0 && parameter==.5 && lo+hi==size && 2*x+1==size)return logarithm?-.69314718055994530942:.5;
 }
 const auto whole=probability_mass(parameter,lo,hi,size);
 if(!whole.ok)return nan;
 double lp;
 if(!cdf)lp=probability_log_weight(x,whole.mode,parameter,size)-whole.logsum;
 else {
   const auto below=probability_mass(parameter,lo,x,size),above=probability_mass(parameter,x+1,hi,size);
   const double l=probability_interval_ratio(below,whole,parameter,size),u=probability_interval_ratio(above,whole,parameter,size);
   const auto tails=l<u?special::tails_from_log(l,true):special::tails_from_log(u,false);
   return special::tail_probability(tails,lower,logarithm);
 }
 return logarithm?lp:std::exp(lp);
}
inline double quantile(double p,double parameter,double a,double b,double size,bool lower,bool logp) {
  const double lp=logp?p:std::log(p),lo=std::max(0.,std::floor(a)+1),hi=std::min(std::floor(b),size<0?INFINITY:size);
  if(std::isnan(lp)||lp>0||(!logp&&p<0))return std::numeric_limits<double>::quiet_NaN();
  if(std::isnan(evaluate(false,lo,parameter,a,b,size,true,true)))return std::numeric_limits<double>::quiet_NaN();
  if(lo==0 && ((size<0 && hi==INFINITY)||(size>=0 && hi==size)))
    return size<0?distributions::qpois(p,parameter,lower,logp):distributions::qbinom(p,size,parameter,lower,logp);
  if(size<0 && parameter==0 && !std::isfinite(hi))return 0;
  if(lp==-INFINITY)return lower?lo:hi;if(lp==0)return lower?hi:lo;
  if(parameter==0 || (size>=0 && parameter==1))return parameter==0?0:size;
  const double offset=special::probability_minus_half(p,true,logp);
  double target=std::abs(offset)<.25?2*std::atanh(2*offset):lp-special::log1mexp(lp);
  if(!lower)target=-target;
  return special::discrete_search(lo,hi,[&](double q) {
    if(q<lo)return 0;
    if(q>=hi)return 1;
    if(size>=0 && parameter==.5 && lo+hi==size && 2*q+1==size)return int(target<=0);
    const auto below=probability_mass(parameter,lo,q,size),above=probability_mass(parameter,q+1,hi,size);
    if(!below.ok || !above.ok)return -1;
    const double odds=probability_log_weight(below.mode,above.mode,parameter,size)+below.logsum-above.logsum;
    return std::isnan(odds)?-1:int(odds>=target);
  });
}
} }
