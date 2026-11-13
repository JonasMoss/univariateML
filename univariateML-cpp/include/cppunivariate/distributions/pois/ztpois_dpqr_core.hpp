#pragma once
#include "../../special/tails.hpp"
#include "ztpois_profile.hpp"
#include "../pois/dpqr.hpp"
#include "../nbinom/dpqr.hpp"
#include "../binom/dpqr.hpp"
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
  if(kind==0)return distributions::dpois(x,a,true)-std::log(-std::expm1(-a));
  if(kind==1)return std::log(a)+(x-1)*std::log1p(-a);
  if(kind==2 && a==0)return x*std::log1p(-b)-std::log(x)-std::log(-std::log(b));
  if(kind==2) return distributions::dnbinom(x,a,b,true)-std::log(-std::expm1(a*std::log(b)));
  if(kind==3)return distributions::dbinom(x,a,b,true)-std::log(-std::expm1(a*std::log1p(-b)));
  return x*std::log(a)-std::log(x)-std::log(-std::log1p(-a));
}
inline special::LogTails positive_tails(int kind,double q,double a,double b) {
  if(q<1) return special::tails_from_log(special::kNegInf,true);
  if(point(kind,a,b)||q==special::kPosInf||(kind==3 && q>=a)) return special::tails_from_log(special::kNegInf,false);
  if(kind==3 && b==1) return special::tails_from_log(special::kNegInf,true);
  q=std::floor(q);
  // Size-one NB conditioned positive is exactly geometric on 1,2,...
  if(kind==2 && a==1) return positive_tails(1,q,b,0);
  if(kind==1) return special::tails_from_log(q*std::log1p(-a),false);
  if(kind==0 || (kind==2 && a>0) || kind==3) {
    const double zero=kind==0 ? -a : kind==2 ? a*std::log(b) : a*std::log1p(-b);
    const double base=kind==0 ? distributions::ppois(q,a,true,true) : kind==2
        ? distributions::pnbinom(q,a,b,true,true) : distributions::pbinom(q,a,b,true,true);
    // Subtract the zero mass only when the base lower tail is at least
    // twice as large. The relative conditioning error is then bounded.
    if(std::isfinite(base) && base<=-0.6931471805599453 && base-zero>0.6931471805599453) {
      const double lower=base+special::log1mexp(zero-base)-special::log1mexp(zero);
      return special::tails_from_log(std::min(0.0,lower),true);
    }
  }
  double upper;
  if(kind==0) upper=distributions::ppois(q,a,false,true)-std::log(-std::expm1(-a));
  else if(kind==2 && a>0) upper=distributions::pnbinom(q,a,b,false,true)-std::log(-std::expm1(a*std::log(b)));
  else if(kind==3) upper=distributions::pbinom(q,a,b,false,true)-std::log(-std::expm1(a*std::log1p(-b)));
  else {
    const double theta=kind==4?a:1-b, k=q+1;
    upper=special::log_series(logd(kind,k,a,b),k,
        [=](double j){return theta*j/(j+1);},[=](double){return theta;});
  }
  if(std::isnan(upper)) return {special::kNaN,special::kNaN,false};
  if(upper < -0.6931471805599453) return special::tails_from_log(upper,false);
  // The conditional lower tail may be tiny even when the base CDF is large.
  // Sum it directly rather than subtracting the zero mass or near-one survival.
  double lower=logd(kind,1,a,b), term=lower;
  double k=1;
  // Structural CDF guard; an unfinished prefix is a numerical failure.
  int count=0;
  for(;k<q && count<1000000;++count,++k) {
    double ratio;
    if(kind==0) ratio=a/(k+1);
    else if(kind==2 && a>0) ratio=(a+k)*(1-b)/(k+1);
    else if(kind==3) ratio=(a-k)*b/((k+1)*(1-b));
    else ratio=(kind==4?a:1-b)*k/(k+1);
    term+=std::log(ratio);lower=special::log_sum_exp(lower,term);
    if (k+1==k || std::isnan(lower)) return {special::kNaN,special::kNaN,false};
  }
  if (count==1000000) return {special::kNaN,special::kNaN,false};
  return special::tails_from_log(std::min(0.0,lower),true);
}
inline double logsurvival(int kind,double q,double a,double b) {
  return positive_tails(kind,q,a,b).upper;
}
inline double density(int kind,double x,double a,double b,double p0,bool log) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  const double l=x==0 ? std::log(p0) : std::log1p(-p0)+logd(kind,x,a,b);
  return log?l:std::exp(l);
}
inline special::LogTails zero_tails(int kind,double q,double a,double b,double p0) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1)||std::isnan(q))return {special::kNaN,special::kNaN,false};
  if(q<0)return special::tails_from_log(special::kNegInf,true);
  const auto t=positive_tails(kind,q,a,b);
  const double lo=special::log_sum_exp(std::log(p0),std::log1p(-p0)+t.lower);
  const double up=std::log1p(-p0)+t.upper;
  return lo<up?special::tails_from_log(lo,true):special::tails_from_log(up,false);
}
inline double cdf(int kind,double q,double a,double b,double p0,bool lower,bool log) {
  return special::tail_probability(zero_tails(kind,q,a,b,p0),lower,log);
}
inline double quantile(int kind,double p,double a,double b,double p0,bool lower,bool log,double start=-1,bool fuzz=true) {
  if(!valid(kind,a,b)||!(p0>=0 && p0<=1))return special::kNaN;
  if(std::isnan(p)||(log ? p>0 : (p<0 || p>1)))return special::kNaN;
  // Retain the component map only for sampling. Public ties are compared
  // on the supplied modified-law scale, including its atom at zero.
  if(!fuzz && p0==0.5) {
    const double offset=special::probability_minus_half(p,lower,log);
    if(offset<=0)return 0;
    if(offset<0.25)return quantile(kind,2*offset,a,b,0,true,false,1,false);
  }
  // Modification wrappers explicitly retain zero even when its mass is zero.
  // The default preserves positive-truncation callers (including lgser).
  if(start<0) start=p0==0?1:0;
  const double end=kind==3?a:(p0==1?0:(point(kind,a,b)?1:special::kPosInf));
  return special::discrete_quantile(special::discrete_log_tails(p,lower,log),start,end,
      [=](double x){return zero_tails(kind,x,a,b,p0);},p,lower,log,fuzz);
}
}
}
