#pragma once
#include "../../core/precision.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
namespace cppunivariate {
namespace bhatt_detail {
inline double logphi(double z){return -.91893853320467274178-(0.5*z)*z;}
inline double mills(double t) {
  if(t<12)return std::sqrt(std::acos(-1.0)/2)*std::exp(t*t/2)*std::erfc(t/std::sqrt(2.0));
  double r=0;for(int k=40;k>=1;--k)r=k/(t+r);return 1/(t+r);
}
inline double logQ(double z) {
  return special::normal_log_upper(z);
}
inline double logdiff(double a,double b){return special::log_diff_exp(a,b);}
inline double logH(double z) {
  if (z<0) return std::log(std::exp(logphi(z))-z*std::exp(logQ(z)));
  return logphi(z)+special::normal_log_mills_slope(z);
}
// Preserve the existing fitter helper signatures and rounding. The d/p/q
// tail algorithms above use the double overloads and do not borrow range.
inline CPPUNIV_EXTENDED_REAL logphi_extended(CPPUNIV_EXTENDED_REAL z){return -CPPUNIV_EXTENDED_LITERAL(.91893853320467274178,.91893853320467274178L)-z*z/2;}
inline CPPUNIV_EXTENDED_REAL mills_extended(CPPUNIV_EXTENDED_REAL t) {
  if(t<12)return std::sqrt(std::acos(-CPPUNIV_EXTENDED_LITERAL(1.0,1.0L))/2)*std::exp(t*t/2)*std::erfc(t/std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)));
  CPPUNIV_EXTENDED_REAL r=0;for(int k=40;k>=1;--k)r=k/(t+r);return 1/(t+r);
}
inline CPPUNIV_EXTENDED_REAL logQ_extended(CPPUNIV_EXTENDED_REAL z) {
  return z>=0?logphi_extended(z)+std::log(mills_extended(z)):std::log1p(-std::erfc(-z/std::sqrt(CPPUNIV_EXTENDED_LITERAL(2.0,2.0L)))/2);
}
inline CPPUNIV_EXTENDED_REAL logdiff_extended(CPPUNIV_EXTENDED_REAL a,CPPUNIV_EXTENDED_REAL b){return a+std::log(-std::expm1(std::min(CPPUNIV_EXTENDED_LITERAL(0.0,0.0L),b-a)));}
inline CPPUNIV_EXTENDED_REAL logH_extended(CPPUNIV_EXTENDED_REAL z) {
  if(z>=12){CPPUNIV_EXTENDED_REAL r=0;for(int k=40;k>=2;--k)r=k/(z+r);const CPPUNIV_EXTENDED_REAL h=1/(z+r);return logphi_extended(z)+std::log(h/(z+h));}
  if(z>=0)return logphi_extended(z)+std::log1p(-z*mills_extended(z));
  return std::log(std::exp(logphi_extended(z))-z*std::exp(logQ_extended(z)));
}
inline bool valid(double m,double s,double a){return std::isfinite(m)&&std::isfinite(s)&&s>0&&std::isfinite(a)&&a>=0;}
// Eight-point Gauss-Legendre rule for a narrow normal convolution interval.
inline double narrow(double z,double a,bool cdf) {
  constexpr double nodes[4]={.18343464249564980494,.52553240991632898582,.79666647741362673959,.96028985649753623168};
  constexpr double weights[4]={.36268378337836198297,.31370664587788728734,.22238103445337447054,.10122853629037625915};
  const double anchor=cdf?logQ(z):logphi(z);double sum=0;
  for(int i=0;i<4;++i)for(int sign:{-1,1}) {const double t=z+sign*a*nodes[i];sum+=weights[i]*std::exp((cdf?logQ(t):logphi(t))-anchor)/2;}
  return anchor+std::log(sum);
}
inline CPPUNIV_EXTENDED_REAL narrow_extended(CPPUNIV_EXTENDED_REAL z,CPPUNIV_EXTENDED_REAL a,bool cdf) {
  constexpr CPPUNIV_EXTENDED_REAL nodes[4]={CPPUNIV_EXTENDED_LITERAL(.18343464249564980494,.18343464249564980494L),CPPUNIV_EXTENDED_LITERAL(.52553240991632898582,.52553240991632898582L),CPPUNIV_EXTENDED_LITERAL(.79666647741362673959,.79666647741362673959L),CPPUNIV_EXTENDED_LITERAL(.96028985649753623168,.96028985649753623168L)};
  constexpr CPPUNIV_EXTENDED_REAL weights[4]={CPPUNIV_EXTENDED_LITERAL(.36268378337836198297,.36268378337836198297L),CPPUNIV_EXTENDED_LITERAL(.31370664587788728734,.31370664587788728734L),CPPUNIV_EXTENDED_LITERAL(.22238103445337447054,.22238103445337447054L),CPPUNIV_EXTENDED_LITERAL(.10122853629037625915,.10122853629037625915L)};
  const CPPUNIV_EXTENDED_REAL anchor=cdf?logQ_extended(z):logphi_extended(z);CPPUNIV_EXTENDED_REAL sum=0;
  for(int i=0;i<4;++i)for(int sign:{-1,1}) {const CPPUNIV_EXTENDED_REAL t=z+sign*a*nodes[i];sum+=weights[i]*std::exp((cdf?logQ_extended(t):logphi_extended(t))-anchor)/2;}
  return anchor+std::log(sum);
}
// Original central calculation: extended precision preserves fitter rounding
// only. The public caller admits this branch only within double range.
inline double central_density(double x,double mu=0,double sigma=1,double a=1,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(x))return special::kNaN;
  if(!std::isfinite(x))return log?special::kNegInf:0;
  const CPPUNIV_EXTENDED_REAL z=(static_cast<CPPUNIV_EXTENDED_REAL>(x)-mu)/sigma,b=static_cast<CPPUNIV_EXTENDED_REAL>(a)/sigma;
  CPPUNIV_EXTENDED_REAL l;
  if(b*(1+std::abs(z))<CPPUNIV_EXTENDED_LITERAL(1e-8,1e-8L))l=bhatt_detail::logphi_extended(z)-std::log(static_cast<CPPUNIV_EXTENDED_REAL>(sigma));
  else if(b*(1+std::abs(z))<CPPUNIV_EXTENDED_LITERAL(.01,.01L))l=bhatt_detail::narrow_extended(z,b,false)-std::log(static_cast<CPPUNIV_EXTENDED_REAL>(sigma));
  else {
    const CPPUNIV_EXTENDED_REAL lo=(static_cast<CPPUNIV_EXTENDED_REAL>(x)-mu-a)/sigma,hi=(static_cast<CPPUNIV_EXTENDED_REAL>(x)-mu+a)/sigma;
    if(lo==hi) {
      // The interval is below long-double coordinate resolution. Its
      // convolution correction is below double log-density precision.
      l=bhatt_detail::logphi_extended(z)-std::log(static_cast<CPPUNIV_EXTENDED_REAL>(sigma));
      return log?static_cast<double>(l):static_cast<double>(std::exp(l));
    }
    l=(lo< -12 && hi>12)?0:(lo>=0?bhatt_detail::logdiff_extended(bhatt_detail::logQ_extended(lo),bhatt_detail::logQ_extended(hi)):bhatt_detail::logdiff_extended(bhatt_detail::logQ_extended(-hi),bhatt_detail::logQ_extended(-lo)));
    l-=std::log(2*static_cast<CPPUNIV_EXTENDED_REAL>(a));
  }
  return log?static_cast<double>(l):static_cast<double>(std::exp(l));
}

}
namespace distributions {
inline double dbhatt(double x,double mu=0,double sigma=1,double a=1,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(x))return special::kNaN;
  if(!std::isfinite(x))return log?special::kNegInf:0;
  const double difference=x-mu, width=a/sigma, zz=difference/sigma;
  if (std::isfinite(difference) && std::isfinite(difference-a) &&
      std::isfinite(difference+a) && a<=std::numeric_limits<double>::max()/2 &&
      std::isfinite(width) && width<1e4 && (a==0 || width>=1e-150) &&
      std::abs(zz)<1e4 && (zz==0 || std::abs(zz)>=1e-150))
    return bhatt_detail::central_density(x,mu,sigma,a,log);

  const double z=std::abs(special::scaled_difference(x,mu,sigma)),b=static_cast<double>(a)/sigma;
  double l;
  if(b*(1+std::abs(z))<1e-8)l=bhatt_detail::logphi(z)-std::log(static_cast<double>(sigma));
  else if(b*(1+std::abs(z))<.01)l=bhatt_detail::narrow(z,b,false)-std::log(static_cast<double>(sigma));
  else {
    const double lo=std::isfinite(z) && std::isfinite(b) ? z-b : (std::abs(x-mu)-a)/sigma,hi=z+b;
    if(lo==hi) {
      // The interval is below double coordinate resolution. Its
      // convolution correction is below double log-density precision.
      l=bhatt_detail::logphi(z)-std::log(static_cast<double>(sigma));
      return log?static_cast<double>(l):static_cast<double>(std::exp(l));
    }
    l=(lo< -12 && hi>12)?0:(lo>=0?bhatt_detail::logdiff(bhatt_detail::logQ(lo),bhatt_detail::logQ(hi)):bhatt_detail::logdiff(bhatt_detail::logQ(-hi),bhatt_detail::logQ(-lo)));
    l-=(std::log(2.0)+std::log(a));
  }
  return log?static_cast<double>(l):static_cast<double>(std::exp(l));
}
inline double pbhatt(double q,double mu=0,double sigma=1,double a=1,bool lower_tail=true,bool log_p=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(q))return special::kNaN;
  if(!std::isfinite(q)){const bool one=lower_tail?q>0:q<0;return log_p?(one?0:special::kNegInf):(one?1:0);}
  const double d=std::abs(q-mu),z=std::abs(special::scaled_difference(q,mu,sigma)),b=a/sigma;
  double lp;
  if (d<a && std::log(a-d)-std::log(sigma)>std::log(40.0)) lp=std::log(a-d)-std::log(2.0)-std::log(a);
  else if(b*(1+z)<1e-8)lp=bhatt_detail::logQ(z);
  else if(b*(1+z)<.01)lp=bhatt_detail::narrow(z,b,true);
  else {
    const double lo=std::isfinite(z) && std::isfinite(b) ? z-b : (d-a)/sigma,hi=z+b;
    lp=lo==hi?bhatt_detail::logQ(z):bhatt_detail::logdiff(bhatt_detail::logH(lo),bhatt_detail::logH(hi))-(std::log(2.0)+std::log(a)-std::log(sigma));
  }
  const bool small=lower_tail?q<=mu:q>=mu;
  const double ans=static_cast<double>(small?lp:special::log1mexp(lp));
  return log_p?ans:std::exp(ans);
}
inline double qbhatt(double p,double mu=0,double sigma=1,double a=1,bool lower_tail=true,bool log_p=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(p)||(log_p?p>0:p<0||p>1))return special::kNaN;
  const double offset=special::probability_offset(p,log_p)*(lower_tail?1.0:-1.0);
  if (std::abs(offset)<1e-4) {
    const double b=a/sigma;
    if (b>40) return std::fma(2*offset,a,mu);
    const double f=std::exp(dbhatt(0,0,1,b,true)), y=offset/f;
    const double curvature=-std::exp(-0.5*b*b-0.91893853320467274178)/f;
    return std::fma(sigma,y*(1-curvature*y*y/6),mu);
  }
  double lp=log_p?p:std::log(p);
  if(lp==special::kNegInf)return lower_tail?special::kNegInf:special::kPosInf;
  if(lp==0)return lower_tail?special::kPosInf:special::kNegInf;
  if(lp>-.6931471805599453){lp=std::log(-std::expm1(lp));lower_tail=!lower_tail;}
  const double edge=lower_tail?-std::numeric_limits<double>::max():std::numeric_limits<double>::max();
  if(pbhatt(edge,mu,sigma,a,lower_tail,true)>lp)return lower_tail?special::kNegInf:special::kPosInf;
  double lo=-1,hi=1;const double scale=std::max(sigma,a);
  auto coordinate=[&](double z){return special::expm1_affine(z,mu,scale);};
  auto at=[&](double z){return pbhatt(coordinate(z),mu,sigma,a,lower_tail,true);};
  for(int k=0;k<1100&&(lower_tail?at(lo)>lp:at(lo)<lp);++k){lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
  for(int k=0;k<1100&&(lower_tail?at(hi)<lp:at(hi)>lp);++k){hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
  for(int k=0;k<1100;++k){const double m=lo/2+hi/2;if(lower_tail?at(m)<lp:at(m)>lp)lo=m;else hi=m;if(hi-lo<4e-14*std::max(1.0,std::abs(m)))break;}
  return coordinate(lo/2+hi/2);
}
template <typename Rng> inline double rbhatt(Rng& rng,double mu=0,double sigma=1,double a=1){if(!bhatt_detail::valid(mu,sigma,a))return special::kNaN;return mu+sigma*rng.rnorm()+rng.runif(-a,a);}
}
}
