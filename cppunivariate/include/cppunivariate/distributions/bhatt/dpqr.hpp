#pragma once
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
inline long double logphi(long double z){return -.91893853320467274178L-z*z/2;}
inline long double mills(long double t) {
  if(t<12)return std::sqrt(std::acos(-1.0L)/2)*std::exp(t*t/2)*std::erfc(t/std::sqrt(2.0L));
  long double r=0;for(int k=40;k>=1;--k)r=k/(t+r);return 1/(t+r);
}
inline long double logQ(long double z) {
  return z>=0?logphi(z)+std::log(mills(z)):std::log1p(-std::erfc(-z/std::sqrt(2.0L))/2);
}
inline long double logdiff(long double a,long double b){return a+std::log(-std::expm1(std::min(0.0L,b-a)));}
inline long double logH(long double z) {
  if(z>=12){long double r=0;for(int k=40;k>=2;--k)r=k/(z+r);const long double h=1/(z+r);return logphi(z)+std::log(h/(z+h));}
  if(z>=0)return logphi(z)+std::log1p(-z*mills(z));
  return std::log(std::exp(logphi(z))-z*std::exp(logQ(z)));
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
inline long double narrow(long double z,long double a,bool cdf) {
  constexpr long double nodes[4]={.18343464249564980494L,.52553240991632898582L,.79666647741362673959L,.96028985649753623168L};
  constexpr long double weights[4]={.36268378337836198297L,.31370664587788728734L,.22238103445337447054L,.10122853629037625915L};
  const long double anchor=cdf?logQ(z):logphi(z);long double sum=0;
  for(int i=0;i<4;++i)for(int sign:{-1,1}) {const long double t=z+sign*a*nodes[i];sum+=weights[i]*std::exp((cdf?logQ(t):logphi(t))-anchor)/2;}
  return anchor+std::log(sum);
}
// Original central calculation: extended precision preserves fitter rounding
// only. The public caller admits this branch only within double range.
inline double central_density(double x,double mu=0,double sigma=1,double a=1,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(x))return special::kNaN;
  if(!std::isfinite(x))return log?special::kNegInf:0;
  const long double z=(static_cast<long double>(x)-mu)/sigma,b=static_cast<long double>(a)/sigma;
  long double l;
  if(b*(1+std::abs(z))<1e-8L)l=bhatt_detail::logphi(z)-std::log(static_cast<long double>(sigma));
  else if(b*(1+std::abs(z))<.01L)l=bhatt_detail::narrow(z,b,false)-std::log(static_cast<long double>(sigma));
  else {
    const long double lo=(static_cast<long double>(x)-mu-a)/sigma,hi=(static_cast<long double>(x)-mu+a)/sigma;
    if(lo==hi) {
      // The interval is below long-double coordinate resolution. Its
      // convolution correction is below double log-density precision.
      l=bhatt_detail::logphi(z)-std::log(static_cast<long double>(sigma));
      return log?static_cast<double>(l):static_cast<double>(std::exp(l));
    }
    l=(lo< -12 && hi>12)?0:(lo>=0?bhatt_detail::logdiff(bhatt_detail::logQ(lo),bhatt_detail::logQ(hi)):bhatt_detail::logdiff(bhatt_detail::logQ(-hi),bhatt_detail::logQ(-lo)));
    l-=std::log(2*static_cast<long double>(a));
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
inline double pbhatt(double x,double mu=0,double sigma=1,double a=1,bool lower=true,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(x))return special::kNaN;
  if(!std::isfinite(x)){const bool one=lower?x>0:x<0;return log?(one?0:special::kNegInf):(one?1:0);}
  const double d=std::abs(x-mu),z=std::abs(special::scaled_difference(x,mu,sigma)),b=a/sigma;
  double lp;
  if (d<a && std::log(a-d)-std::log(sigma)>std::log(40.0)) lp=std::log(a-d)-std::log(2.0)-std::log(a);
  else if(b*(1+z)<1e-8)lp=bhatt_detail::logQ(z);
  else if(b*(1+z)<.01)lp=bhatt_detail::narrow(z,b,true);
  else {
    const double lo=std::isfinite(z) && std::isfinite(b) ? z-b : (d-a)/sigma,hi=z+b;
    lp=lo==hi?bhatt_detail::logQ(z):bhatt_detail::logdiff(bhatt_detail::logH(lo),bhatt_detail::logH(hi))-(std::log(2.0)+std::log(a)-std::log(sigma));
  }
  const bool small=lower?x<=mu:x>=mu;
  const double ans=static_cast<double>(small?lp:special::log1mexp(lp));
  return log?ans:std::exp(ans);
}
inline double qbhatt(double p,double mu=0,double sigma=1,double a=1,bool lower=true,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(p)||(log?p>0:p<0||p>1))return special::kNaN;
  const double offset=special::probability_offset(p,log)*(lower?1.0:-1.0);
  if (std::abs(offset)<1e-4) {
    const double b=a/sigma;
    if (b>40) return std::fma(2*offset,a,mu);
    const double f=std::exp(dbhatt(0,0,1,b,true)), y=offset/f;
    const double curvature=-std::exp(-0.5*b*b-0.91893853320467274178)/f;
    return std::fma(sigma,y*(1-curvature*y*y/6),mu);
  }
  double lp=log?p:std::log(p);
  if(lp==special::kNegInf)return lower?special::kNegInf:special::kPosInf;
  if(lp==0)return lower?special::kPosInf:special::kNegInf;
  if(lp>-.6931471805599453){lp=std::log(-std::expm1(lp));lower=!lower;}
  const double edge=lower?-std::numeric_limits<double>::max():std::numeric_limits<double>::max();
  if(pbhatt(edge,mu,sigma,a,lower,true)>lp)return lower?special::kNegInf:special::kPosInf;
  double lo=-1,hi=1;const double scale=std::max(sigma,a);
  auto coordinate=[&](double z){return special::expm1_affine(z,mu,scale);};
  auto at=[&](double z){return pbhatt(coordinate(z),mu,sigma,a,lower,true);};
  for(int k=0;k<1100&&(lower?at(lo)>lp:at(lo)<lp);++k){lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
  for(int k=0;k<1100&&(lower?at(hi)<lp:at(hi)>lp);++k){hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
  for(int k=0;k<1100;++k){const double m=lo/2+hi/2;if(lower?at(m)<lp:at(m)>lp)lo=m;else hi=m;if(hi-lo<4e-14*std::max(1.0,std::abs(m)))break;}
  return coordinate(lo/2+hi/2);
}
template <typename Rng> inline double rbhatt(Rng& rng,double mu=0,double sigma=1,double a=1){if(!bhatt_detail::valid(mu,sigma,a))return special::kNaN;return mu+sigma*rng.rnorm()+rng.runif(-a,a);}
}
}
