#pragma once
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
namespace cppunivariate {
namespace bhatt_detail {
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
inline long double narrow(long double z,long double a,bool cdf) {
  constexpr long double nodes[4]={.18343464249564980494L,.52553240991632898582L,.79666647741362673959L,.96028985649753623168L};
  constexpr long double weights[4]={.36268378337836198297L,.31370664587788728734L,.22238103445337447054L,.10122853629037625915L};
  const long double anchor=cdf?logQ(z):logphi(z);long double sum=0;
  for(int i=0;i<4;++i)for(int sign:{-1,1}) {const long double t=z+sign*a*nodes[i];sum+=weights[i]*std::exp((cdf?logQ(t):logphi(t))-anchor)/2;}
  return anchor+std::log(sum);
}
}
namespace distributions {
inline double dbhatt(double x,double mu=0,double sigma=1,double a=1,bool log=false) {
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
inline double pbhatt(double x,double mu=0,double sigma=1,double a=1,bool lower=true,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(x))return special::kNaN;
  if(!std::isfinite(x)){const bool one=lower?x>0:x<0;return log?(one?0:special::kNegInf):(one?1:0);}
  const long double d=std::abs(static_cast<long double>(x)-mu),z=d/sigma,b=static_cast<long double>(a)/sigma;
  long double lp;
  if(b*(1+z)<1e-8L)lp=bhatt_detail::logQ(z);
  else if(b*(1+z)<.01L)lp=bhatt_detail::narrow(z,b,true);
  else {
    const long double lo=(d-a)/sigma,hi=(d+a)/sigma;
    lp=lo==hi?bhatt_detail::logQ(z):bhatt_detail::logdiff(bhatt_detail::logH(lo),bhatt_detail::logH(hi))-std::log(2*b);
  }
  const bool small=lower?x<=mu:x>=mu;
  const double ans=static_cast<double>(small?lp:std::log1p(-std::exp(lp)));
  return log?ans:std::exp(ans);
}
inline double qbhatt(double p,double mu=0,double sigma=1,double a=1,bool lower=true,bool log=false) {
  if(!bhatt_detail::valid(mu,sigma,a)||std::isnan(p)||(log?p>0:p<0||p>1))return special::kNaN;
  double lp=log?p:std::log(p);
  if(lp==special::kNegInf)return lower?special::kNegInf:special::kPosInf;
  if(lp==0)return lower?special::kPosInf:special::kNegInf;
  if(lp>-.6931471805599453){lp=std::log(-std::expm1(lp));lower=!lower;}
  const double edge=lower?-std::numeric_limits<double>::max():std::numeric_limits<double>::max();
  if(pbhatt(edge,mu,sigma,a,lower,true)>lp)return lower?special::kNegInf:special::kPosInf;
  double lo=-1,hi=1;const double scale=std::max(sigma,a);
  auto coordinate=[&](double z){
    const long double t=std::abs(static_cast<long double>(z));
    const long double distance=t<.1L?scale*std::expm1(t):std::exp(std::log(static_cast<long double>(scale))+t)*(-std::expm1(-t));
    return static_cast<double>(static_cast<long double>(mu)+(z<0?-1:1)*distance);
  };
  auto at=[&](double z){return pbhatt(coordinate(z),mu,sigma,a,lower,true);};
  for(int k=0;k<1100&&(lower?at(lo)>lp:at(lo)<lp);++k){lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
  for(int k=0;k<1100&&(lower?at(hi)<lp:at(hi)>lp);++k){hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
  for(int k=0;k<1100;++k){const double m=lo/2+hi/2;if(lower?at(m)<lp:at(m)>lp)lo=m;else hi=m;if(hi-lo<4e-14*std::max(1.0,std::abs(m)))break;}
  return coordinate(lo/2+hi/2);
}
template <typename Rng> inline double rbhatt(Rng& rng,double mu=0,double sigma=1,double a=1){if(!bhatt_detail::valid(mu,sigma,a))return special::kNaN;return mu+sigma*rng.rnorm()+rng.runif(-a,a);}
}
}
