#pragma once
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace skellam_detail {
inline bool valid(double a,double b) {
  return std::isfinite(a) && std::isfinite(b) && a>=0 && b>=0;
}
inline double poisson_log(double k,double rate) {
  if(rate==0) return k==0 ? 0 : -INFINITY;
  if(k==0) return -rate;
  if(k<100) return static_cast<double>(-static_cast<long double>(rate)+k*std::log(static_cast<long double>(rate))-std::lgamma(static_cast<long double>(k)+1));
  const long double n=k,a=rate,v=(a-n)/n,inv=1/n;
  long double dev;
  if(std::abs(v)<0.001L) {
    long double term=-v*v/2;dev=term;
    for(int j=3;j<16;++j) {term*=-v*(j-1)/j;dev+=term;}
    dev*=n;
  } else dev=n*std::log(a/n)+n-a;
  const long double correction=inv/12-inv*inv*inv/360+std::pow(inv,5)/1260-std::pow(inv,7)/1680;
  return static_cast<double>(dev-0.5L*std::log(6.283185307179586476925286766559L*n)-correction);
}
// Positive Bessel series centered at its largest term; uniform Debye
// expansion for large hypot(order,argument), regular also at order zero.
inline double logmass(double k,double a,double b) {
  if (k<0) {k=-k;std::swap(a,b);}
  if (b==0) return poisson_log(k,a);
  if (a==0) return k==0 ? -b : -INFINITY;
  const long double n=k,aa=a,bb=b,z=2*std::sqrt(aa)*std::sqrt(bb);
  const long double r=std::hypot(n,z);
  if (r>10000) {
    const long double t2=(n/r)*(n/r);
    const long double u1=(3-5*t2)/(24*r);
    const long double u2=(81-462*t2+385*t2*t2)/(1152*r*r);
    const long double u3=(30375-369603*t2+765765*t2*t2-425425*t2*t2*t2)/(414720*r*r*r);
    const long double exponent=((n-(aa-bb))*(n+(aa-bb)))/(r+aa+bb)
        +n*(std::asinh((aa-bb)/z)-std::asinh(n/z));
    return static_cast<double>(exponent-0.5L*std::log(6.283185307179586476925286766559L*r)+std::log1p(u1+u2+u3));
  }
  const long double product=aa*bb;
  const long double j=std::floor((z*z)/(2*(r+n)));
  const long double lp=-aa-bb+(j+n)*std::log(aa)+j*std::log(bb)
      -std::lgamma(j+n+1)-std::lgamma(j+1);
  long double total=1,term=1;
  for(long double h=j;h>0;--h) {term*=h*(h+n)/product;total+=term;if(term<total*1e-20L) break;}
  term=1;
  for(long double h=j+1;;++h) {term*=product/(h*(h+n));total+=term;if(term<total*1e-20L) break;}
  return static_cast<double>(lp+std::log(total));
}
inline double logadd(double a,double b) {
  if(a==-INFINITY) return b;
  if(b==-INFINITY) return a;
  return std::max(a,b)+std::log1p(std::exp(-std::abs(a-b)));
}
}
namespace distributions {
inline double dskellam(double x,double mu1,double mu2,bool log=false) {
  if(!skellam_detail::valid(mu1,mu2) || std::isnan(x)) return special::kNaN;
  if(!std::isfinite(x) || x!=std::floor(x)) return log ? -INFINITY : 0;
  const double lp=skellam_detail::logmass(x,mu1,mu2);
  return log ? lp : std::exp(lp);
}
inline double pskellam(double x,double a,double b,bool lower=true,bool log=false) {
  if(!skellam_detail::valid(a,b) || std::isnan(x)) return special::kNaN;
  if(!std::isfinite(x)) {const bool one=(x>0)==lower;return log ? (one?0:-INFINITY) : (one?1:0);}
  const double k=std::floor(x);
  if(!lower) return pskellam(-k-1,b,a,true,log);
  if(a==0 && b==0) return log ? (k>=0?0:-INFINITY) : (k>=0?1:0);
  if(b==0 && k<0) return log ? -INFINITY : 0;
  if(a==0 && k>=0) return log ? 0 : 1;
  const double prob=b==0 ? special::poisson_cdf(k,a,true) :
      a==0 ? special::poisson_cdf(-k-1,b,false) : k>=0 ? special::pnchisq(2*a,2*(k+1),2*b,false)
                        : special::pnchisq(2*b,-2*k,2*a,true);
  if(!log) return prob;
  if(prob>0 && prob<1) return std::log(prob);
  if(prob==1) return std::log1p(-pskellam(k,a,b,false,false));
  double total=-INFINITY;
  for(double v=k;;--v) {
    const double term=skellam_detail::logmass(v,a,b);
    total=skellam_detail::logadd(total,term);
    if(term<total-40 || v-1==v) break;
  }
  return total;
}
inline double qskellam(double p,double a,double b,bool lower=true,bool log=false) {
  if(!skellam_detail::valid(a,b) || std::isnan(p) || (log ? p>0 : (p<0 || p>1))) return special::kNaN;
  double target=log ? p : std::log(p);
  if(!lower) target=log ? std::log(-std::expm1(p)) : std::log1p(-p);
  if((log && p==-INFINITY) || (!log && p==0)) return lower ? (b==0?0:-INFINITY) : (a==0?0:INFINITY);
  if((log && p==0) || (!log && p==1)) return lower ? (a==0?0:INFINITY) : (b==0?0:-INFINITY);
  const bool use_lower=target < -0.6931471805599453;
  const double goal=use_lower ? target : (lower ? (log?std::log(-std::expm1(p)):std::log1p(-p)) : (log?p:std::log(p)));
  const auto reached=[&](double k) {const double f=pskellam(k,a,b,use_lower,true);return use_lower ? f>=goal : f<=goal;};
  double step=std::max(1.0,std::sqrt(a+b)),lo=std::floor(a-b),hi=lo;
  while(reached(lo)) {hi=lo;lo-=step;step*=2;if(!std::isfinite(lo)) return -INFINITY;}
  step=std::max(1.0,std::sqrt(a+b));
  while(!reached(hi)) {lo=hi;hi+=step;step*=2;if(!std::isfinite(hi)) return INFINITY;}
  lo=std::floor(lo);hi=std::ceil(hi);
  while(hi-lo>1) {const double mid=std::floor(lo+(hi-lo)/2);if(mid==lo || mid==hi) break;if(reached(mid)) hi=mid;else lo=mid;}
  return hi;
}
template<class RNG> inline double rskellam(RNG& rng,double a,double b) {
  if(!skellam_detail::valid(a,b)) return special::kNaN;
  const double positive=a==0 ? 0 : rng.rpois(a);
  const double negative=b==0 ? 0 : rng.rpois(b);
  return positive-negative;
}
}
}
