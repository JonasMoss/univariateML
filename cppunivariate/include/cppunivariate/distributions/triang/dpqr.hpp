#pragma once
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline bool triang_valid(double a,double b,double c) {
  return std::isfinite(a)&&std::isfinite(b)&&std::isfinite(c)&&a<b&&a<=c&&c<=b;
}
inline double dtriang(double x,double a,double b,double c,bool log=false) {
  if(!triang_valid(a,b,c)||std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double v=-INFINITY;
  if(x>=a&&x<=b) {
    v=std::log(2.0)-std::log(b-a);
    if(x<c) v+=std::log((x-a)/(c-a));
    if(x>c) v+=std::log((b-x)/(b-c));
  }
  return log?v:std::exp(v);
}
inline double ptriang(double x,double a,double b,double c,bool lower_tail=true,bool log_p=false) {
  if(!triang_valid(a,b,c)||std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  double p;
  if(x<=a) p=0;
  else if(x>=b) p=1;
  else if(x<=c) p=((x-a)/(b-a))*((x-a)/(c-a));
  else p=1-((b-x)/(b-a))*((b-x)/(b-c));
  if(!lower_tail) p=1-p;
  return log_p?std::log(p):p;
}
inline double qtriang(double p,double a,double b,double c,bool lower_tail=true,bool log_p=false) {
  if(log_p) p=std::exp(p);
  if(!triang_valid(a,b,c)||!(p>=0&&p<=1)) return std::numeric_limits<double>::quiet_NaN();
  if(!lower_tail) p=1-p;
  const double t=(c-a)/(b-a);
  return p<=t?a+(b-a)*std::sqrt(p*t):b-(b-a)*std::sqrt((1-p)*(1-t));
}
template<class Rng> inline double rtriang(Rng& rng,double a,double b,double c) {
  return qtriang(rng.runif(0,1),a,b,c);
}
} // namespace distributions
} // namespace cppunivariate
