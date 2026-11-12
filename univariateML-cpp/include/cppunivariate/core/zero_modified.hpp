// Shared exact conditional-mean equations and scalar profile search.
#pragma once
#include "work_budget.hpp"
#include "span.hpp"
#include "../distributions/lgser/mle.hpp"
#include "../distributions/binom/dpqr.hpp"
#include "../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
namespace zero_detail {
inline double nb_prob(double size, double mean,int maxit=100) {
  double lo = 0, hi = 1;
  for (int i = 0; i < solver_budget(maxit); ++i) {
    const double p = (lo + hi) / 2;
    if(p==lo||p==hi)break;
    const double m = size * (1-p) / (p * (-std::expm1(size * std::log(p))));
    if (m > mean) lo = p; else hi = p;
  }
  return hi-lo<1e-12?(lo+hi)/2:NAN;
}
inline double bin_prob(double size, double mean,int maxit=100) {
  if (mean == 1) return 0;
  if (mean == size) return 1;
  double lo = 0, hi = 1;
  for (int i = 0; i < solver_budget(maxit); ++i) {
    const double p = (lo+hi)/2;
    if(p==lo||p==hi)break;
    const double m = size*p / (-std::expm1(size*std::log1p(-p)));
    if (m < mean) lo=p; else hi=p;
  }
  return hi-lo<1e-12?(lo+hi)/2:NAN;
}
inline double mean(ispan x) { double s=0;for(int v:x)s+=v;return s/x.size(); }
// Reuse the existing logarithmic fitter where its Lambert-W iteration is
// resolved. Near mean one that iteration can fail; bracket the same exact
// mean equation instead, without changing the legacy fitter or its golden.
inline LgserResult logarithmic_fit(ispan x,int maxit=100) {
  auto out=lgser_mle(x);
  const double m=mean(x);
  if(m==1)return {0,0,true};
  if(out.theta>0 && out.theta<1 && std::isfinite(out.logLik) &&
     std::abs(-out.theta/((1-out.theta)*std::log1p(-out.theta))-m)<1e-10*m)return out;
  double lo=0,hi=1;
  for(int i=0;i<solver_budget(maxit);++i) {
    const double p=(lo+hi)/2;
    if(p==lo||p==hi)break;
    if(-p/((1-p)*std::log1p(-p))<m)lo=p;else hi=p;
  }
  if(hi-lo>=1e-12)return {NAN,NAN,false};
  out.theta=(lo+hi)/2;out.logLik=0;out.all_ones=false;
  for(int v:x)out.logLik+=v*std::log(out.theta)-std::log(v)-std::log(-std::log1p(-out.theta));
  return out;
}
// Stable rising/falling factorials even near infinite-size faces.
inline double nb_logpmf(int x, double size, double prob) {
  double l=size*std::log(prob) - special::lgamma(x+1.0);
  for(int j=0;j<x;++j) l+=std::log((size+j)*(1-prob));
  return l;
}
inline double bin_logpmf(int x, double size, double prob) {
  // Share the stable count kernel with d/p/q: the factorial and falling
  // product otherwise cancel after accumulating O(x) rounding error.
  if(size==std::floor(size))return distributions::dbinom(x,size,prob,true);
  // The size search also interpolates between integers. Its gamma-extended
  // binomial mass is beta_density(p; x+1, size-x+1)/(size+1), using the
  // same stable beta kernel as dbinom without its integer-size validation.
  if(!(size>=0) || !std::isfinite(size) || !(prob>=0 && prob<=1))return NAN;
  if(x<0 || x>size)return -std::numeric_limits<double>::infinity();
  if(prob==0)return x==0?0:-std::numeric_limits<double>::infinity();
  if(prob==1)return x==size?0:-std::numeric_limits<double>::infinity();
  return special::beta_log_density_positive(prob,x+1.,size-x+1.)-std::log(size+1.);
}
template <typename F>
inline double maximize(F f, double lo, double hi, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double r=.6180339887498948482;
  double c=hi-r*(hi-lo), d=lo+r*(hi-lo), fc=f(c), fd=f(d);
  for(int i=0;i<maxit && hi-lo>tol;++i) {
    if(fc>fd) {hi=d;d=c;fd=fc;c=hi-r*(hi-lo);fc=f(c);}
    else {lo=c;c=d;fc=fd;d=lo+r*(hi-lo);fd=f(d);}
  }
  return fc>fd ? c : d;
}
inline bool positive(ispan x) {
  if(x.size()==0) return false;
  for(int v:x) if(v<1) return false;
  return true;
}
} // namespace zero_detail
} // namespace cppunivariate
