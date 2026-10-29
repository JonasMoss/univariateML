#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace missing_detail {
// Local beta log-coordinate bridge, replaceable by special/tails.hpp.
inline double softplus(double x) { return std::fmax(x,0)+std::log1p(std::exp(-std::abs(x))); }
inline double logprob(double p,bool log_p) {
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 return log_p ? p : std::log(p);
}
// Evaluate the favourable incomplete-beta continued fraction. The prefactor
// remains in log coordinates even when exp(lx) underflows to zero.
inline double beta_small(double lx,double ly,double a,double b,double p) {
 const double x=std::exp(lx);
 if(p>=std::numeric_limits<double>::min()) return std::log(p);
 return a*lx+b*ly-cppunivariate::special::lbeta(a,b)
   +std::log(cppunivariate::special::fs_beta_fraction(a,b,x)/a);
}
inline double beta_log(double lx,double ly,double a,double b,bool lower) {
 const double p=lx<-.6931471805599453 ? cppunivariate::special::ibeta(a,b,std::exp(lx)) : cppunivariate::special::ibetac(b,a,std::exp(ly));
 const double q=ly<-.6931471805599453 ? cppunivariate::special::ibeta(b,a,std::exp(ly)) : cppunivariate::special::ibetac(a,b,std::exp(lx));
 if(p<=q) {
  const double lp=beta_small(lx,ly,a,b,p);
  return lower ? lp : cppunivariate::special::fs_log1mexp(lp);
 }
 const double lq=beta_small(ly,lx,b,a,q);
 return lower ? cppunivariate::special::fs_log1mexp(lq) : lq;
}
// Invert in log odds, preserving both beta coordinates beyond double's
// probability range. Monotone bisection also avoids rounded-to-one inverses.
inline double beta_logit_q(double lp,double a,double b,bool lower) {
 if(std::isnan(lp)) return lp;
 if(lp>-.6931471805599453) return beta_logit_q(cppunivariate::special::fs_log1mexp(lp),a,b,!lower);
 if(lp==-std::numeric_limits<double>::infinity()) return lower ? lp : -lp;
 if(lp>-700) {
  const double prob=std::exp(lp);
  const double x=lower ? cppunivariate::special::ibeta_inv(a,b,prob) : cppunivariate::special::ibetac_inv(a,b,prob);
  if(x>=std::numeric_limits<double>::min() && x<=.5) return std::log(x)-std::log1p(-x);
  const double y=lower ? cppunivariate::special::ibetac_inv(b,a,prob) : cppunivariate::special::ibeta_inv(b,a,prob);
  if(y>=std::numeric_limits<double>::min() && y<=.5) return std::log1p(-y)-std::log(y);
 }
 double lo=-1,hi=1;
 auto value=[&](double t) {return beta_log(-softplus(-t),-softplus(t),a,b,lower);};
 for(int i=0;i<1024 && (lower ? value(lo)>lp : value(lo)<lp);++i) lo*=2;
 for(int i=0;i<1024 && (lower ? value(hi)<lp : value(hi)>lp);++i) hi*=2;
 for(int i=0;i<120;++i) {
  const double mid=lo*.5+hi*.5;
  if(mid==lo || mid==hi) break;
  if(lower ? value(mid)<lp : value(mid)>lp) lo=mid; else hi=mid;
 }
 return lo*.5+hi*.5;
}
} }
