#pragma once
#include "wedge.hpp"
#include "../lst/dpqr.hpp"
#include "../sn/dpqr.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace st_detail {
inline bool valid(double xi,double omega,double alpha,double nu) {
 return std::isfinite(xi)&&std::isfinite(omega)&&omega>0&&std::isfinite(alpha)&&std::isfinite(nu)&&nu>0;
}
// The direct log-beta difference loses precision at large degrees of
// freedom. The same gamma-ratio expansion is used by the half-t fitter.
inline double logmode(double nu) {
 return special::log_t_mode(nu);
}
inline double log_t_density(double logabs,double nu) {
 return student_detail::logdensity_log(logabs,nu);
}
inline double standardize(double x,double xi,double omega) {
 double z=static_cast<double>((static_cast<CPPUNIV_EXTENDED_REAL>(x)-xi)/omega);
 if(!std::isfinite(z)&&std::isfinite(x)) {
  double alternative=x/omega-xi/omega;
  if(std::isfinite(alternative))z=alternative;
 }
 return z;
}
inline double logdensity(double x,double xi,double omega,double alpha,double nu) {
 const double z=standardize(x,xi,omega);
 if(!std::isfinite(z))return special::kNegInf;
 const double logh=(alpha==0||z==0)?special::kNegInf:
   std::log(std::abs(alpha))+std::log(std::abs(z))
   -std::log(std::hypot(std::sqrt(nu),z))+0.5*std::log(nu+1);
 const bool negative=(alpha<0)!=(z<0);
 const double lc=student_detail::probability_log(logh,negative,nu+1,true,true);
 return std::log(2.)-std::log(omega)+log_t_density(std::log(std::abs(z)),nu)+lc;
}
}}
namespace cppunivariate { namespace distributions {
inline double dst(double x,double xi,double omega,double alpha,double nu,bool log) {
 if(!st_detail::valid(xi,omega,alpha,nu)||std::isnan(x))return special::kNaN;
 double v=st_detail::logdensity(x,xi,omega,alpha,nu);return log?v:std::exp(v);
}
inline double pst(double q,double xi,double omega,double alpha,double nu,bool lower_tail,bool log_p) {
 if(!st_detail::valid(xi,omega,alpha,nu)||std::isnan(q))return special::kNaN;
 if(alpha==0)return plst(q,xi,omega,nu,lower_tail,log_p);
 double z=st_detail::standardize(q,xi,omega),a=alpha;
 if(a<0){a=-a;z=-z;lower_tail=!lower_tail;}
 if(!std::isfinite(z)){double lp=((z>0)==lower_tail)?0:special::kNegInf;return log_p?lp:std::exp(lp);}
 if(std::abs(z)<1e-10/(1+a)) {
  const double p0=std::atan2(1.,a)/cppunivariate::pi;
  const double slope=std::exp(st_detail::logmode(nu));
  const double lp0=std::log(p0)+std::log1p(z*slope/p0);
  const double result=lower_tail?lp0:special::fs_log1mexp(lp0);
  return log_p?result:std::exp(result);
 }
 bool ok=true;
 const double thin=st_wedge_detail::thin_log(std::abs(z),a,nu,ok);
 if(!ok)return special::kNaN;
 double lp;
 if(z<=0)lp=lower_tail?thin:special::fs_log1mexp(thin);
 else {
  const double wide=std::log(2.)+student_detail::probability_log(std::log(z),true,nu,true,true);
  const double upper=wide+special::fs_log1mexp(thin-wide);
  lp=lower_tail?special::fs_log1mexp(upper):upper;
 }
 return log_p?lp:std::exp(lp);
}
inline double qst(double p,double xi,double omega,double alpha,double nu,bool lower_tail,bool log_p) {
 if(!st_detail::valid(xi,omega,alpha,nu)||std::isnan(p)||(log_p?p>0:(p<0||p>1)))return special::kNaN;
 if(alpha==0)return qlst(p,xi,omega,nu,lower_tail,log_p);
 // At alpha=+/-1 the thin-side probability at the origin is exactly 1/4.
 // Retain the binary64 logarithmic input's offset from the exact log(1/4).
 if((alpha==1&&lower_tail)||(alpha==-1&&!lower_tail)) {
  const double residual=2.3190468138462995584e-17;
  const double delta=log_p ? .25*std::expm1((p+2*std::log(2.))+2*residual) : p-.25;
  if(std::abs(delta)<1e-12) {
   const double z=delta/std::exp(st_detail::logmode(nu));
   return xi+omega*(lower_tail?z:-z);
  }
 }
 double lp=log_p?p:std::log(p);
 if(lp==special::kNegInf)return lower_tail?special::kNegInf:special::kPosInf;
 if(lp==0)return lower_tail?special::kPosInf:special::kNegInf;
 if(lp>-std::log(2.)){lp=special::fs_log1mexp(lp);lower_tail=!lower_tail;}
 double lo=-1,hi=1;
 for(int k=0;k<1024;++k){double q=pst(lo,0,1,alpha,nu,lower_tail,true);if(std::isnan(q))return q;if(lower_tail?q<=lp:q>=lp)break;lo*=2;if(!std::isfinite(lo))return special::kNegInf;}
 for(int k=0;k<1024;++k){double q=pst(hi,0,1,alpha,nu,lower_tail,true);if(std::isnan(q))return q;if(lower_tail?q>=lp:q<=lp)break;hi*=2;if(!std::isfinite(hi))return special::kPosInf;}
 for(int k=0;k<1200;++k){double mid=lo/2+hi/2;if(mid==lo||mid==hi)break;double q=pst(mid,0,1,alpha,nu,lower_tail,true);if(std::isnan(q))return q;if(lower_tail?q<lp:q>lp)lo=mid;else hi=mid;}
 return xi+omega*(lo/2+hi/2);
}
template<class Rng> inline double rst(Rng& rng,double xi,double omega,double alpha,double nu) {
 if(!st_detail::valid(xi,omega,alpha,nu))return special::kNaN;
 const double den=std::hypot(1.,alpha),z=alpha/den*std::abs(rng.rnorm())+rng.rnorm()/den;
 return xi+omega*z/std::sqrt(rng.rgamma(nu/2,2/nu));
}
}}
