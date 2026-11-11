#pragma once
#include "special.hpp"
#include "tails.hpp"
#include "compensated_log.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace missing_detail {
// Shared beta log-coordinate bridge. Contract and numerical limitations:
// dev/research/evaluations/2026-10-31-step7-routines.md (repository root).
inline double softplus(double x) { return std::fmax(x,0)+std::log1p(std::exp(-std::abs(x))); }
inline double logprob(double p,bool log_p) {
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 return log_p ? p : std::log(p);
}
// Evaluate the favourable incomplete-beta continued fraction. The prefactor
// remains in log coordinates even when exp(lx) underflows to zero.
inline double beta_small(double lx,double ly,double a,double b,double p) {
 const double x=std::exp(lx);
 if(p>=1e-290 && lx>std::log(std::numeric_limits<double>::min())) return std::log(p);
 return a*lx+b*ly-cppunivariate::special::log_beta_positive(a,b)
   +std::log(cppunivariate::special::fs_beta_fraction(a,b,x)/a);
}
inline double beta_log(double lx,double ly,double a,double b,bool lower) {
 // Exact unit/two-shape antiderivatives bypass backend normalizers that
 // lose relative tail accuracy when long-double promotion is unavailable.
 if(b==1) {
  const double log_hazard=std::log(a)+(lx<-.6931471805599453?std::log(-lx):cppunivariate::special::log_neg_log1mexp(ly));
  const auto tails=cppunivariate::special::tails_from_log_hazard(log_hazard,true);
  return lower?tails.lower:tails.upper;
 }
 if(a==1) return beta_log(ly,lx,b,a,!lower);
 if(b==2) {
  const double la=std::log(a), maximum=std::exp(ly+std::max(0.0,la));
  double log_hazard;
  // The first omitted relative term is <= 2m/(3(1-m)).
  if(maximum<.5 && (2.0/3)*maximum/(1-maximum)<=1e-12)
    log_hazard=(la+std::log1p(a))+2*ly-.69314718055994530942;
  else {
    const double w=std::exp(ly);
    const double aw=w>=std::numeric_limits<double>::min()?a*w:std::exp(la+ly);
    const double first=w<=.5?cppunivariate::special::scaled_positive_deviance(1,-w):-lx-w;
    const double second=cppunivariate::special::scaled_positive_deviance(1,aw);
    log_hazard=cppunivariate::special::log_sum_exp(la+std::log(first),std::log(second));
  }
  const auto tails=cppunivariate::special::tails_from_log_hazard(log_hazard,true);
  return lower?tails.lower:tails.upper;
 }
 if(a==2) return beta_log(ly,lx,b,a,!lower);
 const double p=lx<-.6931471805599453 ? cppunivariate::special::ibeta(a,b,std::exp(lx)) : cppunivariate::special::ibetac(b,a,std::exp(ly));
 const double q=ly<-.6931471805599453 ? cppunivariate::special::ibeta(b,a,std::exp(ly)) : cppunivariate::special::ibetac(a,b,std::exp(lx));
 if(p<=q) {
  const double lp=beta_small(lx,ly,a,b,p);
  return lower ? lp : cppunivariate::special::fs_log1mexp(lp);
 }
 const double lq=beta_small(ly,lx,b,a,q);
 return lower ? cppunivariate::special::fs_log1mexp(lq) : lq;
}
// Bridge from a resolved log odds. In a concentrating symmetric model,
// rounding the two beta coordinates can exceed the forward contract.
inline double beta_log_from_logit(double t,double a,double b,bool lower) {
 if(a==b) {
  if(t==0) return -0.6931471805599453;
  const double coordinate_error=8*std::numeric_limits<double>::epsilon();
  if(std::sqrt(a)*coordinate_error>2.5e-11 && std::abs(t)<2*coordinate_error/1e-10)
    return cppunivariate::special::kNaN;
 }
 return beta_log(-softplus(-t),-softplus(t),a,b,lower);
}
inline double symmetric_beta_logit_density(double t,double a) {
 const double log_slope=cppunivariate::special::log_gamma_ratio(a,0.5,0)-1.26551212348464539649;
 const double h=std::sinh(t*0.25), u=2*h*h;
 double penalty;
 if(std::isfinite(h) && std::log1p(u)==u)
   penalty=((a*h)*h)*4;
 else {
   const double log_cosh=std::abs(t)>4 ? std::abs(t)*0.5-0.6931471805599453+std::log1p(std::exp(-std::abs(t))) : std::log1p(u);
   penalty=(a*log_cosh)*2;
 }
 return log_slope-penalty;
}
// Stirling's normalizer minus two nonnegative Bernoulli deviances.
// u and v are relative displacements of the two beta coordinates.
inline double centered_beta_logit_density(double a,double b,double u,double v) {
 const double large=std::max(a,b),small=std::min(a,b);
 const double logsum=std::log(large)+std::log1p(small/large);
 const double constant=.5*(std::log(a)+std::log(b)-logsum)-.91893853320467274178
   +special::density_stirling_remainder(a+b)-special::density_stirling_remainder(a)-special::density_stirling_remainder(b);
 return constant-special::scaled_positive_deviance(a,u)-special::scaled_positive_deviance(b,v);
}
inline double beta_logit_density(double t,double a,double b,double coordinate_error) {
 if(a==b) return symmetric_beta_logit_density(t,a);
 if(a>=16 && b>=16 && std::isfinite(t)) {
  const double mode=special::log_positive_ratio(a,b),h=std::expm1(t-mode);
  const double large=std::max(a,b),na=a/large,nb=b/large,denom=na*(1+h)+nb;
  const double u=nb*h/denom,v=-na*h/denom;
  if(std::abs(u)<=.5 && std::abs(v)<=.5) {
   const double value=centered_beta_logit_density(a,b,u,v);
   const double ratio=a/b;
   const double mode_error=8*std::numeric_limits<double>::epsilon()*(std::abs(mode)+(ratio>=.5 && ratio<=2?0:1));
   const double error=coordinate_error+mode_error;
   const double margin=std::abs(a*u)*error+.5*(a+b)*error*error;
   if(margin>1e-12*std::max(1.0,std::abs(value))) return special::kNaN;
   return value;
  }
 }
 return -special::log_beta_positive(a,b)-a*special::softplus(-t)-b*special::softplus(t);
}
// Recompute the center before rounding a concentrating log-odds coordinate.
inline double compensated_beta_logit_density(special::compensated_log::Number t,double a,double b,double input_error) {
 const auto mode=special::compensated_log::add(special::compensated_log::logarithm(a),
   special::compensated_log::negative(special::compensated_log::logarithm(b)));
 const auto difference=special::compensated_log::add(t,special::compensated_log::negative(mode));
 const double d=difference.high+difference.low,h=std::expm1(d);
 const double large=std::max(a,b),na=a/large,nb=b/large,denom=na*(1+h)+nb;
 const double u=nb*h/denom,v=-na*h/denom;
 if(!(std::abs(u)<=.5 && std::abs(v)<=.5))
   return beta_logit_density(t.high+t.low,a,b,input_error);
 const double value=centered_beta_logit_density(a,b,u,v);
 const double error=input_error+special::compensated_log::margin(std::abs(std::log(a))+std::abs(std::log(b)))
     +8*std::numeric_limits<double>::epsilon()*std::abs(d);
 const double margin=std::abs(a*u)*error+.5*(a+b)*error*error;
 return margin<=1e-11*std::max(1.0,std::abs(value))?value:special::kNaN;
}
// Retain q*(x-location)-p*scale before either ratio or beta coordinate
// rounds. Power-of-two scaling and a short product expansion preserve it.
inline bool centered_beta_prime_density(double x,double location,double scale,double a,double b,double &answer) {
 if(a<16 || b<16) return false;
 int es=0,ex=0;
 std::frexp(std::max(a,b),&es);
 std::frexp(std::max(std::abs(x),std::max(std::abs(location),scale)),&ex);
 const double na=std::scalbn(a,-es),nb=std::scalbn(b,-es);
 const double nx=std::scalbn(x,-ex),nl=std::scalbn(location,-ex),ns=std::scalbn(scale,-ex);
 const std::array<double,3> left{{nb,-nb,-na}},right{{nx,nl,ns}};
 std::array<double,6> expansion{};std::size_t length=0;
 for(std::size_t k=0;k<3;++k) {
  const double product=left[k]*right[k];
  const std::array<double,2> pieces{{product,std::fma(left[k],right[k],-product)}};
  for(double piece:pieces) {
   double carry=piece;
   for(std::size_t j=0;j<length;++j) {
    const double sum=carry+expansion[j];
    expansion[j]=special::addition_residual(carry,expansion[j],sum);carry=sum;
   }
   expansion[length++]=carry;
  }
 }
 double delta=0;for(std::size_t j=0;j<length;++j) delta+=expansion[j];
 const double denom=(nx-nl)+ns,u=delta/(na*denom),v=-delta/(nb*denom);
 if(std::abs(u)>.5 || std::abs(v)>.5 || std::isnan(u) || std::isnan(v)) return false;
 answer=centered_beta_logit_density(a,b,u,v)-special::log_positive_distance(x,location);
 return true;
}
// Symmetric beta density in log odds is g(0)*cosh(t/2)^(-2a).
// log cosh(t/2)<=t^2/8 bounds the relative linear-inverse error.
inline bool symmetric_beta_logit_quantile(double input,double a,double b,
                                          bool lower,bool log_p,double &answer) {
 if(a!=b) return false;
 const double offset=cppunivariate::special::probability_minus_half(input,lower,log_p);
 if(offset==0) {answer=0;return true;}
 const double log_slope=cppunivariate::special::log_gamma_ratio(a,0.5,0)
     -1.26551212348464539649; // log(2*sqrt(pi))
 const double d=std::copysign(std::exp(std::log(std::abs(offset))-log_slope),offset);
 if(std::isfinite(d) && (a*d)*d<=1e-12) {answer=d;return true;}
 return false;
}
// Invert in log odds, preserving both beta coordinates beyond double's
// probability range. Monotone bisection also avoids rounded-to-one inverses.
inline double beta_logit_q(double lp,double a,double b,bool lower) {
 if(std::isnan(lp)) return lp;
 if(lp>-.6931471805599453) return beta_logit_q(cppunivariate::special::fs_log1mexp(lp),a,b,!lower);
 if(lp==-std::numeric_limits<double>::infinity()) return lower ? lp : -lp;
 double lo=-1,hi=1;
 auto value=[&](double t) {return beta_log(-softplus(-t),-softplus(t),a,b,lower);};
 for(int i=0;i<1024 && (lower ? value(lo)>lp : value(lo)<lp);++i) lo*=2;
 for(int i=0;i<1024 && (lower ? value(hi)<lp : value(hi)>lp);++i) hi*=2;
 const double vl=value(lo), vh=value(hi);
 if (!std::isfinite(lo) || !std::isfinite(hi) || std::isnan(vl) || std::isnan(vh) ||
     (lower ? (vl>lp || vh<lp) : (vl<lp || vh>lp)))
   return std::numeric_limits<double>::quiet_NaN();
 for(int i=0;i<120;++i) {
  const double mid=lo*.5+hi*.5;
  if(mid==lo || mid==hi) return mid;
  const double v=value(mid);
  if (std::isnan(v)) return v;
  if (mid==0 && v==lp) return mid;
  if(lower ? v<lp : v>lp) lo=mid; else hi=mid;
 }
 // Never return an unverified capped bisection.
 return std::numeric_limits<double>::quiet_NaN();
}
} }
