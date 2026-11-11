// Logit-normal law: logit(X) is normal.
#pragma once
#include "../../core/precision.hpp"
#include "../../special/special.hpp"
#include "../../special/tails.hpp"
#include "../../special/compensated_log.hpp"
#include <cmath>
namespace cppunivariate {
namespace distributions {
inline double logitnorm_logtail(double z, bool lower) {
  return special::fs_normal_p(z,lower,true);
}

inline bool logitnorm_valid(double mu,double sigma) {
  return std::isfinite(mu) && std::isfinite(sigma) && sigma>0;
}
struct LogitnormCoordinate {double value,error;};
inline LogitnormCoordinate logitnorm_coordinate(double x,double mu,double sigma,bool precise) {
 const double l=std::log(x),r=std::log1p(-x);
 const double eps=std::numeric_limits<double>::epsilon();
 if(x==.5) {const double z=-mu/sigma;return {z,8*eps*std::abs(z)};}
 if(!precise) {
   const double z=(l-r-mu)/sigma;
   return {z,8*eps*(1+std::abs(l)+std::abs(r)+std::abs(mu))/sigma+8*eps*std::abs(z)};
 }
 const auto lx=special::compensated_log::logarithm(x);
 const auto ly=special::compensated_log::logarithm(special::compensated_log::sum(1,-x));
 const auto w=special::compensated_log::add(special::compensated_log::add(lx,special::compensated_log::negative(ly)),{-mu,0});
 const double error=special::compensated_log::margin(std::abs(l)+std::abs(r)+std::abs(mu));
 const double value=w.high+w.low,z=value/sigma;
 if(std::abs(value)<=2*error && error/sigma>1e-12)return {special::kNaN,special::kPosInf};
 return {z,error/sigma+8*eps*std::abs(z)};
}
inline double dlogitnorm(double x,double mu,double sigma,bool log=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(x)) return special::kNaN;
  if (x<=0 || x>=1) return log ? special::kNegInf : 0;
  const double l=std::log(x),r=std::log1p(-x),constant=-std::log(sigma)-.91893853320467274178-l-r;
  auto coordinate=logitnorm_coordinate(x,mu,sigma,false);
  double v=constant-(.5*coordinate.value)*coordinate.value;
  if(coordinate.error*(std::abs(coordinate.value)+coordinate.error)>1e-11*std::max(1.0,std::abs(v))) {
    coordinate=logitnorm_coordinate(x,mu,sigma,true);
    v=constant-(.5*coordinate.value)*coordinate.value;
    if(coordinate.error*(std::abs(coordinate.value)+coordinate.error)>1e-11*std::max(1.0,std::abs(v)))return special::kNaN;
  }
  return log?v:std::exp(v);
}
inline double logitnorm_probability_margin(LogitnormCoordinate coordinate,double selected) {
 double margin=coordinate.error*(1+std::abs(coordinate.value)+coordinate.error);
 if(selected>-.6931471805599453 && coordinate.error<std::abs(coordinate.value)) {
   const double radius=std::abs(coordinate.value)-coordinate.error;
   const double other=special::normal_log_upper(radius);
   if(other<-745)return 0;
   margin*=std::exp(other-special::log1mexp(other));
 }
 return margin;
}
inline double plogitnorm(double q,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(q)) return special::kNaN;
  double v;
  if (q<=0) v=lower_tail ? special::kNegInf : 0;
  else if (q>=1) v=lower_tail ? 0 : special::kNegInf;
  else {
    auto coordinate=logitnorm_coordinate(q,mu,sigma,false);
    v=logitnorm_logtail(coordinate.value,lower_tail);
    if(logitnorm_probability_margin(coordinate,v)>5e-11*(log_p?std::max(1.0,std::abs(v)):1)) {
      coordinate=logitnorm_coordinate(q,mu,sigma,true);
      v=logitnorm_logtail(coordinate.value,lower_tail);
      if(!log_p && std::abs(coordinate.value)>40+coordinate.error)
        return (coordinate.value>0)==lower_tail?1:0;
      if(logitnorm_probability_margin(coordinate,v)>5e-11*(log_p?std::max(1.0,std::abs(v)):1))return special::kNaN;
    }
  }
  return log_p ? v : std::exp(v);
}
inline double qlogitnorm(double p,double mu,double sigma,bool lower_tail=true,bool log_p=false) {
  if (!logitnorm_valid(mu,sigma) || std::isnan(p) || (log_p ? p>0 : p<0 || p>1)) return special::kNaN;
  if (p==(log_p ? special::kNegInf : 0)) return lower_tail ? 0 : 1;
  if (p==(log_p ? 0 : 1)) return lower_tail ? 1 : 0;
  const double z=std::fma(sigma,special::normal_quantile(p,lower_tail,log_p),mu);
  return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
}
template <typename Rng>
inline double rlogitnorm(Rng &rng,double mu,double sigma) {
  if (!logitnorm_valid(mu,sigma)) return special::kNaN;
  const double z=mu+sigma*rng.rnorm();
  return z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
}
}
}
