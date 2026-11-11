// Frechet: F(x)=exp(-((x-mu)/sigma)^(-lambda)), x>mu.
#pragma once
#include "../invweibull/dpqr.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
namespace frechet_dpqr_detail {
inline bool exact_center(double mu,double sigma) {
 // Sterbenz: the subtraction sigma-(-mu) is exact, including subnormals.
 return mu<=0 && -mu>=sigma*0.5 && (-mu)*0.5<=sigma;
}
inline double log_ratio(double x,double mu,double sigma) {
 if(exact_center(mu,sigma)) {
   const double d=x-(mu+sigma);
   if(std::abs(d)<sigma*0.5) return std::log1p(d/sigma);
 }
 const double y=x-mu;
 if(!std::isfinite(y)) {
   // Exact halving puts the distance back in range; retain its residual too.
   if(sigma>=2*std::numeric_limits<double>::min())
     return log_ratio(x*0.5,mu*0.5,sigma*0.5);
   return std::log(x*0.5-mu*0.5)+std::log(2.0)-std::log(sigma);
 }
 // Error-free TwoDiff: y+e is the exact distance, without extended precision.
 const double b=y-x;
 // TwoDiff's virtual subtraction can overflow although the distance is finite.
 if(!std::isfinite(b))
   return sigma>=2*std::numeric_limits<double>::min()
       ? log_ratio(x*0.5,mu*0.5,sigma*0.5)
       : invweibull_dpqr_detail::log_ratio(y,sigma);
 const double e=(x-(y-b))-(mu+b);
 if(y>sigma*0.5 && y*0.5<sigma)
   return std::log1p(((y-sigma)+e)/sigma);
 return invweibull_dpqr_detail::log_ratio(y,sigma)+std::log1p(e/y);
}
}
inline double dfrechet(double x,double lambda,double mu,double sigma,bool log) {
 if(!(lambda>0) || !(sigma>0) || !std::isfinite(lambda) || !std::isfinite(mu) || !std::isfinite(sigma) || std::isnan(x)) return special::kNaN;
 if(x<=mu || std::isinf(x)) return log ? special::kNegInf : 0;
 const double z=frechet_dpqr_detail::log_ratio(x,mu,sigma);
 const double lh=-lambda*z, h=std::exp(lh);
 const double ll=std::isinf(h) ? special::kNegInf : invweibull_dpqr_detail::log_ratio(lambda,sigma)-z+lh-h;
 return log ? ll : std::exp(ll);
}
inline double pfrechet(double q,double lambda,double mu,double sigma,bool lower_tail,bool log_p) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma)) || std::isnan(q)) return special::kNaN;
 if(q<=mu) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
 if(q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
 const double lh=-lambda*frechet_dpqr_detail::log_ratio(q,mu,sigma);
 return special::tail_probability(special::tails_from_log_hazard(lh,true),lower_tail,log_p);
}
inline double qfrechet(double p,double lambda,double mu,double sigma,bool lower_tail,bool log_p) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma))) return special::kNaN;
 const auto t=special::quantile_log_tails(p,lower_tail,log_p);
 if(!t.valid) return special::kNaN;
 if(t.lower==special::kNegInf) return mu;
 if(t.upper==special::kNegInf) return special::kPosInf;
 const double lh=invweibull_dpqr_detail::quantile_log_hazard(p,lower_tail,log_p,t);
 const double v=-lh/lambda;
 if(frechet_dpqr_detail::exact_center(mu,sigma) && std::abs(v)<0.5) {
   if(lh==0) return mu+sigma;
   // Division can underflow although sigma*v is representable. Here the
   // expm1 remainder is far below relative binary64 precision.
   if(std::abs(v)<std::numeric_limits<double>::min())
     return std::fma(-lh,sigma/lambda,mu+sigma);
   return std::fma(sigma,std::expm1(v),mu+sigma);
 }
 const double w=std::exp(v);
 if(std::isnormal(w)) return std::fma(sigma,w,mu);
 // Rescue both overflowing and subnormal multipliers before applying scale.
 const double quarter=std::exp(v*0.25);
 return std::fma(((sigma*quarter)*quarter)*quarter,quarter,mu);
}
template <typename Rng> inline double rfrechet(Rng &rng,double lambda,double mu,double sigma) {
 if(!(std::isfinite(mu) && lambda>0 && std::isfinite(lambda) && sigma>0 && std::isfinite(sigma))) return special::kNaN;
 return qfrechet(rng.runif(0.0,1.0),lambda,mu,sigma,true,false);
}
} }
