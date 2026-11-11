#pragma once
#include "../../special/tails.hpp"
#include "../../special/beta_tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dfpareto(double x,double shape1,double shape2,double shape3,double scale,double min,bool log) {
 const double inf=std::numeric_limits<double>::infinity();
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(min) || !std::isfinite(scale) || !(std::isfinite(shape1)&&std::isfinite(shape2)&&std::isfinite(shape3)) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
 if(x<min || x==inf) return log ? -inf : 0;
 const double c=std::log(shape2)-std::log(scale)-cppunivariate::special::log_beta_positive(shape3,shape1);
 const double power=std::fma(shape2,shape3,-1.0);
 if(x==min) {const double ll=power<0 ? inf : (power>0 ? -inf : c); return log ? ll : std::exp(ll);}
 const double z=special::log_shifted_positive_ratio(x,min,scale),t=shape2*z;
 double centered;
 if(shape2==1 && missing_detail::centered_beta_prime_density(x,min,scale,shape3,shape1,centered))
   return log?centered:std::exp(centered);
 const double coordinate_error=8*std::numeric_limits<double>::epsilon()*(std::abs(t)+shape2);
 double kernel=missing_detail::beta_logit_density(t,shape3,shape1,coordinate_error);
 if(std::isnan(kernel) && shape1>=16 && shape3>=16) {
   const auto lx=special::compensated_log::log_distance(x,min);
   const auto lr=special::compensated_log::add(lx,special::compensated_log::negative(special::compensated_log::logarithm(scale)));
   const auto coordinate=special::compensated_log::multiply(lr,{shape2,0});
   const double error=shape2*special::compensated_log::margin(std::abs(lx.high)+std::abs(std::log(scale)));
   kernel=missing_detail::compensated_beta_logit_density(coordinate,shape3,shape1,error);
 }
 const double ll=std::log(shape2)-special::log_positive_distance(x,min)+kernel;
 return log ? ll : std::exp(ll);
}
} }

#include "../gb2/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double pfpareto(double q,double shape1,double shape2,double shape3,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && shape3>0 && std::isfinite(shape3) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 if(q<=min) return log_p ? (lower_tail ? -std::numeric_limits<double>::infinity() : 0) : (lower_tail ? 0 : 1);
 if(q>=std::numeric_limits<double>::infinity()) return log_p ? (lower_tail ? 0 : -std::numeric_limits<double>::infinity()) : (lower_tail ? 1 : 0);
 const double t=shape2*special::log_shifted_positive_ratio(q,min,scale);
 const double lp=missing_detail::beta_log_from_logit(t,shape3,shape1,lower_tail);
 return log_p?lp:std::exp(lp);
}
inline double qfpareto(double p,double shape1,double shape2,double shape3,double scale,double min,bool lower_tail,bool log_p) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && shape3>0 && std::isfinite(shape3) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 if(std::isnan(p) || (log_p ? p>0 : (p<0 || p>1))) return std::numeric_limits<double>::quiet_NaN();
 if(p==(log_p ? -std::numeric_limits<double>::infinity() : 0)) return lower_tail ? min : std::numeric_limits<double>::infinity();
 if(p==(log_p ? 0 : 1)) return lower_tail ? std::numeric_limits<double>::infinity() : min;
 double t;
 if(!missing_detail::symmetric_beta_logit_quantile(p,shape3,shape1,lower_tail,log_p,t))
   t=missing_detail::beta_logit_q(missing_detail::logprob(p,log_p),shape3,shape1,lower_tail);
 return special::shifted_scaled_power_quantile(min,scale,t,shape2);
}
template <typename Rng> inline double rfpareto(Rng &rng,double shape1,double shape2,double shape3,double scale,double min) {
 if(!(std::isfinite(min) && shape1>0 && std::isfinite(shape1) && shape2>0 && std::isfinite(shape2) && shape3>0 && std::isfinite(shape3) && scale>0 && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 return min+rgb2(rng,shape1,shape2,shape3,scale);
}
} }
