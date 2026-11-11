#pragma once
#include "mle.hpp"
#include "../../special/beta_tails.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgb2(double x,double shape1,double shape2,double shape3,
                   double scale,bool log) {
  if(!(shape1>0&&shape2>0&&shape3>0&&scale>0)||
     !(std::isfinite(shape1)&&std::isfinite(shape2)&&std::isfinite(shape3))||!std::isfinite(scale)||std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if(x==0) {
    const double power=std::fma(shape2,shape3,-1.0);
    const double ld=power<0 ? special::kPosInf : (power>0 ? special::kNegInf :
      std::log(shape2)-std::log(scale)-gb2_detail::logbeta(shape3,shape1));
    return log ? ld : std::exp(ld);
  }
  if(x<0||!std::isfinite(x))return log ? -std::numeric_limits<double>::infinity():0;
  const double t=shape2*special::log_positive_ratio(x,scale);
  double centered;
  if(shape2==1 && missing_detail::centered_beta_prime_density(x,0,scale,shape3,shape1,centered))
    return log?centered:std::exp(centered);
  const double coordinate_error=8*std::numeric_limits<double>::epsilon()*(std::abs(t)+shape2);
  double kernel=missing_detail::beta_logit_density(t,shape3,shape1,coordinate_error);
  if(std::isnan(kernel) && shape1>=16 && shape3>=16) {
    const auto lr=special::compensated_log::add(special::compensated_log::logarithm(x),special::compensated_log::negative(special::compensated_log::logarithm(scale)));
    const auto coordinate=special::compensated_log::multiply(lr,{shape2,0});
    const double error=shape2*special::compensated_log::margin(std::abs(std::log(x))+std::abs(std::log(scale)));
    kernel=missing_detail::compensated_beta_logit_density(coordinate,shape3,shape1,error);
  }
  const double ll=std::log(shape2)-std::log(x)+kernel;
  return log?ll:std::exp(ll);
}
} // namespace distributions
} // namespace cppunivariate

#include "../../special/beta_tails.hpp"
namespace cppunivariate { namespace distributions {
inline double pgb2(double q,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !(std::isfinite(shape1)&&std::isfinite(shape2)&&std::isfinite(shape3)&&std::isfinite(scale)) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 const double t=q<=0 ? -std::numeric_limits<double>::infinity() : shape2*special::log_positive_ratio(q,scale);
 const double lp=missing_detail::beta_log_from_logit(t,shape3,shape1,lower_tail);
 return log_p ? lp : std::exp(lp);
}
inline double qgb2(double p,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !(std::isfinite(shape1)&&std::isfinite(shape2)&&std::isfinite(shape3)&&std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
 const double lp=missing_detail::logprob(p,log_p);
 double t;
 if(!missing_detail::symmetric_beta_logit_quantile(p,shape3,shape1,lower_tail,log_p,t))
   t=missing_detail::beta_logit_q(lp,shape3,shape1,lower_tail);
 return std::exp(std::log(scale)+t/shape2);
}
template <typename Rng> inline double rgb2(Rng &rng,double shape1,double shape2,double shape3,double scale) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale)) return std::numeric_limits<double>::quiet_NaN();
 const double g3=rng.rgamma(shape3,1),g1=rng.rgamma(shape1,1);
 return std::exp(std::log(scale)+(std::log(g3)-std::log(g1))/shape2);
}
} }
