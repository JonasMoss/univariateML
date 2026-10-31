#pragma once
#include "mle.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgb2(double x,double shape1,double shape2,double shape3,
                   double scale,bool log) {
  if(!(shape1>0&&shape2>0&&shape3>0&&scale>0)||
     !std::isfinite(shape1+shape2+shape3)||!std::isfinite(scale)||std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if(x==0) {
    const double power=shape2*shape3-1;
    const double ld=power<0 ? special::kPosInf : (power>0 ? special::kNegInf :
      std::log(shape2)-std::log(scale)-gb2_detail::logbeta(shape3,shape1));
    return log ? ld : std::exp(ld);
  }
  if(x<0||!std::isfinite(x))return log ? -std::numeric_limits<double>::infinity():0;
  const double t=shape2*(std::log(x)-std::log(scale));
  const double ll=std::log(shape2)-std::log(x)-gb2_detail::logbeta(shape3,shape1)
    -shape3*gb2_detail::softplus(-t)-shape1*gb2_detail::softplus(t);
  return log?ll:std::exp(ll);
}
} // namespace distributions
} // namespace cppunivariate

#include "../../special/beta_tails.hpp"
namespace cppunivariate { namespace distributions {
inline double pgb2(double q,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 const double t=q<=0 ? -std::numeric_limits<double>::infinity() : shape2*(std::log(q)-std::log(scale));
 const double lp=missing_detail::beta_log(-missing_detail::softplus(-t),-missing_detail::softplus(t),shape3,shape1,lower_tail);
 return log_p ? lp : std::exp(lp);
}
inline double qgb2(double p,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale)) return std::numeric_limits<double>::quiet_NaN();
 const double lp=missing_detail::logprob(p,log_p);
 return std::exp(std::log(scale)+missing_detail::beta_logit_q(lp,shape3,shape1,lower_tail)/shape2);
}
template <typename Rng> inline double rgb2(Rng &rng,double shape1,double shape2,double shape3,double scale) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale)) return std::numeric_limits<double>::quiet_NaN();
 const double g3=rng.rgamma(shape3,1),g1=rng.rgamma(shape1,1);
 return std::exp(std::log(scale)+(std::log(g3)-std::log(g1))/shape2);
}
} }
