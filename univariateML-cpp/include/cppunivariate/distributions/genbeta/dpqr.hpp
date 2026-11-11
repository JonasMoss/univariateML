#pragma once
#include "mle.hpp"
#include "../../special/beta_tails.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgenbeta(double x,double shape1,double shape2,double shape3,double scale,bool log) {
  const double inf=std::numeric_limits<double>::infinity();
  if(!(shape1>0&&shape2>0&&shape3>0&&scale>0)||!std::isfinite(shape1+shape2+shape3+scale)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  double ll=-inf;
  if(x==0) {
    const double power=std::fma(shape1,shape3,-1.0);
    if(power<0)ll=inf;
    else if(power==0)ll=std::log(shape3)-std::log(scale)-gb2_detail::logbeta(shape1,shape2);
  } else if(x==scale) {
    if(shape2<1)ll=inf;
    else if(shape2==1)ll=std::log(shape1)+std::log(shape3)-std::log(scale);
  } else if(x>0&&x<scale) {
    if(shape3==1) ll=special::affine_beta_log_density_positive(x,shape1,shape2,0,scale);
    else {
      const double z=shape3*special::log_positive_ratio(x,scale);
      const double r=z>-.6931471805599453?std::log(-std::expm1(z)):std::log1p(-std::exp(z));
      const double t=z-r;
      const double error=8*std::numeric_limits<double>::epsilon()*((std::abs(z)+shape3)/(-std::expm1(z))+std::abs(r));
      double kernel=missing_detail::beta_logit_density(t,shape1,shape2,error);
      double complement=r;
      if(std::isnan(kernel) && shape1>=16 && shape2>=16) {
        const auto lr=special::compensated_log::add(special::compensated_log::logarithm(x),special::compensated_log::negative(special::compensated_log::logarithm(scale)));
        const auto coordinate=special::compensated_log::multiply(lr,{shape3,0});
        const auto logcomplement=special::compensated_log::log_one_minus_exp(coordinate);
        const auto logit=special::compensated_log::add(coordinate,special::compensated_log::negative(logcomplement));
        const double coordinate_error=shape3*special::compensated_log::margin(std::abs(std::log(x))+std::abs(std::log(scale)));
        const double resolved_error=(coordinate_error+special::compensated_log::margin(0)*std::min(1.0,std::abs(coordinate.high)))/(-std::expm1(coordinate.high))
            +special::compensated_log::margin(std::abs(logcomplement.high));
        kernel=missing_detail::compensated_beta_logit_density(logit,shape1,shape2,resolved_error);
        complement=logcomplement.high+logcomplement.low;
      }
      ll=std::log(shape3)-std::log(x)-complement+kernel;
    }
  }
  return log?ll:std::exp(ll);
}
} // namespace distributions
} // namespace cppunivariate

#include "../../special/beta_tails.hpp"
namespace cppunivariate { namespace distributions {
inline double pgenbeta(double q,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
 const double lx=q<=0 ? -std::numeric_limits<double>::infinity() : (q>=scale ? 0 : shape3*(q>scale*.5 ? std::log1p((q-scale)/scale) : std::log(q)-std::log(scale)));
 const double ly=cppunivariate::special::fs_log1mexp(lx);
 const double lp=missing_detail::beta_log(lx,ly,shape1,shape2,lower_tail);
 return log_p ? lp : std::exp(lp);
}
inline double qgenbeta(double p,double shape1,double shape2,double shape3,double scale,bool lower_tail,bool log_p) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale)) return std::numeric_limits<double>::quiet_NaN();
 const double lp=missing_detail::logprob(p,log_p);
 const double t=missing_detail::beta_logit_q(lp,shape1,shape2,lower_tail);
 if(t==std::numeric_limits<double>::infinity()) return scale;
 return std::exp(std::log(scale)-missing_detail::softplus(-t)/shape3);
}
template <typename Rng> inline double rgenbeta(Rng &rng,double shape1,double shape2,double shape3,double scale) {
 if(!(shape1>0 && shape2>0 && shape3>0 && scale>0) || !std::isfinite(shape1+shape2+shape3+scale)) return std::numeric_limits<double>::quiet_NaN();
 const double g1=rng.rgamma(shape1,1),g2=rng.rgamma(shape2,1);
 return std::exp(std::log(scale)-missing_detail::softplus(std::log(g2)-std::log(g1))/shape3);
}
} }
