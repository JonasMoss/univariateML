#pragma once
#include "mle.hpp"
namespace cppunivariate {
namespace distributions {
inline double dgenbeta(double x,double shape1,double shape2,double shape3,double scale,bool log) {
  const double inf=std::numeric_limits<double>::infinity();
  if(!(shape1>0&&shape2>0&&shape3>0&&scale>0)||!std::isfinite(shape1+shape2+shape3+scale)||std::isnan(x))return std::numeric_limits<double>::quiet_NaN();
  double ll=-inf;
  if(x==0) {
    if(shape1*shape3<1)ll=inf;
    else if(shape1*shape3==1)ll=std::log(shape3)-std::log(scale)-gb2_detail::logbeta(shape1,shape2);
  } else if(x==scale) {
    if(shape2<1)ll=inf;
    else if(shape2==1)ll=std::log(shape1)+std::log(shape3)-std::log(scale);
  } else if(x>0&&x<scale) {
    const double z=shape3*(x>scale*.5?std::log1p((x-scale)/scale):std::log(x)-std::log(scale));
    const double r=z>-.6931471805599453?std::log(-std::expm1(z)):std::log1p(-std::exp(z));
    ll=std::log(shape3)-std::log(x)+shape1*z+(shape2-1)*r-gb2_detail::logbeta(shape1,shape2);
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
