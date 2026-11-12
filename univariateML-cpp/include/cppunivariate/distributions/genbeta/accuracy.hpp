#pragma once
#include "../../core/likelihood_polish.hpp"
#include "../beta/mle.hpp"
#include "../../special/beta_tails.hpp"
namespace cppunivariate {
namespace genbeta_detail {
// Final-value arithmetic only: the search and its conditional statistics
// retain their own coordinates. Preserve the nonlinear beta coordinate before
// rounding when large beta shapes amplify a small log-ratio error.
inline double public_loglik(span x,double a,double b,double power,double scale) {
  accuracy_detail::ScalarSum sum;
  for(double v:x) {
    const auto ratio=special::compensated_log::add(special::compensated_log::logarithm(v),
      special::compensated_log::negative(special::compensated_log::logarithm(scale)));
    const auto coordinate=special::compensated_log::multiply(ratio,{power,0});
    const auto complement=special::compensated_log::log_one_minus_exp(coordinate);
    const auto logit=special::compensated_log::add(coordinate,special::compensated_log::negative(complement));
    const double coordinate_error=power*special::compensated_log::margin(std::abs(std::log(v))+std::abs(std::log(scale)));
    const double error=(coordinate_error+special::compensated_log::margin(0)*std::min(1.,std::abs(coordinate.high)))/(-std::expm1(coordinate.high))
      +special::compensated_log::margin(std::abs(complement.high));
    const double kernel=a>=16&&b>=16
      ?missing_detail::compensated_beta_logit_density(logit,a,b,error)
      :missing_detail::beta_logit_density(logit.high+logit.low,a,b,error);
    sum.add(std::log(power)-std::log(v)-(complement.high+complement.low)+kernel);
  }
  return static_cast<double>(sum.value);
}
inline accuracy_detail::Jet stirling_jet(const accuracy_detail::Jet& x) {
  const auto r=accuracy_detail::reciprocal(x),v=r*r;
  return r*(accuracy_detail::Jet(1./12)-v*(accuracy_detail::Jet(1./360)-v*(accuracy_detail::Jet(1./1260)-v*(accuracy_detail::Jet(1./1680)-v*(accuracy_detail::Jet(1./1188)-v*(accuracy_detail::Jet(691./360360)-v*accuracy_detail::Jet(1./156)))))));
}
// log Gamma(a) + b log(a) - log Gamma(a+b), with the leading
// gamma-limit terms removed analytically. Recurrence shifts are exact;
// the omitted Stirling term at a>=32 is below 8e-25.
inline accuracy_detail::Jet gamma_ratio_correction(accuracy_detail::Jet a,const accuracy_detail::Jet& b) {
  accuracy_detail::JetSum correction;
  for(int j=0;j<32&&a.v<32;++j) {
    correction.add(accuracy_detail::log1pmx(b/a)-b*accuracy_detail::log1pmx(accuracy_detail::reciprocal(a)));
    a=a+accuracy_detail::Jet(1);
  }
  return correction.value-(a+b-accuracy_detail::Jet(.5))*accuracy_detail::log1pmx(b/a)-b*(b-accuracy_detail::Jet(.5))/a+stirling_jet(a)-stirling_jet(a+b);
}
inline accuracy_detail::Jet log_relative_exponential(const accuracy_detail::Jet& z) {
  if(z.v<.1) {
    const auto v=z*z;
    return -z/accuracy_detail::Jet(2)+v*(accuracy_detail::Jet(1./24)-v*(accuracy_detail::Jet(1./2880)-v*(accuracy_detail::Jet(1./181440)-v*(accuracy_detail::Jet(1./9676800)-v*accuracy_detail::Jet(1./479001600)))));
  }
  const ExtendedReal q=std::exp(-z.v),d=-std::expm1(-z.v);
  return accuracy_detail::compose(z,std::log(d)-std::log(z.v),q/d-1/z.v,1/(z.v*z.v)-q/(d*d));
}
}
}
