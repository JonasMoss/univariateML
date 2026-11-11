// Exponentiated Weibull: scale a, Weibull shape b, CDF exponent c.
#pragma once
#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {
inline bool expweibull_parameters(double scale, double shape, double exponent) {
  return scale>0 && std::isfinite(scale) && shape>0 && std::isfinite(shape) &&
         exponent>0 && std::isfinite(exponent);
}
inline double expweibull_log_ratio(double x, double scale) {
  const double ratio=x/scale;
  return ratio>=0.5 && ratio<=2 ? std::log1p((x-scale)/scale) : std::log(x)-std::log(scale);
}
inline double dexpweibull(double x, double scale, double shape, double exponent, bool log) {
  if (!expweibull_parameters(scale,shape,exponent) || std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  if (x==0) {
    const double power_minus_one=std::fma(shape,exponent,-1.0);
    if (power_minus_one<0) return special::kPosInf;
    if (power_minus_one>0) return log ? special::kNegInf : 0;
    return log ? -std::log(scale) : 1/scale;
  }
  const double z=expweibull_log_ratio(x,scale), lh=shape*z;
  const double h=std::exp(lh);
  double ld;
  if (lh < -36) {
    // log(1-exp(-h)) = log(h)-h/2+O(h^2). The omitted
    // correction is relative O(h) even when exponent*h is large.
    const double leading=std::isfinite(lh) ? exponent*lh : (exponent*shape)*z;
    const double correction=std::exp(std::log(exponent)+lh)*0.5;
    ld=std::log(shape)+std::log(exponent)-std::log(x)+leading-h*0.5-correction;
  } else if (std::isinf(h)) ld=special::kNegInf;
  else ld=std::log(shape)+std::log(exponent)-std::log(x)+lh-h+
          (exponent-1)*special::log1mexp(-h);
  return log ? ld : std::exp(ld);
}
inline double pexpweibull(double q, double scale, double shape, double exponent,
                         bool lower_tail, bool log_p) {
  if (!expweibull_parameters(scale,shape,exponent) || std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double z=expweibull_log_ratio(q,scale), lh=shape*z;
  if (lh < -36) {
    // Retain the logarithm of the product: c*lh can be subnormal even
    // when its log complement is accurately representable. If lh itself
    // overflows, factor it before taking the logarithm.
    const double lH=std::isfinite(lh) ? std::log(exponent)+std::log(-lh) :
                     (std::log(exponent)+std::log(shape))+std::log(-z);
    return special::tail_probability(special::tails_from_log_hazard(lH,true),lower_tail,log_p);
  }
  // H=-log(F)=c*(-log(1-exp(-h))); retain log(H) when H underflows.
  const double ratio=q/scale;
  // A directly evaluated power avoids the extra log/product/exp rounding
  // when the quotient is exactly representable (proved by the FMA).
  const double h=ratio>=.5 && ratio<=2 && std::fma(-ratio,scale,q)==0
      ? std::pow(ratio,shape) : std::exp(lh);
  const double lH=std::log(exponent)+special::log_neg_log1mexp(-h);
  return special::tail_probability(special::tails_from_log_hazard(lH,true),lower_tail,log_p);
}
inline double qexpweibull(double p, double scale, double shape, double exponent,
                         bool lower_tail, bool log_p) {
  if (!expweibull_parameters(scale,shape,exponent)) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return 0;
  if (t.upper==special::kNegInf) return special::kPosInf;
  const double lH=(t.lower < -0.6931471805599453 ? std::log(-t.lower) :
                  special::log_neg_log1mexp(t.upper))-std::log(exponent);
  // log(h) ~ -H for H>36. Divide in log space before exponentiating.
  const double coordinate=lH>std::log(36.0) ? -std::exp(lH-std::log(shape)) :
      (lH < -36 ? std::log(-lH) : special::log_neg_log1mexp(-std::exp(lH)))/shape;
  return std::exp(std::log(scale)+coordinate);
}
template <typename Rng>
inline double rexpweibull(Rng &rng, double scale, double shape, double exponent) {
  return qexpweibull(rng.runif(0.0,1.0),scale,shape,exponent,true,false);
}
} // namespace distributions
} // namespace cppunivariate
