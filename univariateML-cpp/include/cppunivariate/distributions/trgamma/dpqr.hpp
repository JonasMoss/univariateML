// Transformed gamma d/p/q/r, actuar parameterisation:
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)), u = (x/scale)^shape2.
// Quantiles and draws are formed on the log scale, so scale * G^(1/shape2)
// does not overflow when G^(1/shape2) alone would.
#pragma once
#include "../gamma/dpqr.hpp"
#include "../../special/fixed_log.hpp"
#include "../../special/three_word.hpp"

#include "mle.hpp"

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

inline bool trgamma_valid(double shape1, double shape2, double scale) {
  return shape1 > 0.0 && shape2 > 0.0 && scale > 0.0 && std::isfinite(shape1) &&
         std::isfinite(shape2) && std::isfinite(scale);
}

// Shared unit-gamma helpers retain logarithmic arguments below underflow.
inline double trgamma_gamma_logcdf(double logu, double a) {
  return special::gamma_log_tails(logu,a).lower;
}

inline double trgamma_gamma_logquantile(double p, double a, bool lower_tail, bool log_p) {
  return special::gamma_log_quantile(p,a,lower_tail,log_p);
}

constexpr double trgamma_centered_shape_threshold = 1e5;

// Use the original operands to retain the offset from log(shape1).
inline double trgamma_centered_log_argument(double num, double den, double a,
                                            double c, double lu) {
  // log(a) is at most 710. Beyond this cut the subtraction is well
  // conditioned, and no wide product of a possibly huge power is needed.
  if (std::abs(lu)>2048) return lu-std::log(a);
  namespace wide = special::three_word;
  int en=0, ed=0;
  const double mn=std::frexp(num,&en), md=std::frexp(den,&ed);
  wide::Number ratio;
  if (std::abs(en-ed)<=1 && num>den*.5 && num*.5<den) {
    // The subtraction is exact by Sterbenz, including subnormal operands.
    const double difference=std::scalbn(mn,en-ed)-md;
    ratio=wide::log1p(wide::divide({difference},{md}));
  } else {
    ratio=wide::add(wide::logarithm({mn}),wide::negative(wide::logarithm({md})));
    ratio=wide::add(ratio,wide::multiply({double(en-ed)},wide::log_two()));
  }
  const auto z=wide::add(wide::multiply({c},ratio),wide::negative(wide::logarithm({a})));
  if (a>=1e40 && std::abs(z.high)<1e-25)
    return special::fixed_log::centered(num,den,a,c);
  return z.high;
}

// Integrating exp(-u) between exp(-u) and one bounds the leading
// small-Gamma CDF's relative error by u. Preserve a*c when c*log(x/s)
// overflows, and require this remainder to resolve the smaller tail.
inline special::LogTails trgamma_power_tails(double num,double den,double a,double c) {
  const double lr=special::log_positive_ratio(num,den), lu=c*lr;
  if (a>=trgamma_centered_shape_threshold && std::isfinite(lu)) {
    const double z=trgamma_centered_log_argument(num,den,a,c,lu), root=std::sqrt(a);
    return special::tails_from_log(special::gamma_centered::log_tail(root*z,1/root),z<0);
  }
  if(a<1 && std::isfinite(lr)) {
    const double normalizer=special::lgamma1p(a);
    const double lp=std::isfinite(lu)?std::fma(a,lu,-normalizer):(a*c)*lr-normalizer;
    if(lp<0 && lu<std::log(1e-12)+std::min(0.0,std::log(-lp)))
      return special::tails_from_log(lp,true);
  }
  return special::gamma_log_tails(lu,a);
}
inline double trgamma_power_logquantile(double p,double a,double c,bool lower,bool log_p) {
  const auto tails=special::quantile_log_tails(p,lower,log_p);
  if(!tails.valid) return special::kNaN;
  if(a<1 && std::isfinite(tails.lower)) {
    const double numerator=tails.lower+special::lgamma1p(a);
    // The unit-Gamma log quantile lies below -DBL_MAX. Its neglected
    // exp(log u) remainder is smaller than every representable margin.
    if(numerator < -std::numeric_limits<double>::max()*a)
      return numerator/(a*c);
  }
  return trgamma_gamma_logquantile(p,a,lower,log_p)/c;
}
// Invert in standardized coordinates before rounding the physical quantile.
inline double trgamma_large_quantile(double p, double a, double c, double scale,
                                     bool lower, bool log_p, bool inverse) {
  const auto target=special::quantile_log_tails(p,lower,log_p);
  if (!target.valid) return special::kNaN;
  if (target.lower==special::kNegInf) return inverse ? special::kPosInf : 0;
  if (target.upper==special::kNegInf) return inverse ? 0 : special::kPosInf;
  const bool left=target.lower<target.upper;
  const double lp=left ? target.lower : target.upper;
  const double shape=1/std::sqrt(a);
  const double width=std::sqrt(-lp)*std::sqrt(2.0);
  // For z >= 0 the deviance is at least z^2/2, and at least exp(z)/2
  // for z >= 2. For z <= 0 it is at least z^2/(2*(1-z)).
  // These bounds, with the gamma Chernoff inequalities, enclose the root.
  const double upper=std::max(2.0,std::log(-lp)-std::log(a)+std::log(2.0));
  double lo=left ? -(width+2*((-lp)*shape)+1) : -1;
  double hi=left ? 1 : std::min(width+1,(upper+1)/shape);
  // For the opposite side use one standard deviation past the center,
  // checking the bracket directly before relying on that endpoint.
  const double opposite=special::gamma_centered::log_tail(left ? hi : lo,shape);
  if (!(opposite < -std::log(2.0))) return special::kNaN;
  for (int i=0; i<160; ++i) {
    const double mid=lo+(hi-lo)/2;
    if (mid==lo || mid==hi || hi-lo<=1e-13*(1+std::abs(mid))) break;
    const double logtail=special::gamma_centered::log_tail(mid,shape);
    if (std::isnan(logtail)) return special::kNaN;
    const double value=(mid<0)==left ? logtail : special::log1mexp(logtail);
    if (left ? value<lp : value>lp) lo=mid;
    else hi=mid;
  }
  namespace wide = special::three_word;
  const double w=lo+(hi-lo)/2;
  if (hi-lo>1e-13*(1+std::abs(w))) return special::kNaN;
  const auto log_gamma=wide::add(wide::logarithm({a}),wide::multiply({w},{shape}));
  const auto signed_log_gamma=inverse ? wide::negative(log_gamma) : log_gamma;
  const double leading=signed_log_gamma.high/c;
  if (std::abs(leading)>2048) return leading<0 ? 0 : special::kPosInf;
  const auto displacement=wide::divide(signed_log_gamma,{c});
  const auto logx=wide::add(wide::logarithm({scale}),displacement);
  if (logx.high>710 || logx.high<-746) return std::exp(logx.high);
  const int exponent=static_cast<int>(logx.high/wide::log_two().high);
  const auto significand=wide::add(logx,wide::multiply({-double(exponent)},wide::log_two()));
  return std::scalbn(std::exp(significand.high),exponent);
}

inline double trgamma_power_logdensity(double x,double num,double den,double a,double c) {
  const double lr=special::log_positive_ratio(num,den), lu=c*lr;
  if(a<1) {
    if(lu==special::kPosInf) return special::kNegInf;
    const double product=a*c;
    const double logproduct=product>0?std::log(product):std::log(a)+std::log(c);
    const double power=std::isfinite(lu)?a*lu:product*lr;
    return logproduct-std::log(x)+power-std::exp(lu)-special::lgamma1p(a);
  }
  if (!std::isfinite(lu)) return special::kNegInf;
  const double z=a>=trgamma_centered_shape_threshold ? trgamma_centered_log_argument(num,den,a,c,lu) : lu-std::log(a);
  const double penalty=std::abs(z)<0.1 && a>=trgamma_centered_shape_threshold
      ? special::gamma_centered::penalty(std::sqrt(a)*z,1/std::sqrt(a))
      : a*trgamma_detail::expm1_minus(z);
  return std::log(c)+.5*std::log(a)-trgamma_detail::kHalfLog2Pi
      -trgamma_detail::stirling_remainder(a)-penalty-std::log(x);
}

inline double dtrgamma(double x, double shape1, double shape2, double scale, bool log) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (!(x >= 0.0) || !std::isfinite(x)) return log ? -std::numeric_limits<double>::infinity() : 0.0;
  if (x == 0.0) {
    const double difference = std::fma(shape1,shape2,-1.0);
    if (difference < 0.0) return std::numeric_limits<double>::infinity();
    if (difference > 0.0) return log ? -std::numeric_limits<double>::infinity() : 0.0;
    const double ld = std::log(shape2) - std::log(scale) - std::lgamma(shape1);
    return log ? ld : std::exp(ld);
  }
  const double ld=trgamma_power_logdensity(x,x,scale,shape1,shape2);
  return log ? ld : std::exp(ld);
}

inline double ptrgamma(double q, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1,shape2,scale) || std::isnan(q)) return special::kNaN;
  if (shape2==1) return pgamma(q,shape1,scale,lower_tail,log_p);
  const auto tails=q<=0?special::tails_from_log(special::kNegInf,true):
    trgamma_power_tails(q,scale,shape1,shape2);
  return special::tail_probability(tails,lower_tail,log_p);
}

inline double qtrgamma(double p, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (shape1>=trgamma_centered_shape_threshold)
    return trgamma_large_quantile(p,shape1,shape2,scale,lower_tail,log_p,false);
  const double displacement=trgamma_power_logquantile(p,shape1,shape2,lower_tail,log_p);
  return std::exp(std::log(scale)+displacement);
}

template <typename Rng>
inline double rtrgamma(Rng &rng, double shape1, double shape2, double scale) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0.01) return qtrgamma(rng.runif(0.0, 1.0), shape1, shape2, scale, true, false);
  return std::exp(std::log(scale) + std::log(rng.rgamma(shape1, 1.0)) / shape2);
}

}  // namespace distributions
}  // namespace cppunivariate
