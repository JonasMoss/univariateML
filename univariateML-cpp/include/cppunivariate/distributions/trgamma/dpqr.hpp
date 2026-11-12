// Transformed gamma d/p/q/r, actuar parameterisation:
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)), u = (x/scale)^shape2.
// Quantiles and draws are formed on the log scale, so scale * G^(1/shape2)
// does not overflow when G^(1/shape2) alone would.
#pragma once
#include "../gamma/dpqr.hpp"

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

// Integrating exp(-u) between exp(-u) and one bounds the leading
// small-Gamma CDF's relative error by u. Preserve a*c when c*log(x/s)
// overflows, and require this remainder to resolve the smaller tail.
inline special::LogTails trgamma_power_tails(double lr,double a,double c) {
  const double lu=c*lr;
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
inline double trgamma_power_logdensity(double x,double lr,double a,double c) {
  const double lu=c*lr;
  if(a<1) {
    if(lu==special::kPosInf) return special::kNegInf;
    const double product=a*c;
    const double logproduct=product>0?std::log(product):std::log(a)+std::log(c);
    const double power=std::isfinite(lu)?a*lu:product*lr;
    return logproduct-std::log(x)+power-std::exp(lu)-special::lgamma1p(a);
  }
  const double z=lu-std::log(a);
  return std::log(c)+.5*std::log(a)-trgamma_detail::kHalfLog2Pi
      -trgamma_detail::stirling_remainder(a)-a*trgamma_detail::expm1_minus(z)-std::log(x);
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
  const double ld=trgamma_power_logdensity(x,special::log_positive_ratio(x,scale),shape1,shape2);
  return log ? ld : std::exp(ld);
}

inline double ptrgamma(double q, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1,shape2,scale) || std::isnan(q)) return special::kNaN;
  if (shape2==1) return pgamma(q,shape1,scale,lower_tail,log_p);
  const auto tails=q<=0?special::tails_from_log(special::kNegInf,true):
    trgamma_power_tails(special::log_positive_ratio(q,scale),shape1,shape2);
  return special::tail_probability(tails,lower_tail,log_p);
}

inline double qtrgamma(double p, double shape1, double shape2, double scale, bool lower_tail,
                       bool log_p) {
  if (!trgamma_valid(shape1, shape2, scale)) return std::numeric_limits<double>::quiet_NaN();
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
