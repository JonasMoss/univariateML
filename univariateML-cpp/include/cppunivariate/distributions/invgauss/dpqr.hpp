// Inverse Gaussian distribution (actuar R-level: mean + shape;
// dispersion = 1/shape).
//
//   f(x)   = (shape/(2*pi*x^3))^(1/2) * exp(-shape*(x-mean)^2 / (2*x*mean^2))
//
// Log densities, normal Mills-ratio tails, and bracketed log-tail inversion.
// r matches actuar's rinvgauss: one rnorm + one runif call per sample.

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

namespace detail {
constexpr double kInvGaussYphiLarge = 5e5;
} // namespace detail

inline double dinvgauss(double x, double mean, double shape, bool log) {
  if (!(mean>0 && shape>0 && std::isfinite(mean) && std::isfinite(shape)) || std::isnan(x)) return special::kNaN;
  if (x<=0 || x==special::kPosInf) return log ? special::kNegInf : 0.0;
  const double lr=0.5*(std::log(shape)-std::log(x));
  const double z=x==mean ? 0.0 : std::copysign(std::exp(std::log(std::abs(x-mean))-std::log(mean)+lr),x-mean);
  const double ld=0.5*std::log(shape)-0.91893853320467274178-1.5*std::log(x)-(0.5*z)*z;
  return log ? ld : std::exp(ld);
}

inline double pinvgauss(double q, double mean, double shape, bool lower_tail,
                        bool log_p) {
  if (!(mean>0 && shape>0 && std::isfinite(mean) && std::isfinite(shape)) || std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if (q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  const double lr=0.5*(std::log(shape)-std::log(q)), root=std::exp(lr);
  const double a=q==mean ? 0.0 : std::copysign(std::exp(std::log(std::abs(q-mean))-std::log(mean)+lr),q-mean);
  const double b=std::exp(special::log_sum_exp(std::log(q),std::log(mean))-std::log(mean)+lr);
  const double lphi=-(0.5*a)*a-0.91893853320467274178;
  const double lp=special::log_sum_exp(special::normal_log_upper(-a),lphi+special::normal_log_mills(b));
  if (lp < -0.6931471805599453)
    return special::tail_probability(special::tails_from_log(lp,true),lower_tail,log_p);
  double difference;
  if (lr+std::log(2.0) < -14.0+std::log1p(std::abs(a))) {
    // Midpoint derivative of M; the relative correction is O((b-a)^2).
    difference=std::log(2.0)+lr+special::normal_log_mills_slope(a+root);
  } else {
    difference=special::log_diff_exp(special::normal_log_mills(a),special::normal_log_mills(b));
  }
  const double lq=lphi+difference;
  return special::tail_probability(special::tails_from_log(lq,false),lower_tail,log_p);
}

inline double qinvgauss(double p, double mean, double shape, bool lower_tail,
                        bool log_p) {
  if (!(mean>0 && shape>0 && std::isfinite(mean) && std::isfinite(shape))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return 0.0;
  if (t.upper==special::kNegInf) return special::kPosInf;
  const bool left=t.lower<t.upper;
  const double target=left?t.lower:t.upper;
  double lo=-746.0, hi=710.0;
  for (int k=0;k<160;++k) {
    const double mid=lo+(hi-lo)/2;
    if (mid==lo || mid==hi) break;
    const double v=pinvgauss(std::exp(mid),mean,shape,left,true);
    if (left ? v<target : v>target) lo=mid; else hi=mid;
  }
  return std::exp(lo+(hi-lo)/2);
}

template <typename Rng>
inline double rinvgauss(Rng &rng, double mean, double shape) {
  // Bit-for-bit port of actuar's rinvgauss: phi = 1/shape (dispersion form).
  const double phi = 1.0 / shape;
  const double z = rng.rnorm();
  const double yphi = z * z * phi * mean;
  double x;
  if (yphi > detail::kInvGaussYphiLarge) {
    x = 1.0 / yphi;
  } else {
    x = 1.0 + yphi / 2.0 * (1.0 - std::sqrt(1.0 + 4.0 / yphi));
  }
  const double u = rng.runif(0.0, 1.0);
  return mean * ((u <= 1.0 / (1.0 + x)) ? x : 1.0 / x);
}

} // namespace distributions
} // namespace cppunivariate
