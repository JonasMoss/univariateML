// Cauchy distribution: scalar d/p/q/r.
// Parameters: location, scale > 0; support x in R.
//   f(x)    = 1 / (pi * scale * (1 + ((x-location)/scale)^2))
//   F(x)    = 0.5 + atan((x-location)/scale) / pi
//   F^-1(p) = location + scale * tan(pi * (p - 0.5))

#pragma once

#include "../../special/tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double cauchy_log_abs_z(double x,double location,double scale) {
  const double d=x-location;
  const double ld=std::isfinite(d) || !std::isfinite(x) ? std::log(std::fabs(d)) :
      std::log(std::fabs(x/2-location/2))+std::log(2.0);
  return ld-std::log(scale);
}
inline double dcauchy(double x, double location, double scale, bool log) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  const double z=std::fabs(special::scaled_difference(x,location,scale));
  const double lz=cauchy_log_abs_z(x,location,scale);
  const double ld=-std::log(M_PI)-std::log(scale)-(z>1 ? 2*lz+std::log1p(1/z/z) : std::log1p(z*z));
  return log ? ld : std::exp(ld);
}

inline double pcauchy(double q, double location, double scale, bool lower_tail,
                      bool log_p) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  const double z=special::scaled_difference(q,location,scale), a=std::fabs(z);
  const double lp=a>1e16 ? -cauchy_log_abs_z(q,location,scale)-std::log(M_PI) : std::log(std::atan2(1.0,a)/M_PI);
  return special::tail_probability(special::tails_from_log(lp,z<=0),lower_tail,log_p);
}

inline double qcauchy(double p, double location, double scale, bool lower_tail,
                      bool log_p) {
  if (!(scale>0 && std::isfinite(scale) && std::isfinite(location))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double offset=special::probability_minus_half(p,lower_tail,log_p);
  if (std::fabs(offset)<1e-8) return std::fma(scale,std::tan(M_PI*offset),location);

  const double lp=std::min(t.lower,t.upper);
  const double sign=t.lower<t.upper ? -1.0 : 1.0;
  // Around p=1/4 retain cot(pi*p)-1 separately, so a cancelling
  // location does not erase the rounded log-input's true quantile.
  if (std::fabs(lp+std::log(4.0))<0.1) {
    const double shift=log_p ? (p < -std::log(2.0) ?
        0.25*std::expm1((p+1.3862943611198905725)+4.6380936276925991168e-17) :
        -0.75*std::expm1((p+0.287682072451780901)+2.607160616442563869e-17)) :
        std::min(p,1-p)-0.25;
    const double tangent=std::tan(M_PI*shift);
    const double correction=-2*tangent/(1+tangent);
    return std::fma(sign*scale,1.0,location)+sign*scale*correction;
  }
  if (lp < -36) return location+sign*std::exp(std::log(scale)-std::log(M_PI)-lp);
  const double a=lp==-std::log(2.0) ? 0 : 1/std::tan(M_PI*std::exp(lp));
  return std::fma(sign*scale,a,location);
}

template <typename Rng>
inline double rcauchy(Rng &rng, double location, double scale) {
  // Inverse-CDF; matches qcauchy at U ~ U(0, 1).
  const double u = rng.runif(0.0, 1.0);
  return location + scale * std::tan(M_PI * (u - 0.5));
}

}  // namespace distributions
}  // namespace cppunivariate
