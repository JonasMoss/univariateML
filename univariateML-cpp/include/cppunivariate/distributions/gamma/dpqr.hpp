// Gamma distribution: scalar d/p/q/r.
// Parameters: shape > 0, scale > 0 (README order).
// Support: x >= 0.
//
// Regularized incomplete gamma tails, with logarithmic continuations
// and inversion in log coordinates.

#pragma once

#include "../../special/special.hpp"
#include "../../special/tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dgamma(double x, double shape, double scale, bool log) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape > 0.0 && std::isfinite(shape)) || std::isnan(x)) return special::kNaN;
  if (x < 0.0 || x == special::kPosInf) return log ? special::kNegInf : 0.0;
  if (x == 0.0) {
    if (shape < 1.0) return special::kPosInf;
    if (shape > 1.0) return log ? special::kNegInf : 0.0;
    return log ? log_rate : rate;
  }
  const double ld=special::gamma_log_density_positive(x,shape,scale);
  return log ? ld : std::exp(ld);
}

inline double pgamma(double q, double shape, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape>0 && std::isfinite(shape)) || std::isnan(q)) return special::kNaN;
  if(q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  if(q==special::kPosInf) return special::tail_probability(special::tails_from_log(special::kNegInf,false),lower_tail,log_p);
  if(shape==1) return special::tail_probability(special::tails_from_log_hazard(special::log_positive_ratio(q,scale),false),lower_tail,log_p);
  special::LogTails tails;
  if(special::gamma_laplace_tails(q,shape,scale,tails))
    return special::tail_probability(tails,lower_tail,log_p);
  if(shape>=16) return special::tail_probability(special::gamma_centered_physical_tails(q,shape,scale),lower_tail,log_p);
  const double x=q/scale;
  const double lx=special::log_positive_ratio(q,scale);
  tails=special::gamma_log_tails(lx,shape,std::isfinite(x)&&x>0?x:special::kNaN);
  if(std::isfinite(x) && x>0 && tails.valid) {
    const double residual=std::fma(-x,scale,q)/scale;
    if(residual!=0) {
      const bool lower=tails.lower<=tails.upper;
      const double small=lower?tails.lower:tails.upper;
      if(std::isfinite(small)) {
        const double correction=std::copysign(std::exp(std::log(std::abs(residual))+
            special::gamma_log_density_positive(x,shape,1)-small),residual)*(lower?1:-1);
        // First-order transport has a negligible remainder at this margin.
        if(!(std::abs(correction)<=1e-7)) return special::kNaN;
        tails=special::tails_from_log(small+std::log1p(correction),lower);
      }
    }
  }
  return special::tail_probability(tails,lower_tail,log_p);
}

inline double qgamma(double p, double shape, double scale, bool lower_tail,
                     bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(!tails.valid || !(shape>0 && std::isfinite(shape))) return special::kNaN;
  // Chernoff: both tails beyond a*(1+-epsilon) are at most
  // exp(-a*epsilon^2/(2*(1+epsilon))). Reserve a factor two in that bound.
  const double epsilon=1e-10;
  if(special::gamma_mean_quantile_enclosed(shape,tails)) {
    const double mode=std::exp(std::log(scale)+std::log(shape));
    if(std::isfinite(mode) && mode<std::numeric_limits<double>::max()/(1+2*epsilon)) return mode;
    return special::kNaN;
  }
  return std::exp(special::gamma_log_quantile(p,shape,lower_tail,log_p)+std::log(scale));
}

template <typename Rng>
inline double rgamma(Rng &rng, double shape, double scale) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  return rng.rgamma(shape, scale);
}

}  // namespace distributions
}  // namespace cppunivariate
