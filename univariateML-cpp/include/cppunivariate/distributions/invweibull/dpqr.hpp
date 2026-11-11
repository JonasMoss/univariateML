// Inverse Weibull: shape > 0, scale > 0; F(x) = exp(-(scale/x)^shape).
#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {
namespace invweibull_dpqr_detail {
inline double log_ratio(double x,double scale) {
  if(x>scale*0.5 && x*0.5<scale)
    return std::log1p((x-scale)/scale);
  const double ratio=x/scale;
  // A normal quotient avoids cancellation of large logarithms even at
  // ratios outside the centered interval. Logs retain the full range.
  return std::isnormal(ratio) ? std::log(ratio) : std::log(x)-std::log(scale);
}
inline double quantile_log_hazard(double p,bool lower,bool log_p,
                                  const special::LogTails& t) {
  // At F=exp(-1), log(F) rounds to -1 for several different inputs.
  // Retain the residuals of the exact probability center on each input scale.
  if(log_p && lower) return std::log(-p);
  double offset;
  if(!log_p) {
    const double f=lower ? p : 1-p;
    if(std::abs(f-0.36787944117144233)<0.04) {
      offset=((f-0.36787944117144233)+1.2428753672788363168e-17)
          /0.36787944117144233;
      return std::log1p(-std::log1p(offset));
    }
  } else if(std::abs(p+0.4586751453870819)<0.125) {
    offset=-std::expm1((p+0.4586751453870819)+1.8138570925507324931e-17)
        *1.7182818284590452354;
    return std::log1p(-std::log1p(offset));
  }
  return t.upper < -0.6931471805599453
      ? special::log_neg_log1mexp(t.upper) : std::log(-t.lower);
}
}

inline double dinvweibull(double x, double shape, double scale, bool log) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<=0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double lh=-shape*invweibull_dpqr_detail::log_ratio(x,scale);
  const double h=std::exp(lh);
  const double ld=std::isinf(h) ? special::kNegInf : invweibull_dpqr_detail::log_ratio(shape,x)+lh-h;
  return log ? ld : std::exp(ld);
}

inline double pinvweibull(double q, double shape, double scale,
                          bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh=-shape*invweibull_dpqr_detail::log_ratio(q,scale);
  const auto t=lh < -36 ? special::tails_from_log(lh,false) : special::tails_from_log(-std::exp(lh),true);
  return special::tail_probability(t,lower_tail,log_p);
}

inline double qinvweibull(double p, double shape, double scale,
                          bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=invweibull_dpqr_detail::quantile_log_hazard(p,lower_tail,log_p,t);
  const double v=-lh/shape;
  // Apply scale by multiplication: no cancellation of log(scale) and v.
  const double w=std::exp(v);
  if(std::isnormal(w)) return scale*w;
  // A subnormal multiplier can lose bits that scaling would rescue.
  const double quarter=std::exp(v*0.25);
  return (((scale*quarter)*quarter)*quarter)*quarter;
}

template <typename Rng>
inline double rinvweibull(Rng &rng, double shape, double scale) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  const double u = rng.runif(0.0, 1.0);
  return qinvweibull(u,shape,scale,true,false);
}

} // namespace distributions
} // namespace cppunivariate
