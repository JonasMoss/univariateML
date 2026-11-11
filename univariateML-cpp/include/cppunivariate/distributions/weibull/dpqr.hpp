// Weibull distribution: scalar d/p/q/r.
// Parameters: shape > 0, scale > 0; support x >= 0.
//   f(x)    = (shape/scale) * (x/scale)^(shape-1) * exp(-(x/scale)^shape)
//   F(x)    = 1 - exp(-(x/scale)^shape)
//   F^-1(p) = scale * (-log1p(-p))^(1/shape)

#pragma once

#include "../../special/tails.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dweibull(double x, double shape, double scale, bool log) {
  if (!(shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  if (x==0) return shape<1 ? special::kPosInf : (shape>1 ? (log ? special::kNegInf : 0) : (log ? -std::log(scale) : 1/scale));
  const double lh=shape*special::log_positive_ratio(x,scale);
  const double h=std::exp(lh);
  const double ld=std::isinf(h) ? special::kNegInf : std::log(shape)-std::log(x)+lh-h;
  return log ? ld : std::exp(ld);
}

inline double pweibull(double q, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh = shape*special::log_positive_ratio(q,scale);
  const auto t = lh < -36 ? special::tails_from_log(lh,true) : special::tails_from_log(-std::exp(lh),false);
  return special::tail_probability(t,lower_tail,log_p);
}

namespace weibull_detail {
// Around unit hazard, retain the low parts of 1-exp(-1), exp(-1),
// and log(1-exp(-1)) before dividing the log hazard by a small shape.
inline double quantile_log_hazard(double p,bool lower,bool log_p,
                                  const special::LogTails &tails) {
  if (!log_p && lower && p>=0.5 && p<=0.75) {
    const double delta=(p-0.63212055882855766598)-1.2428753672788363e-17;
    return std::log1p(-std::log1p(-2.71828182845904523536*delta));
  }
  if (!log_p && !lower && p>=0.25 && p<=0.5) {
    const double delta=(p-0.36787944117144233402)+1.2428753672788363e-17;
    return std::log1p(-std::log1p(2.71828182845904523536*delta));
  }
  if (log_p && lower && p>=-0.75 && p<=-0.25) {
    const double delta=(p-(-0.45867514538708187288))+1.8138570925507326e-17;
    return std::log1p(-std::log1p(-1.71828182845904523536*std::expm1(delta)));
  }
  return tails.lower < -0.6931471805599453
      ? special::log_neg_log1mexp(tails.lower) : std::log(-tails.upper);
}
}

inline double qweibull(double p, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=weibull_detail::quantile_log_hazard(p,lower_tail,log_p,t);
  return std::exp(std::log(scale)+lh/shape);
}

template <typename Rng>
inline double rweibull(Rng &rng, double shape, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale * std::pow(-std::log1p(-u), 1.0 / shape);
}

}  // namespace distributions
}  // namespace cppunivariate
