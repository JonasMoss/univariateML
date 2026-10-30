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
  const double h=std::exp(shape*(std::log(x)-std::log(scale)));
  const double ld=std::isinf(h) ? special::kNegInf : std::log(shape)-std::log(scale)+(shape-1)*(std::log(x)-std::log(scale))-h;
  return log ? ld : std::exp(ld);
}

inline double pweibull(double q, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh = shape*(std::log(q)-std::log(scale));
  const auto t = lh < -36 ? special::tails_from_log(lh,true) : special::tails_from_log(-std::exp(lh),false);
  return special::tail_probability(t,lower_tail,log_p);
}

inline double qweibull(double p, double shape, double scale, bool lower_tail,
                       bool log_p) {
  if (!(shape>0 && std::isfinite(shape) && scale>0 && std::isfinite(scale))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=t.lower < -0.6931471805599453 ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper);
  return std::exp(std::log(scale)+lh/shape);
}

template <typename Rng>
inline double rweibull(Rng &rng, double shape, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale * std::pow(-std::log1p(-u), 1.0 / shape);
}

}  // namespace distributions
}  // namespace cppunivariate
