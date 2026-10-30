// Inverse Weibull: shape > 0, scale > 0; F(x) = exp(-(scale/x)^shape).
#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double dinvweibull(double x, double shape, double scale, bool log) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<=0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double lh=-shape*(log_rate+std::log(x));
  const double h=std::exp(lh);
  const double ld=std::isinf(h) ? special::kNegInf : std::log(shape)+lh-h-std::log(x);
  return log ? ld : std::exp(ld);
}

inline double pinvweibull(double q, double shape, double scale,
                          bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh=-shape*(log_rate+std::log(q));
  const auto t=lh < -36 ? special::tails_from_log(lh,false) : special::tails_from_log(-std::exp(lh),true);
  return special::tail_probability(t,lower_tail,log_p);
}

inline double qinvweibull(double p, double shape, double scale,
                          bool lower_tail, bool log_p) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  const double rate = 1.0 / scale;
  const double log_rate = std::isfinite(rate) ? std::log(rate) : -std::log(scale);
  if (!(shape>0 && std::isfinite(shape))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=t.upper < -0.6931471805599453 ? special::log_neg_log1mexp(t.upper) : std::log(-t.lower);
  return std::exp(-log_rate-lh/shape);
}

template <typename Rng>
inline double rinvweibull(Rng &rng, double shape, double scale) {
  if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN;
  if (!(shape > 0.0 && std::isfinite(shape))) return special::kNaN;
  const double u = rng.runif(0.0, 1.0);
  return scale / std::pow(-std::log(u), 1.0 / shape);
}

} // namespace distributions
} // namespace cppunivariate
