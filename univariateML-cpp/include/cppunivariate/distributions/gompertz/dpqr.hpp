// Gompertz distribution (extraDistr).
// Parameters: a > 0, b > 0; support x >= 0.
//   f(x)   = a * exp(b*x - (a/b) * (exp(b*x) - 1))
//   F(x)   = 1 - exp(-(a/b) * (exp(b*x) - 1))
//   F^-1(p)= log(1 - (b/a) * log(1-p)) / b
// Sample: inverse-CDF on U ~ Uniform(0, 1). Note: extraDistr's sampler uses
// log(1 - b/a*log(u)) / b directly (without the 1-u substitution).

#pragma once

#include "../../special/tails.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double gompertz_log_h(double x,double a,double b) {
  const double bx=b*x;
  if (bx==0) return std::log(a)+std::log(x);
  if (bx>36) return std::log(a)-std::log(b)+bx+special::log1mexp(-bx);
  return std::log(a)+std::log(x)+std::log(std::expm1(bx)/bx);
}
inline double dgompertz(double x, double a, double b, bool log) {
  if (!(a>0 && b>0 && std::isfinite(a) && std::isfinite(b))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<0 || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double h=std::exp(gompertz_log_h(x,a,b));
  const double ld=std::isinf(h) ? special::kNegInf : std::log(a)+b*x-h;
  return log ? ld : std::exp(ld);
}

inline double pgompertz(double q, double a, double b, bool lower_tail,
                        bool log_p) {
  if (!(a>0 && b>0 && std::isfinite(a) && std::isfinite(b))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=0) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double lh=gompertz_log_h(q,a,b);
  return special::tail_probability(lh < -36 ? special::tails_from_log(lh,true) : special::tails_from_log(-std::exp(lh),false),lower_tail,log_p);
}

inline double qgompertz(double p, double a, double b, bool lower_tail,
                        bool log_p) {
  if (!(a>0 && b>0 && std::isfinite(a) && std::isfinite(b))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double lh=t.lower < -0.6931471805599453 ? special::log_neg_log1mexp(t.lower) : std::log(-t.upper);
  const double lv=lh+std::log(b)-std::log(a);
  if (lv < -36) return std::exp(lh-std::log(a));
  return (lv>36 ? lv+std::log1p(std::exp(-lv)) : std::log1p(std::exp(lv)))/b;
}

template <typename Rng>
inline double rgompertz(Rng &rng, double a, double b) {
  // extraDistr's rng_gompertz: log(1 - b/a * log(u)) / b on U ~ Uniform(0, 1).
  const double u = rng.runif(0.0, 1.0);
  const double ratio=b/a, h=-ratio*std::log(u);
  // Preserve the historical seeded draws where its arithmetic is accurate.
  // The quantile accepts log survival directly, avoiding both b/a overflow
  // and cancellation when adding a tiny cumulative hazard to one.
  if (std::isfinite(ratio) && h>=1e-3 && std::isfinite(h))
    return std::log(1.0 + h) / b;
  return qgompertz(std::log(u),a,b,false,true);
}

} // namespace distributions
} // namespace cppunivariate
