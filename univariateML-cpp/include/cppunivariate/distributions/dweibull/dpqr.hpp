// Discrete Weibull (type I) in extraDistr's parametrization: shape1 = q in
// (0, 1), shape2 = beta > 0, P(X >= k) = q^(k^beta) on k = 0, 1, ....
// Everything goes through the cumulative hazard H(k) = t*k^beta,
// t = -log q, in the log domain, so log pmf and tails stay finite where
// q^(k^beta) underflows. Non-integer CDF arguments are floored.

#pragma once

#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions {

namespace dweibull_dpqr_detail {

inline bool valid(double q, double beta) {
  return q > 0 && q < 1 && beta > 0 && std::isfinite(beta);
}

// log(1 - exp(-D)) from log D.
inline double log1mexp_log(double logD) {
  if (logD < -30) return logD - std::exp(logD) / 2;
  const double D = std::exp(logD);
  if (D < 0.6931471805599453) return std::log(-std::expm1(-D));
  return std::log1p(-std::exp(-D));
}

// Smallest k >= 0 with H(k + 1) >= y.
inline double quantile_from_hazard(double y, double log_t, double beta) {
  if (!(y > 0)) return 0;
  if (std::isinf(y)) return std::numeric_limits<double>::infinity();
  const double log_y = std::log(y);
  double k = std::ceil(std::exp((log_y - log_t) / beta)) - 1;
  if (!(k > 0)) k = 0;
  if (k < 9007199254740992.0) {
    // Correct rounding of the closed form by one step either way.
    if (log_t + beta * std::log(k + 1) < log_y) k += 1;
    if (k > 0 && log_t + beta * std::log(k) >= log_y) k -= 1;
  }
  return k;
}

}  // namespace dweibull_dpqr_detail

inline double ddweibull(double x, double shape1, double shape2, bool log) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kNegInf = -std::numeric_limits<double>::infinity();
  if (std::isnan(x) || !dweibull_dpqr_detail::valid(shape1, shape2)) return kNaN;
  if (x < 0 || std::isinf(x) || x != std::floor(x)) return log ? kNegInf : 0.0;
  double lp;
  if (x == 0) {
    lp = std::log1p(-shape1);
  } else {
    const double log_t = std::log(-std::log(shape1));
    const double delta = std::log1p(1 / x), s = shape2 * delta;
    const double log_a = log_t + shape2 * std::log(x);
    // log(expm1(s)): t*((x+1)^beta - x^beta) = exp(log_a + log_e).
    const double log_e = s < 1e-10 ? std::log(shape2) + std::log(delta) + s / 2
                       : s > 40     ? s + std::log1p(-std::exp(-s))
                                    : std::log(std::expm1(s));
    lp = -std::exp(log_a) + dweibull_dpqr_detail::log1mexp_log(log_a + log_e);
  }
  return log ? lp : std::exp(lp);
}

inline double pdweibull(double q, double shape1, double shape2, bool lower_tail,
                        bool log_p) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(q) || !dweibull_dpqr_detail::valid(shape1, shape2)) return kNaN;
  const double k = std::floor(q);
  double log_lower, log_upper;
  if (k < 0) {
    log_lower = -std::numeric_limits<double>::infinity();
    log_upper = 0;
  } else if (std::isinf(k)) {
    log_lower = 0;
    log_upper = -std::numeric_limits<double>::infinity();
  } else {
    // P(X > k) = exp(-H(k + 1)).
    const double log_h = std::log(-std::log(shape1)) + shape2 * std::log(k + 1);
    log_upper = -std::exp(log_h);
    log_lower = dweibull_dpqr_detail::log1mexp_log(log_h);
  }
  const double out = lower_tail ? log_lower : log_upper;
  return log_p ? out : std::exp(out);
}

inline double qdweibull(double p, double shape1, double shape2, bool lower_tail,
                        bool log_p) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(p) || !dweibull_dpqr_detail::valid(shape1, shape2)) return kNaN;
  if (log_p ? p > 0 : (p < 0 || p > 1)) return kNaN;
  // y = -log P(X > k) at the target lower-tail probability.
  double y;
  if (log_p) {
    y = lower_tail ? (p == 0 ? std::numeric_limits<double>::infinity()
                             : -dweibull_dpqr_detail::log1mexp_log(std::log(-p)))
                   : -p;
  } else {
    y = lower_tail ? -std::log1p(-p) : -std::log(p);
  }
  const double seed=dweibull_dpqr_detail::quantile_from_hazard(
      y, std::log(-std::log(shape1)), shape2);
  if (y==0 || std::isinf(y)) return seed;
  if (seed<9007199254740992.0) {
    const double edge=pdweibull(9007199254740992.0,shape1,shape2,lower_tail,log_p);
    if (std::isnan(edge)) return kNaN;
    if (lower_tail ? edge>=p : edge<=p) return seed;
  }
  return special::discrete_search(0,INFINITY,[&](double k) {
    const double f=pdweibull(k,shape1,shape2,lower_tail,log_p);
    return std::isnan(f) ? -1 : int(lower_tail ? f>=p : f<=p);
  });
}

// Inversion: X = floor(scale * E^(1/beta)) with E ~ Exp(1).
template <typename Rng>
inline double rdweibull(Rng &rng, double shape1, double shape2) {
  if (!dweibull_dpqr_detail::valid(shape1, shape2)) {
    return std::numeric_limits<double>::quiet_NaN();
  }
  const double y = -std::log(rng.runif(0.0, 1.0));
  const double seed=dweibull_dpqr_detail::quantile_from_hazard(
      y, std::log(-std::log(shape1)), shape2);
  if (seed<9007199254740992.0 &&
      pdweibull(9007199254740992.0,shape1,shape2,false,true)<=-y) return seed;
  return qdweibull(-y,shape1,shape2,false,true);
}

}  // namespace distributions
}  // namespace cppunivariate
