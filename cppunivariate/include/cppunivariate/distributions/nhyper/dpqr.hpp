#pragma once
#include "mle.hpp"
#include <cfloat>

namespace cppunivariate {
namespace distributions {
namespace nhyper_detail {
inline bool valid(double n, double m, double r) {
  return hyper_detail::is_count(n) && hyper_detail::is_count(m) &&
    hyper_detail::is_count(r) && r <= m &&
    static_cast<long double>(n) + m <= hyper_detail::kMaxCount;
}
inline double logadd(double a, double b) {
  if (a < b) std::swap(a, b);
  return std::isinf(a) ? a : a + std::log1p(std::exp(b-a));
}
}
inline double dnhyper(double x, double m, double N, double r, bool log) {
  if (!hyper_detail::is_count(N) || N < m) return std::numeric_limits<double>::quiet_NaN();
  const double n = N - m;
  if (!nhyper_detail::valid(n,m,r) || std::isnan(x)) return std::numeric_limits<double>::quiet_NaN();
  const double lp = nhyper_log_pmf(x,n,m,r);
  return log ? lp : std::exp(lp);
}
// T <= q iff at least r white balls occur in q ordinary urn draws.
// Sum the smaller hypergeometric tail; log complements retain small tails.
inline double pnhyper(double q, double m, double N, double r, bool lower_tail, bool log_p) {
  if (!hyper_detail::is_count(N) || N < m) return std::numeric_limits<double>::quiet_NaN();
  const double n = N - m;
  if (!nhyper_detail::valid(n,m,r) || std::isnan(q)) return std::numeric_limits<double>::quiet_NaN();
  const double lo = r, hi = r == 0 ? 0 : n+r;
  double lp = -std::numeric_limits<double>::infinity();
  q = std::floor(q);
  if (q < lo) lp = lower_tail ? lp : 0;
  else if (q >= hi) lp = lower_tail ? 0 : lp;
  else {
    // H is hypergeometric(N, m, q). Choose its small tail by its mean.
    const bool below = r > q * (m/(n+m));
    const double from = below ? r : std::max(0.0,q-n);
    const double to = below ? std::min(q,m) : r-1;
    for (double v = from; v <= to; ++v)
      lp = nhyper_detail::logadd(lp,hyper_log_pmf(v,n+m,m,q));
    lp = std::min(0.0,lp);
    if (below != lower_tail)
      lp = lp < -0.6931471805599453 ? std::log1p(-std::exp(lp)) : std::log(-std::expm1(lp));
  }
  return log_p ? lp : std::exp(lp);
}
inline double qnhyper(double p, double m, double N, double r, bool lower_tail, bool log_p) {
  if (!hyper_detail::is_count(N) || N < m) return std::numeric_limits<double>::quiet_NaN();
  const double n = N - m;
  if (!nhyper_detail::valid(n,m,r) || std::isnan(p) ||
      (log_p ? p > 0 : (p < 0 || p > 1))) return std::numeric_limits<double>::quiet_NaN();
  double lp = log_p ? p : std::log(p);
  double lo = r, hi = r == 0 ? 0 : n+r;
  if (lp == -std::numeric_limits<double>::infinity()) return lower_tail ? lo : hi;
  if (lp == 0) return lower_tail ? hi : lo;
  if (lp > -0.6931471805599453) {
    lp = std::log(-std::expm1(lp)); lower_tail = !lower_tail;
  }
  while (lo < hi) {
    const double mid = lo + std::floor((hi-lo)/2);
    const double v = pnhyper(mid,m,N,r,lower_tail,true);
    if (lower_tail ? v >= lp - 1000*DBL_EPSILON : v <= lp + 1000*DBL_EPSILON) hi = mid;
    else lo = mid+1;
  }
  return lo;
}
template <typename Rng>
inline double rnhyper(Rng &rng, double m, double N, double r) {
  if (!hyper_detail::is_count(N) || N < m) return std::numeric_limits<double>::quiet_NaN();
  return qnhyper(rng.runif(0,1),m,N,r,true,false);
}
}  // namespace distributions
}  // namespace cppunivariate
