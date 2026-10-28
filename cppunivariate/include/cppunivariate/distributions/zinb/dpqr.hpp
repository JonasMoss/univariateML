#pragma once
#include "../nbinom/dpqr.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline double dzinb(double x, double size, double prob, double pi, bool log) {
  if (!(size > 0) || !(prob > 0 && prob <= 1) || !(pi >= 0 && pi <= 1))
    return std::numeric_limits<double>::quiet_NaN();
  if (std::isnan(x)) return x;
  if (x < 0 || !std::isfinite(x) || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  if (pi == 1 || prob == 1) return x == 0 ? (log ? 0 : 1) :
      (log ? -std::numeric_limits<double>::infinity() : 0);
  double l = std::log1p(-pi) + cppunivariate::special::dnbinom(x, size, prob, true);
  if (x == 0 && pi > 0) {
    const double a = std::log(pi), m = std::max(a, l);
    l = m + std::log1p(std::exp(std::min(a, l) - m));
  }
  return log ? l : std::exp(l);
}
inline double pzinb(double q, double size, double prob, double pi,
                    bool lower_tail, bool log_p) {
  if (!(size > 0) || !(prob > 0 && prob <= 1) || !(pi >= 0 && pi <= 1) || std::isnan(q))
    return std::numeric_limits<double>::quiet_NaN();
  double tail;
  if (q < 0) tail = 1;
  else if (prob == 1 || pi == 1 || std::isinf(q)) tail = 0;
  else tail = (1-pi) * cppunivariate::special::negative_binomial_cdf(std::floor(q), size, prob, false);
  if (log_p) return lower_tail ? std::log1p(-tail) : std::log(tail);
  return lower_tail ? 1-tail : tail;
}
inline double qzinb(double p, double size, double prob, double pi,
                    bool lower_tail, bool log_p) {
  if (!(size > 0) || !(prob > 0 && prob <= 1) || !(pi >= 0 && pi <= 1) || std::isnan(p) ||
      (log_p ? p > 0 : (p < 0 || p > 1))) return std::numeric_limits<double>::quiet_NaN();
  if (log_p) p = lower_tail ? std::exp(p) : -std::expm1(p);
  else if (!lower_tail) p = 1-p;
  if (pi == 1 || prob == 1 || p <= pi) return 0;
  return cppunivariate::distributions::qnbinom((p-pi)/(1-pi), size, prob, true, false);
}
template <typename Rng>
inline double rzinb(Rng &rng, double size, double prob, double pi) {
  if (pi == 1 || prob == 1 || rng.runif(0, 1) < pi) return 0;
  return cppunivariate::distributions::rnbinom(rng, size, prob);
}
} // namespace distributions
} // namespace cppunivariate
