#pragma once
#include "../exp/dpqr.hpp"
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dinvexp(double x, double rate, bool log) {
  if (!(rate > 0 && std::isfinite(rate))) return std::numeric_limits<double>::quiet_NaN();
  if (x <= 0 || x == std::numeric_limits<double>::infinity())
    return log ? -std::numeric_limits<double>::infinity() : 0;
  const double ll=dexp(1/x, 1.0/rate, true)-2*std::log(x);
  return log ? ll : std::exp(ll);
}
inline double pinvexp(double x, double rate, bool lower_tail, bool log_p) {
  if (!(rate > 0 && std::isfinite(rate))) return std::numeric_limits<double>::quiet_NaN();
  if (x <= 0) { const double p=lower_tail ? 0 : 1; return log_p ? std::log(p) : p; }
  return pexp(1/x, 1.0/rate, !lower_tail, log_p);
}
inline double qinvexp(double p, double rate, bool lower_tail, bool log_p) {
  if (!(rate > 0 && std::isfinite(rate))) return std::numeric_limits<double>::quiet_NaN();
  return 1/qexp(p, 1.0/rate, !lower_tail, log_p);
}
template <typename Rng> inline double rinvexp(Rng &rng, double rate) { return 1/rexp(rng, 1.0/rate); }
} }
