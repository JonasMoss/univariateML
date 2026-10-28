#pragma once
#include "../lomax/dpqr.hpp"
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dinvpareto(double x, double shape, double scale, bool log) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  if (x == 0) {
    if (shape < 1) return std::numeric_limits<double>::infinity();
    if (shape == 1) return log ? -std::log(scale) : 1/scale;
  }
  if (x <= 0 || x == std::numeric_limits<double>::infinity())
    return log ? -std::numeric_limits<double>::infinity() : 0;
  const double ll=dlomax(1/x, scale, shape, true)-2*std::log(x);
  return log ? ll : std::exp(ll);
}
inline double pinvpareto(double x, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  if (x <= 0) { const double p=lower_tail ? 0 : 1; return log_p ? std::log(p) : p; }
  return plomax(1/x, scale, shape, !lower_tail, log_p);
}
inline double qinvpareto(double p, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  return 1/qlomax(p, scale, shape, !lower_tail, log_p);
}
template <typename Rng> inline double rinvpareto(Rng &rng, double shape, double scale) { return 1/rlomax(rng, scale, shape); }
} }
