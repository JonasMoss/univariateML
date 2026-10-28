#pragma once
#include "../paralogis/dpqr.hpp"
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dinvparalogis(double x, double shape, double scale, bool log) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  if (x == 0) {
    if (shape*shape < 1) return std::numeric_limits<double>::infinity();
    if (shape*shape == 1) return log ? -std::log(scale) : 1/scale;
  }
  if (x <= 0 || x == std::numeric_limits<double>::infinity())
    return log ? -std::numeric_limits<double>::infinity() : 0;
  const double ll=dparalogis(1/x, shape, 1.0/scale, true)-2*std::log(x);
  return log ? ll : std::exp(ll);
}
inline double pinvparalogis(double x, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  if (x <= 0) { const double p=lower_tail ? 0 : 1; return log_p ? std::log(p) : p; }
  return pparalogis(1/x, shape, 1.0/scale, !lower_tail, log_p);
}
inline double qinvparalogis(double p, double shape, double scale, bool lower_tail, bool log_p) {
  if (!(shape > 0 && scale > 0 && std::isfinite(shape) && std::isfinite(scale))) return std::numeric_limits<double>::quiet_NaN();
  return 1/qparalogis(p, shape, 1.0/scale, !lower_tail, log_p);
}
template <typename Rng> inline double rinvparalogis(Rng &rng, double shape, double scale) { return 1/rparalogis(rng, shape, 1.0/scale); }
} }
