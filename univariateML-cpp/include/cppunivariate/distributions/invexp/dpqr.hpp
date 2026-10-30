#pragma once
#include "../../special/tails.hpp"
#include "../exp/dpqr.hpp"
#include "../invweibull/dpqr.hpp"
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dinvexp(double x, double scale, bool log) {
  return dinvweibull(x,1.0,scale,log);
}
inline double pinvexp(double q, double scale, bool lower_tail, bool log_p) {
  return pinvweibull(q,1.0,scale,lower_tail,log_p);
}
inline double qinvexp(double p, double scale, bool lower_tail, bool log_p) {
  return qinvweibull(p,1.0,scale,lower_tail,log_p);
}
template <typename Rng> inline double rinvexp(Rng &rng, double scale) { if (!(scale > 0.0 && std::isfinite(scale))) return special::kNaN; return 1/rexp(rng, scale); }
} }
