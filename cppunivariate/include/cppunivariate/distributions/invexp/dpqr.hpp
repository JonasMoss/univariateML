#pragma once
#include "../../special/tails.hpp"
#include "../exp/dpqr.hpp"
#include "../invweibull/dpqr.hpp"
#include <limits>
namespace cppunivariate { namespace distributions {
inline double dinvexp(double x, double rate, bool log) {
  return dinvweibull(x,1.0,rate,log);
}
inline double pinvexp(double q, double rate, bool lower_tail, bool log_p) {
  return pinvweibull(q,1.0,rate,lower_tail,log_p);
}
inline double qinvexp(double p, double rate, bool lower_tail, bool log_p) {
  return qinvweibull(p,1.0,rate,lower_tail,log_p);
}
template <typename Rng> inline double rinvexp(Rng &rng, double rate) { return 1/rexp(rng, 1.0/rate); }
} }
