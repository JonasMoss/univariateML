#pragma once
#include "../burr/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dparalogis(double x, double shape, double scale, bool log) { return dburr(x, shape, shape, scale, log); }
inline double pparalogis(double q, double shape, double scale, bool lower_tail, bool log_p) { return pburr(q, shape, shape, scale, lower_tail, log_p); }
inline double qparalogis(double p, double shape, double scale, bool lower_tail, bool log_p) { return qburr(p, shape, shape, scale, lower_tail, log_p); }
template <typename Rng>
inline double rparalogis(Rng &rng, double shape, double scale) {
  const double u = rng.runif(0.0, 1.0);
  return scale *
         std::pow(std::pow(1.0 - u, -1.0 / shape) - 1.0, 1.0 / shape);
}


} }
