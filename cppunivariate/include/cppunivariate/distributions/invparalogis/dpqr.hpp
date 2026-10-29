#pragma once
#include "../invburr/dpqr.hpp"
#include "../paralogis/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dinvparalogis(double x, double shape, double scale, bool log) { return dinvburr(x, shape, shape, scale, log); }
inline double pinvparalogis(double q, double shape, double scale, bool lower_tail, bool log_p) { return pinvburr(q, shape, shape, scale, lower_tail, log_p); }
inline double qinvparalogis(double p, double shape, double scale, bool lower_tail, bool log_p) { return qinvburr(p, shape, shape, scale, lower_tail, log_p); }
template <typename Rng> inline double rinvparalogis(Rng &rng, double shape, double scale) { return 1/rparalogis(rng, shape, 1.0/scale); }

} }
