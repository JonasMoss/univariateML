#pragma once
#include "../invburr/dpqr.hpp"
#include "../lomax/dpqr.hpp"
namespace cppunivariate { namespace distributions {
inline double dinvpareto(double x, double shape, double scale, bool log) { return dinvburr(x, shape, 1.0, scale, log); }
inline double pinvpareto(double q, double shape, double scale, bool lower_tail, bool log_p) { return pinvburr(q, shape, 1.0, scale, lower_tail, log_p); }
inline double qinvpareto(double p, double shape, double scale, bool lower_tail, bool log_p) { return qinvburr(p, shape, 1.0, scale, lower_tail, log_p); }
template <typename Rng> inline double rinvpareto(Rng &rng, double shape, double scale) { return 1/rlomax(rng, scale, shape); }

} }
