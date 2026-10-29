#pragma once
#include "../ht/dpqr.hpp"
namespace cppunivariate {
namespace distributions {
inline double dhcauchy(double x,double sigma,bool log) { return dht(x,1,sigma,log); }
inline double phcauchy(double q,double sigma,bool lower_tail,bool log_p) { return pht(q,1,sigma,lower_tail,log_p); }
inline double qhcauchy(double p,double sigma,bool lower_tail,bool log_p) { return qht(p,1,sigma,lower_tail,log_p); }
template <typename Rng>
inline double rhcauchy(Rng &rng,double sigma) { return rht(rng,1,sigma); }
} // namespace distributions
} // namespace cppunivariate
