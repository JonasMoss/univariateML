#pragma once
#include "../ht/dpqr.hpp"
namespace cppunivariate {
namespace distributions {
inline double dhcauchy(double x,double mu,double sigma,bool log) { return dht(x,mu,1,sigma,log); }
inline double phcauchy(double q,double mu,double sigma,bool lower_tail,bool log_p) { return pht(q,mu,1,sigma,lower_tail,log_p); }
inline double qhcauchy(double p,double mu,double sigma,bool lower_tail,bool log_p) { return qht(p,mu,1,sigma,lower_tail,log_p); }
template <typename Rng>
inline double rhcauchy(Rng &rng,double mu,double sigma) { return rht(rng,mu,1,sigma); }
} // namespace distributions
} // namespace cppunivariate
