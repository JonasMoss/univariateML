#pragma once
#include "../ht/dpqr.hpp"
namespace cppunivariate {
namespace distributions {
inline double dhcauchy(double x,double mu,double sigma,bool log) { return dht(x,mu,1,sigma,log); }
inline double phcauchy(double q,double mu,double sigma,bool lower_tail,bool log_p) { return pht(q,mu,1,sigma,lower_tail,log_p); }
inline double qhcauchy(double p,double mu,double sigma,bool lower_tail,bool log_p) {
  if(!(sigma>0 && std::isfinite(sigma) && std::isfinite(mu))) return special::kNaN;
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(!tails.valid) return special::kNaN;
  const double offset=special::probability_offset(p,log_p)*(lower_tail?1:-1);
  if(std::abs(offset)<=.25) {
    const double u=std::tan(1.57079632679489661923*offset);
    const double center=mu+sigma;
    return std::fma(sigma,2*u/(1-u),center)+special::addition_residual(mu,sigma,center);
  }
  return qht(p,mu,1,sigma,lower_tail,log_p);
}
template <typename Rng>
inline double rhcauchy(Rng &rng,double mu,double sigma) { return rht(rng,mu,1,sigma); }
} // namespace distributions
} // namespace cppunivariate
