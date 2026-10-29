#pragma once
#include "mle.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline bool ht_valid(double nu,double sigma) { return nu>0 && sigma>0 && std::isfinite(nu) && std::isfinite(sigma); }
inline double dht(double x,double nu,double sigma,bool log) {
  if(!ht_valid(nu,sigma)||std::isnan(x)) return cppunivariate::special::kNaN;
  if(x<0) return log?cppunivariate::special::kNegInf:0;
  const double ld=std::log(2.0)+ht_detail::log_mode(nu)-std::log(sigma)
    -(x==0?0:0.5*(nu+1)*ht_detail::softplus(2*(std::log(x)-std::log(sigma))-std::log(nu)));
  return log?ld:std::exp(ld);
}
inline double pht(double q,double nu,double sigma,bool lower_tail,bool log_p) {
  if(!ht_valid(nu,sigma)||std::isnan(q)) return cppunivariate::special::kNaN;
  if(q<=0) return lower_tail?(log_p?cppunivariate::special::kNegInf:0):(log_p?0:1);
  const double z=q/sigma;
  // Near zero, integrate the essentially constant mode without subtracting 1/2.
  double lp;
  if(z<1e-8*std::min(1.0,std::sqrt(nu))) {
    lp=std::log(2.0)+ht_detail::log_mode(nu)+std::log(q)-std::log(sigma);
    if(!lower_tail) lp=cppunivariate::special::fs_log1mexp(lp);
  } else {
    lp=std::min(0.0,std::log(2.0)+cppunivariate::special::fs_t_p(z,nu,false,true));
    if(lower_tail) lp=cppunivariate::special::fs_log1mexp(lp);
  }
  return log_p?lp:std::exp(lp);
}
inline double qht(double p,double nu,double sigma,bool lower_tail,bool log_p) {
  if(!ht_valid(nu,sigma)||std::isnan(p)||(log_p?p>0:(p<0||p>1))) return cppunivariate::special::kNaN;
  const double lp=log_p?p:std::log(p);
  if(lower_tail && lp < -18) return std::exp(lp-std::log(2.0)-ht_detail::log_mode(nu)+std::log(sigma));
  const double upper=lower_tail?cppunivariate::special::fs_log1mexp(lp):lp;
  return sigma*cppunivariate::special::fs_t_q(upper-std::log(2.0),nu,false);
}
template <typename Rng>
inline double rht(Rng &rng,double nu,double sigma) {
  if(!ht_valid(nu,sigma)) return cppunivariate::special::kNaN;
  return sigma*std::abs(rng.rt(nu));
}
} // namespace distributions
} // namespace cppunivariate
