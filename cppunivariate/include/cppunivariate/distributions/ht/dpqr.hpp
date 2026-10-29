#pragma once
#include "mle.hpp"
#include "../lst/tails.hpp"
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
  const double t=2*(std::log(q)-std::log(sigma))-std::log(nu);
  const double lp=missing_detail::beta_log(-student_detail::softplus(-t),
      -student_detail::softplus(t),0.5,nu/2,lower_tail);
  return log_p?lp:std::exp(lp);
}
inline double qht(double p,double nu,double sigma,bool lower_tail,bool log_p) {
  if(!ht_valid(nu,sigma)||std::isnan(p)||(log_p?p>0:(p<0||p>1))) return cppunivariate::special::kNaN;
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(tails.lower==special::kNegInf) return 0;
  if(tails.upper==special::kNegInf) return special::kPosInf;
  const double lp=log_p?p:std::log(p);
  if(lower_tail && lp < -18) return std::exp(lp-std::log(2.0)-ht_detail::log_mode(nu)+std::log(sigma));
  const double upper=lower_tail?cppunivariate::special::fs_log1mexp(lp):lp;
  const double t=missing_detail::beta_logit_q(upper,nu/2,0.5,true);
  return std::exp(std::log(sigma)+0.5*(std::log(nu)-t));
}
template <typename Rng>
inline double rht(Rng &rng,double nu,double sigma) {
  if(!ht_valid(nu,sigma)) return cppunivariate::special::kNaN;
  return sigma*std::abs(rng.rt(nu));
}
} // namespace distributions
} // namespace cppunivariate
