#pragma once
#include "mle.hpp"
#include "../lst/tails.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline bool ht_valid(double nu,double sigma) { return nu>0 && sigma>0 && std::isfinite(nu) && std::isfinite(sigma); }
inline double dht(double x,double mu,double nu,double sigma,bool log) {
  if(!std::isfinite(mu)||!ht_valid(nu,sigma)||std::isnan(x)) return cppunivariate::special::kNaN;
  const double shifted=x-mu;
  if(std::isfinite(x) && !std::isfinite(shifted)) return special::kNaN;
  x=shifted;
  if(x<0) return log?cppunivariate::special::kNegInf:0;
  const double ld=std::log(2.0)+ht_detail::log_mode(nu)-std::log(sigma)
    -(x==0?0:0.5*(nu+1)*ht_detail::softplus(2*(std::log(x)-std::log(sigma))-std::log(nu)));
  return log?ld:std::exp(ld);
}
inline double pht(double q,double mu,double nu,double sigma,bool lower_tail,bool log_p) {
  if(!std::isfinite(mu)||!ht_valid(nu,sigma)||std::isnan(q)) return cppunivariate::special::kNaN;
  const double shifted=q-mu;
  if(std::isfinite(q) && !std::isfinite(shifted)) return special::kNaN;
  q=shifted;
  if(q<=0) return lower_tail?(log_p?cppunivariate::special::kNegInf:0):(log_p?0:1);
  const double logx=std::log(q)-std::log(sigma);
  if(nu<1e8) {
    const double t=2*logx-std::log(nu);
    const double lp=missing_detail::beta_log(-student_detail::softplus(-t),
        -student_detail::softplus(t),0.5,nu/2,lower_tail);
    return log_p?lp:std::exp(lp);
  }
  if(logx < -2) {
    // Integrate the density series directly: subtracting a Student tail
    // from 1/2 loses relative accuracy near the half-family endpoint.
    const double x2=std::exp(2*logx);
    double term=1,sum=1;
    bool converged=false;
    for(int k=1;k<=32;++k) {
      term*=-.5*(1+(2*k-1)/nu)*x2/k;
      const double delta=term/(2*k+1);sum+=delta;
      if(std::abs(delta)<std::abs(sum)*2e-16) {converged=true;break;}
    }
    if(!converged) return special::kNaN;
    const double lp=std::log(2.0)+student_detail::logmode(nu)+logx+std::log(sum);
    const double result=lower_tail?lp:special::log1mexp(lp);
    return log_p?result:std::exp(result);
  }
  const double upper=std::log(2.0)+student_detail::probability_log(logx,false,nu,false,true);
  const double lp=lower_tail?special::log1mexp(upper):upper;
  return log_p?lp:std::exp(lp);
}
inline double qht(double p,double mu,double nu,double sigma,bool lower_tail,bool log_p) {
  if(!std::isfinite(mu)||!ht_valid(nu,sigma)||std::isnan(p)||(log_p?p>0:(p<0||p>1))) return cppunivariate::special::kNaN;
  const auto tails=special::quantile_log_tails(p,lower_tail,log_p);
  if(tails.lower==special::kNegInf) return mu;
  if(tails.upper==special::kNegInf) return special::kPosInf;
  const double lp=log_p?p:std::log(p);
  if(nu>=1e8 && tails.lower < -9) {
    const double lx=tails.lower-std::log(2.0)-student_detail::logmode(nu);
    const double correction=std::log1p((1+1/nu)*std::exp(2*lx)/6);
    return mu+std::exp(std::log(sigma)+lx+correction);
  }
  if(lower_tail && lp < -18) return mu+std::exp(lp-std::log(2.0)-ht_detail::log_mode(nu)+std::log(sigma));
  const double upper=lower_tail?cppunivariate::special::fs_log1mexp(lp):lp;
  if(nu>=1e8) return mu-student_detail::quantile(upper-std::log(2.0),nu,true,true,std::log(sigma));
  const double t=missing_detail::beta_logit_q(upper,nu/2,0.5,true);
  return mu+std::exp(std::log(sigma)+0.5*(std::log(nu)-t));
}
template <typename Rng>
inline double rht(Rng &rng,double mu,double nu,double sigma) {
  if(!std::isfinite(mu)||!ht_valid(nu,sigma)) return cppunivariate::special::kNaN;
  return mu+sigma*std::abs(rng.rt(nu));
}
} // namespace distributions
} // namespace cppunivariate
