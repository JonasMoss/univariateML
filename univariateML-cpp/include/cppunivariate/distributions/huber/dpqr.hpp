#pragma once
#include "../../special/tails.hpp"
#include "mle.hpp"
#include "../../special/special.hpp"
namespace cppunivariate {
namespace distributions {
inline double dhuber(double x, double mu, double sigma, double epsilon, bool log) {
  if (!(sigma>0 && epsilon>0) || !std::isfinite(mu) ||
      !std::isfinite(sigma) || !std::isfinite(epsilon))
    return std::numeric_limits<double>::quiet_NaN();
  const double l=-std::log(sigma)-std::log(huber_z(epsilon))-
      huber_rho((x-mu)/sigma,epsilon);
  return log ? l : std::exp(l);
}
inline double phuber(double q, double mu, double sigma, double epsilon,
                     bool lower_tail, bool log_p) {
  if (!(sigma>0 && epsilon>0 && std::isfinite(mu) && std::isfinite(sigma) && std::isfinite(epsilon))) return special::kNaN;
  const double z=special::scaled_difference(q,mu,sigma), a=std::fabs(z), norm=huber_z(epsilon);
  const double lp=a>=epsilon ? -epsilon*(a-epsilon/2)-std::log(epsilon)-std::log(norm) :
      std::log((std::sqrt(M_PI/2)*(std::erfc(a/std::sqrt(2.0))-std::erfc(epsilon/std::sqrt(2.0)))+std::exp(-epsilon*epsilon/2)/epsilon)/norm);
  return special::tail_probability(special::tails_from_log(lp,z<=0),lower_tail,log_p);
}
inline double qhuber(double p, double mu, double sigma, double epsilon,
                     bool lower_tail, bool log_p) {
  if (!(sigma>0 && epsilon>0 && std::isfinite(mu) && std::isfinite(sigma) && std::isfinite(epsilon))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  const double offset=special::probability_minus_half(p,lower_tail,log_p);
  const double central=offset*huber_z(epsilon);
  if (std::fabs(central)<1e-3 && std::fabs(central)<epsilon/2)
    return std::fma(sigma,central+central*central*central/6,mu);

  const double lp=std::min(t.lower,t.upper), norm=huber_z(epsilon);
  const double edge=-epsilon*epsilon/2-std::log(epsilon)-std::log(norm);
  const double a=lp<=edge ? epsilon/2-(lp+std::log(epsilon)+std::log(norm))/epsilon :
      special::fs_log_quantile(lp,[epsilon](double z) { return phuber(z,0,1,epsilon,false,true); });
  return std::fma(sigma,t.lower<t.upper ? -a : a,mu);
}
// Preserve the existing seeded sampler transform independently of the
// tail-accurate public quantile. No RNG draws or arithmetic are changed.
inline double huber_sampling_quantile(double p, double mu, double sigma, double epsilon,
                     bool lower_tail, bool log_p) {
  if (log_p) p=std::exp(p);
  if (!lower_tail) p=1-p;
  if (!(p>=0 && p<=1 && sigma>0 && epsilon>0) ||
      !std::isfinite(mu) || !std::isfinite(sigma) || !std::isfinite(epsilon))
    return std::numeric_limits<double>::quiet_NaN();
  const double t=std::min(p,1-p), norm=huber_z(epsilon);
  const double edge=std::exp(-epsilon*epsilon/2)/epsilon/norm;
  double a;
  if (t<=edge) a=epsilon/2-(std::log(t)+std::log(epsilon)+std::log(norm))/epsilon;
  else a=cppunivariate::special::qnorm(0.5+(0.5-t)*norm/std::sqrt(2*std::acos(-1.0)),0,1,true,false);
  return mu+sigma*(p<0.5 ? -a : a);
}
template <typename Rng>
inline double rhuber(Rng& rng, double mu, double sigma, double epsilon) {
  return huber_sampling_quantile(rng.runif(0,1),mu,sigma,epsilon,true,false);
}
}
}
