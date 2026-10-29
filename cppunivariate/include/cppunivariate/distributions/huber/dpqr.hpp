#pragma once
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
inline double phuber(double x, double mu, double sigma, double epsilon,
                     bool lower_tail, bool log_p) {
  if (!(sigma>0 && epsilon>0) || !std::isfinite(mu) ||
      !std::isfinite(sigma) || !std::isfinite(epsilon))
    return std::numeric_limits<double>::quiet_NaN();
  const double z=(x-mu)/sigma, a=std::abs(z), norm=huber_z(epsilon);
  double tail;
  if (a>=epsilon) tail=std::exp(-epsilon*(a-epsilon/2))/epsilon/norm;
  else tail=(std::sqrt(std::acos(-1.0)/2)*
      (std::erfc(a/std::sqrt(2.0))-std::erfc(epsilon/std::sqrt(2.0)))+
      std::exp(-epsilon*epsilon/2)/epsilon)/norm;
  const double p=(lower_tail == (z<=0)) ? tail : 1-tail;
  return log_p ? std::log(p) : p;
}
inline double qhuber(double p, double mu, double sigma, double epsilon,
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
  return qhuber(rng.runif(0,1),mu,sigma,epsilon,true,false);
}
}
}
