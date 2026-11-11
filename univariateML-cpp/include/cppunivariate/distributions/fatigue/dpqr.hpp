// Birnbaum-Saunders (fatigue life) distribution (extraDistr).
// Parameters: alpha > 0 (shape), beta > 0 (scale), mu (location).
//   z      = x - mu, zb = sqrt(z/beta), bz = sqrt(beta/z)
//   f(x)   = (zb + bz) / (2 * alpha * z) * phi((zb - bz)/alpha)
//   F(x)   = Phi((zb - bz)/alpha)
//   F^-1(p)= ( alpha/2*Z_p + sqrt((alpha/2*Z_p)^2 + 1) )^2 * beta + mu
//            with Z_p = qnorm(p)
// Sample: Z ~ N(0, 1) -> X = ( alpha/2*Z + sqrt((alpha/2*Z)^2+1) )^2 * beta + mu

#pragma once

#include "cppunivariate/core/constants.hpp"
#include "../../special/tails.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

inline double fatigue_normal_log_tail(double a) {
  if (a<30) return special::pnorm(a,0,1,false,true);
  if (!std::isfinite(a)) return special::kNegInf;
  const double inv=1/a/a;
  double term=1, sum=1;
  for (int k=1;k<=16;++k) { term*=-(2*k-1)*inv; sum+=term; }
  return -(0.5*a)*a-std::log(a)-0.5*std::log(2*cppunivariate::pi)+std::log(sum);
}
inline double fatigue_w(double x,double alpha,double beta,double mu) {
  const double lz=special::log_shifted_positive_ratio(x,mu,beta);
  const double a=std::fabs(lz)/2;
  const double lw=a+special::log1mexp(-2*a)-std::log(alpha);
  return std::copysign(std::exp(lw),lz);
}
inline double dfatigue(double x, double alpha, double beta, double mu,
                       bool log) {
  if (!(alpha>0 && beta>0 && std::isfinite(alpha) && std::isfinite(beta) && std::isfinite(mu))) return special::kNaN;
  if (std::isnan(x)) return special::kNaN;
  if (x<=mu || x==special::kPosInf) return log ? special::kNegInf : 0;
  const double lz=std::log(x-mu), a=0.5*special::log_shifted_positive_ratio(x,mu,beta);
  const double w=fatigue_w(x,alpha,beta,mu);
  const double ld=special::log_sum_exp(a,-a)-std::log(2.0)-std::log(alpha)-lz-0.5*std::log(2*cppunivariate::pi)-(0.5*w)*w;
  return log ? ld : std::exp(ld);
}

inline double pfatigue(double q, double alpha, double beta, double mu,
                       bool lower_tail, bool log_p) {
  if (!(alpha>0 && beta>0 && std::isfinite(alpha) && std::isfinite(beta) && std::isfinite(mu))) return special::kNaN;
  if (std::isnan(q)) return special::kNaN;
  if (q<=mu) return special::tail_probability(special::tails_from_log(special::kNegInf,true),lower_tail,log_p);
  const double w=fatigue_w(q,alpha,beta,mu);
  return special::tail_probability(special::tails_from_log(fatigue_normal_log_tail(std::fabs(w)),w<=0),lower_tail,log_p);
}

inline double qfatigue(double p, double alpha, double beta, double mu,
                       bool lower_tail, bool log_p) {
  if (!(alpha>0 && beta>0 && std::isfinite(alpha) && std::isfinite(beta) && std::isfinite(mu))) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower==special::kNegInf) return mu;
  if (t.upper==special::kNegInf) return special::kPosInf;
  const double z=special::normal_quantile(p,lower_tail,log_p);
  const double aZ=(alpha/2)*z;
  if(aZ==0 && z!=0) {
    const double center=mu+beta;
    return std::fma(beta*alpha,z,center)+special::addition_residual(mu,beta,center);
  }
  const double displacement=std::isfinite(aZ)?2*std::asinh(aZ)
      :std::copysign(2*(std::log(alpha)+std::log(std::abs(z))),z);
  return special::shifted_scaled_exp(mu,beta,displacement);
}

template <typename Rng>
inline double rfatigue(Rng &rng, double alpha, double beta, double mu) {
  const double z = rng.rnorm();
  const double aZ2 = alpha * z / 2.0;
  return std::pow(aZ2 + std::sqrt(aZ2 * aZ2 + 1.0), 2.0) * beta + mu;
}

} // namespace distributions
} // namespace cppunivariate
