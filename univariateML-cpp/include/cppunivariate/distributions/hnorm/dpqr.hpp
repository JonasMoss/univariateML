#pragma once
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {
inline double dhnorm(double x, double mu, double sigma, bool log) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0) || std::isnan(x))
    return cppunivariate::special::kNaN;
  if (x < mu) return log ? cppunivariate::special::kNegInf : 0.0;
  const double z = special::scaled_difference(x,mu,sigma);
  const double ld = 0.5*std::log(2.0/std::acos(-1.0)) - std::log(sigma) - 0.5*z*z;
  return log ? ld : std::exp(ld);
}
inline double phnorm(double q, double mu, double sigma, bool lower_tail, bool log_p) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma>0) || std::isnan(q)) return special::kNaN;
  const double z=q<=mu ? 0.0 : special::scaled_difference(q,mu,sigma);
  if (z<1e-8) {
    const double lp=q<=mu ? special::kNegInf : std::log(q-mu)-std::log(sigma)-0.22579135264472743236;
    return special::tail_probability(special::tails_from_log(lp,true),lower_tail,log_p);
  }
  if (z<1.0) {
    const double lp=std::log(std::erf(z/std::sqrt(2.0)));
    return special::tail_probability(special::tails_from_log(lp,true),lower_tail,log_p);
  }
  return special::tail_probability(special::tails_from_log(special::normal_log_upper(z)+std::log(2.0),false),lower_tail,log_p);
}
inline double qhnorm(double p, double mu, double sigma, bool lower_tail, bool log_p) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma>0)) return special::kNaN;
  const auto t=special::quantile_log_tails(p,lower_tail,log_p);
  if (!t.valid) return special::kNaN;
  if (t.lower < -6.0) {
    const double y=std::exp(t.lower+0.22579135264472743236), y2=y*y;
    const double lz=t.lower+0.22579135264472743236+std::log1p(y2*(1.0/6.0+y2*7.0/120.0));
    return mu+std::exp(std::log(sigma)+lz);
  }
  if (t.lower == special::kNegInf) return mu;
  return std::fma(sigma,special::normal_quantile(t.upper-std::log(2.0),false,true),mu);
}
template <typename Rng>
inline double rhnorm(Rng &rng, double mu, double sigma) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0))
    return cppunivariate::special::kNaN;
  return mu + sigma * std::abs(rng.rnorm());
}
} // namespace distributions
} // namespace cppunivariate
