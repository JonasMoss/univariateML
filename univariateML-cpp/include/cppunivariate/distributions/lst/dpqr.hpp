#pragma once
#include "../../special/special.hpp"
#include "tails.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline double dlst(double x, double mu, double sigma, double df, bool log) {
  if (!(sigma > 0.0 && df > 0.0 && std::isfinite(sigma) && std::isfinite(df)) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  const double l = student_detail::logdensity_log(special::log_abs_difference(x,mu)-std::log(sigma),df)-std::log(sigma);
  return log ? l : std::exp(l);
}
inline double plst(double q, double mu, double sigma, double df, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0 && df > 0.0 && std::isfinite(sigma) && std::isfinite(df)) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return student_detail::probability_log(special::log_abs_difference(q,mu)-std::log(sigma),q<mu,df,lower_tail,log_p);
}
inline double qlst(double p, double mu, double sigma, double df, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0 && df > 0.0 && std::isfinite(sigma) && std::isfinite(df)) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return mu+student_detail::quantile(p,df,lower_tail,log_p,std::log(sigma));
}
template <typename Rng>
inline double rlst(Rng &rng, double mu, double sigma, double df) {
  if (!(sigma > 0.0 && df > 0.0 && std::isfinite(sigma) && std::isfinite(df)) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return mu+sigma*rng.rt(df);
}
}
}
