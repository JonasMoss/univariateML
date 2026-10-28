#pragma once
#include "../../special/special.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
namespace distributions {
inline double dlst(double x, double mu, double sigma, double df, bool log) {
  if (!(sigma > 0.0 && df > 0.0) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  const double l = cppunivariate::special::dt((x-mu)/sigma,df,true)-std::log(sigma);
  return log ? l : std::exp(l);
}
inline double plst(double q, double mu, double sigma, double df, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0 && df > 0.0) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return cppunivariate::special::pt((q-mu)/sigma,df,lower_tail,log_p);
}
inline double qlst(double p, double mu, double sigma, double df, bool lower_tail, bool log_p) {
  if (!(sigma > 0.0 && df > 0.0) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return mu+sigma*cppunivariate::special::qt(p,df,lower_tail,log_p);
}
template <typename Rng>
inline double rlst(Rng &rng, double mu, double sigma, double df) {
  if (!(sigma > 0.0 && df > 0.0) || !std::isfinite(mu))
    return std::numeric_limits<double>::quiet_NaN();
  return mu+sigma*rng.rt(df);
}
}
}
