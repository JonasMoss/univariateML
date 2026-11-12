// Loggamma distribution (actuar). Note: this is the *distribution*, not
// log-Gamma the function (cppunivariate::special::lgamma).
// Parameters: shapelog > 0, ratelog > 0; support x > 1.
//   X has loggamma distribution iff log(X) ~ Gamma(shape=shapelog, rate=ratelog).
// Evaluate the gamma family on log(x), retaining its log tails.

#pragma once

#include "../../special/constants.hpp"
#include "../gamma/dpqr.hpp"
#include "../../special/compensated_log.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dlgamma(double x, double shapelog, double ratelog, bool log) {
  if (!(shapelog > 0.0 && ratelog > 0.0) || std::isnan(x)) {
    return kNaN;
  }
  if (x < 1.0 || !std::isfinite(x)) {
    return log ? kNegInf : 0.0;
  }
  if(x==1) return dgamma(0,shapelog,1/ratelog,log);
  const auto lx=special::compensated_log::logarithm(x);
  double log_density;
  if(shapelog>=16) {
    const auto product=special::compensated_log::multiply(lx,{ratelog,0});
    const auto displacement=special::compensated_log::add(product,{-shapelog,0});
    const double u=(displacement.high+displacement.low)/shapelog;
    if(std::abs(u)>.5) log_density=dgamma(lx.high,shapelog,1/ratelog,true)-lx.high;
    else log_density=.5*std::log(shapelog)-std::log(lx.high)-.91893853320467274178
      -special::density_stirling_remainder(shapelog)-special::scaled_positive_deviance(shapelog,u)-lx.high;
  } else log_density=dgamma(lx.high,shapelog,1/ratelog,true)-lx.high;
  return log ? log_density : std::exp(log_density);
}

inline double plgamma(double q, double shapelog, double ratelog,
                      bool lower_tail, bool log_p) {
  return cppunivariate::distributions::pgamma(q<=1.0 ? 0.0 : std::log(q),shapelog,1.0/ratelog,lower_tail,log_p);
}

inline double qlgamma(double p, double shapelog, double ratelog,
                      bool lower_tail, bool log_p) {
  return std::exp(cppunivariate::distributions::qgamma(p,shapelog,1.0/ratelog,lower_tail,log_p));
}

template <typename Rng>
inline double rlgamma(Rng &rng, double shapelog, double ratelog) {
  return std::exp(rng.rgamma(shapelog, 1.0 / ratelog));
}

} // namespace distributions
} // namespace cppunivariate
