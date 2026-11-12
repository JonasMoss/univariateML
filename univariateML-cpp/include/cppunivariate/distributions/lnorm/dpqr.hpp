// Lognormal distribution: scalar d/p/q/r.
// Parameters: meanlog (mu), sdlog > 0 (sigma); support x > 0.
//   X = exp(Y) where Y ~ N(mu, sigma^2)
//   f(x)    = (1 / (x * sigma * sqrt(2*pi))) * exp(-(log(x) - mu)^2 / (2 sigma^2))
//   F(x)    = pnorm((log(x) - mu) / sigma)
//   F^-1(p) = exp(mu + sigma * qnorm(p))

#pragma once

#include "../../special/constants.hpp"
#include "../norm/dpqr.hpp"
#include "../../special/compensated_log.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dlnorm(double x, double meanlog, double sdlog, bool log) {
  if (!(sdlog > 0.0) || std::isnan(x) || !std::isfinite(meanlog)) {
    return kNaN;
  }
  if (x <= 0.0 || x == special::kPosInf) {
    return log ? kNegInf : 0.0;
  }
  const auto lx=special::compensated_log::logarithm(x);
  const auto residual=special::compensated_log::add(lx,{-meanlog,0});
  const double z=(residual.high+residual.low)/sdlog;
  const double log_density=-.5*z*z-std::log(sdlog)-.91893853320467274178-lx.high;
  return log ? log_density : std::exp(log_density);
}

inline double plnorm(double q, double meanlog, double sdlog, bool lower_tail,
                     bool log_p) {
  if (!(sdlog > 0.0) || !std::isfinite(meanlog)) {
    return kNaN;
  }
  if (q <= 0.0) {
    return log_p ? (lower_tail ? kNegInf : 0.0) : (lower_tail ? 0.0 : 1.0);
  }
  return cppunivariate::distributions::pnorm(std::log(q), meanlog, sdlog, lower_tail,
                                       log_p);
}

inline double qlnorm(double p, double meanlog, double sdlog, bool lower_tail,
                     bool log_p) {
  if (!(sdlog > 0.0) || !std::isfinite(meanlog)) {
    return kNaN;
  }
  return std::exp(cppunivariate::distributions::qnorm(p, meanlog, sdlog, lower_tail,
                                                log_p));
}

template <typename Rng>
inline double rlnorm(Rng &rng, double meanlog, double sdlog) {
  return std::exp(meanlog + sdlog * rng.rnorm());
}

}  // namespace distributions
}  // namespace cppunivariate
