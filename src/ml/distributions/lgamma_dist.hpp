// Loggamma distribution (actuar). Note: this is the *distribution*, not
// log-Gamma the function (mlcore::special::lgamma).
// Parameters: shapelog > 0, ratelog > 0; support x > 1.
//   X has loggamma distribution iff log(X) ~ Gamma(shape=shapelog, rate=ratelog).
// Implementation: delegate to mlcore::special::dgamma/pgamma/qgamma on log(x).

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dlgamma(double x, double shapelog, double ratelog, bool log) {
  if (!(shapelog > 0.0 && ratelog > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < 1.0 || !std::isfinite(x)) {
    return log ? kNegInf : 0.0;
  }
  // f_lgamma(x) = f_gamma(log(x); shapelog, scale=1/ratelog) / x
  const double log_density = mlcore::special::dgamma(
                                 std::log(x), shapelog, 1.0 / ratelog,
                                 /*log=*/true) -
                             std::log(x);
  return log ? log_density : std::exp(log_density);
}

inline double plgamma(double q, double shapelog, double ratelog,
                      bool lower_tail, bool log_p) {
  if (!(shapelog > 0.0 && ratelog > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  if (q <= 1.0) {
    const double prob_lt = 0.0;
    const double prob = lower_tail ? prob_lt : 1.0 - prob_lt;
    return log_p ? std::log(prob) : prob;
  }
  return mlcore::special::pgamma(std::log(q), shapelog, 1.0 / ratelog,
                                 lower_tail, log_p);
}

inline double qlgamma(double p, double shapelog, double ratelog,
                      bool lower_tail, bool log_p) {
  if (!(shapelog > 0.0 && ratelog > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  return std::exp(mlcore::special::qgamma(p, shapelog, 1.0 / ratelog,
                                          lower_tail, log_p));
}

template <typename Rng>
inline double rlgamma(Rng &rng, double shapelog, double ratelog) {
  return std::exp(rng.rgamma(shapelog, 1.0 / ratelog));
}

} // namespace distributions
} // namespace mlcore
