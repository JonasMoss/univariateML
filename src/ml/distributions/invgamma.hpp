// Inverse-Gamma distribution (extraDistr R-level parameterisation).
// Parameters: alpha > 0 (shape), beta > 0 (scale).
//   f(x)   = beta^alpha / Gamma(alpha) * x^(-alpha-1) * exp(-beta/x)
//   F(x)   = upper-tail of Gamma(alpha, scale=beta) at 1/x
// Sample: 1/Y for Y ~ Gamma(shape=alpha, scale=1/beta).

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dinvgamma(double x, double alpha, double beta, bool log) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0) {
    return log ? kNegInf : 0.0;
  }
  const double log_density = alpha * std::log(beta) - std::lgamma(alpha) +
                             (-alpha - 1.0) * std::log(x) - beta / x;
  return log ? log_density : std::exp(log_density);
}

inline double pinvgamma(double q, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= 0.0) {
    prob = 0.0;
  } else {
    // F_invgamma(x; alpha, scale=beta) = pgamma(1/x; alpha, scale=1/beta,
    //                                            lower_tail=false)
    prob = mlcore::special::pgamma(1.0 / q, alpha, 1.0 / beta,
                                   /*lower_tail=*/false,
                                   /*log_p=*/false);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qinvgamma(double p, double alpha, double beta, bool lower_tail,
                        bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  // Invert: F(x) = 1 - pgamma(1/x; alpha, scale=1/beta) = p
  // -> 1/x = qgamma(1-p; alpha, scale=1/beta)
  const double y = mlcore::special::qgamma(1.0 - p, alpha, 1.0 / beta,
                                           /*lower_tail=*/true,
                                           /*log_p=*/false);
  return 1.0 / y;
}

template <typename Rng>
inline double rinvgamma(Rng &rng, double alpha, double beta) {
  // X ~ InvGamma(shape=alpha, scale=beta) <=> 1/X ~ Gamma(shape=alpha,
  // scale=1/beta).
  return 1.0 / rng.rgamma(alpha, 1.0 / beta);
}

} // namespace distributions
} // namespace mlcore
