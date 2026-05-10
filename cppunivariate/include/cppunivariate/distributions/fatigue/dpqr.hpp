// Birnbaum-Saunders (fatigue life) distribution (extraDistr).
// Parameters: alpha > 0 (shape), beta > 0 (scale), mu (location).
//   z      = x - mu, zb = sqrt(z/beta), bz = sqrt(beta/z)
//   f(x)   = (zb + bz) / (2 * alpha * z) * phi((zb - bz)/alpha)
//   F(x)   = Phi((zb - bz)/alpha)
//   F^-1(p)= ( alpha/2*Z_p + sqrt((alpha/2*Z_p)^2 + 1) )^2 * beta + mu
//            with Z_p = qnorm(p)
// Sample: Z ~ N(0, 1) -> X = ( alpha/2*Z + sqrt((alpha/2*Z)^2+1) )^2 * beta + mu

#pragma once

#include "../../special/constants.hpp"
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dfatigue(double x, double alpha, double beta, double mu,
                       bool log) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(x) ||
      !std::isfinite(alpha) || !std::isfinite(beta) || !std::isfinite(mu)) {
    return kNaN;
  }
  if (x <= mu) {
    return log ? kNegInf : 0.0;
  }
  const double z = x - mu;
  const double zb = std::sqrt(z / beta);
  const double bz = std::sqrt(beta / z);
  const double w = (zb - bz) / alpha;
  // log phi(w) = -0.5 * log(2*pi) - 0.5 * w^2
  const double log_phi = -0.5 * std::log(2.0 * M_PI) - 0.5 * w * w;
  const double log_density =
      std::log(zb + bz) - std::log(2.0) - std::log(alpha) - std::log(z) +
      log_phi;
  return log ? log_density : std::exp(log_density);
}

inline double pfatigue(double q, double alpha, double beta, double mu,
                       bool lower_tail, bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= mu) {
    prob = lower_tail ? 0.0 : 1.0;
  } else {
    const double z = q - mu;
    const double zb = std::sqrt(z / beta);
    const double bz = std::sqrt(beta / z);
    prob = cppunivariate::special::pnorm((zb - bz) / alpha, 0.0, 1.0, true, false);
    if (!lower_tail) {
      prob = 1.0 - prob;
    }
  }
  return log_p ? std::log(prob) : prob;
}

inline double qfatigue(double p, double alpha, double beta, double mu,
                       bool lower_tail, bool log_p) {
  if (!(alpha > 0.0 && beta > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p == 0.0) return mu;
  const double Zp =
      cppunivariate::special::qnorm(p, 0.0, 1.0, true, false);
  const double aZ2 = alpha * Zp / 2.0;
  return std::pow(aZ2 + std::sqrt(aZ2 * aZ2 + 1.0), 2.0) * beta + mu;
}

template <typename Rng>
inline double rfatigue(Rng &rng, double alpha, double beta, double mu) {
  const double z = rng.rnorm();
  const double aZ2 = alpha * z / 2.0;
  return std::pow(aZ2 + std::sqrt(aZ2 * aZ2 + 1.0), 2.0) * beta + mu;
}

} // namespace distributions
} // namespace cppunivariate
