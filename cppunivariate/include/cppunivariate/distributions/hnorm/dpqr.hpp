#pragma once
#include "../../special/special.hpp"
#include <cmath>

namespace cppunivariate {
namespace distributions {
inline double dhalfnorm(double x, double mu, double sigma, bool log) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0) || std::isnan(x))
    return cppunivariate::special::kNaN;
  if (x < mu) return log ? cppunivariate::special::kNegInf : 0.0;
  const double z = (x-mu)/sigma;
  const double ld = 0.5*std::log(2.0/std::acos(-1.0)) - std::log(sigma) - 0.5*z*z;
  return log ? ld : std::exp(ld);
}
inline double phalfnorm(double q, double mu, double sigma, bool lower_tail, bool log_p) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0) || std::isnan(q))
    return cppunivariate::special::kNaN;
  const double z = q <= mu ? 0.0 : (q-mu)/sigma;
  const double p = lower_tail ? std::erf(z/std::sqrt(2.0)) : std::erfc(z/std::sqrt(2.0));
  return log_p ? std::log(p) : p;
}
inline double qhalfnorm(double p, double mu, double sigma, bool lower_tail, bool log_p) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0) || std::isnan(p) ||
      (log_p ? p > 0 : (p < 0 || p > 1))) return cppunivariate::special::kNaN;
  const double upper = lower_tail ? (log_p ? -std::expm1(p) : 1-p) : (log_p ? std::exp(p) : p);
  return mu + sigma * cppunivariate::special::qnorm(upper/2, 0, 1, false, false);
}
template <typename Rng>
inline double rhalfnorm(Rng &rng, double mu, double sigma) {
  if (!std::isfinite(mu) || !std::isfinite(sigma) || !(sigma > 0))
    return cppunivariate::special::kNaN;
  return mu + sigma * std::abs(rng.rnorm());
}
} // namespace distributions
} // namespace cppunivariate
