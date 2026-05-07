// Standardised Student-t (mean = mean, sd = sd, df = nu > 2).
//
// X ~ std(mean, sd, nu) iff (X - mean)/sd has variance 1 and is t-distributed
// with df = nu rescaled by sqrt(nu/(nu-2)). Requires nu > 2.

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;

inline double dstd(double x, double mean, double sd, double nu, bool log) {
  if (!(sd > 0.0 && nu > 2.0) || !std::isfinite(x)) {
    return kNaN;
  }
  const double scale = std::sqrt(nu / (nu - 2.0));
  const double z = (x - mean) / sd;
  const double logdens = mlcore::special::dt(z * scale, nu, /*log=*/true) +
                         std::log(scale / sd);
  return log ? logdens : std::exp(logdens);
}

inline double pstd(double q, double mean, double sd, double nu,
                   bool lower_tail, bool log_p) {
  if (!(sd > 0.0 && nu > 2.0) || !std::isfinite(q)) {
    return kNaN;
  }
  const double scale = std::sqrt(nu / (nu - 2.0));
  const double z = (q - mean) / sd;
  return mlcore::special::pt(z * scale, nu, lower_tail, log_p);
}

inline double qstd(double p, double mean, double sd, double nu,
                   bool lower_tail, bool log_p) {
  if (!(sd > 0.0 && nu > 2.0) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  const double scale = std::sqrt(nu / (nu - 2.0));
  const double z = mlcore::special::qt(p, nu, /*lower_tail=*/true,
                                       /*log_p=*/false);
  return z / scale * sd + mean;
}

template <typename Rng>
inline double rstd(Rng &rng, double mean, double sd, double nu) {
  const double z = rng.rt(nu);
  const double scale = std::sqrt(nu / (nu - 2.0));
  return z / scale * sd + mean;
}

} // namespace distributions
} // namespace mlcore
