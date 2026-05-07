// Nakagami distribution: scalar d/p/q/r.
//
// Parameterised by (shape, scale) where shape > 0, scale > 0. Internally
// uses the relation: if X ~ Nakagami(shape, scale) then X^2 ~ Gamma(shape,
// scale/shape) (R's shape/scale Gamma parameterisation).

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dnaka(double x, double shape, double scale, bool log) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(x) || x < 0.0) {
    return kNaN;
  }
  const double gamma_scale = scale / shape;
  const double x2 = x * x;
  if (log) {
    return std::log(x) + std::log(2.0) +
           mlcore::special::dgamma(x2, shape, gamma_scale, /*log=*/true);
  }
  return 2.0 * x *
         mlcore::special::dgamma(x2, shape, gamma_scale, /*log=*/false);
}

inline double pnaka(double q, double shape, double scale, bool lower_tail,
                    bool log_p) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(q) || q < 0.0) {
    return kNaN;
  }
  const double gamma_scale = scale / shape;
  return mlcore::special::pgamma(q * q, shape, gamma_scale, lower_tail, log_p);
}

inline double qnaka(double p, double shape, double scale, bool lower_tail,
                    bool log_p) {
  if (!(shape > 0.0 && scale > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  const double gamma_scale = scale / shape;
  const double gq =
      mlcore::special::qgamma(p, shape, gamma_scale, lower_tail, log_p);
  return std::sqrt(gq);
}

template <typename Rng>
inline double rnaka(Rng &rng, double shape, double scale) {
  const double gamma_scale = scale / shape;
  return std::sqrt(rng.rgamma(shape, gamma_scale));
}

} // namespace distributions
} // namespace mlcore
