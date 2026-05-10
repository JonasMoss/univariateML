// Inverse digamma: solve digamma(y) = x for y.
//
// Asymptotic expansion for very negative x; small Newton refinement
// otherwise. Up to four polishing Newton steps using Rmath digamma/
// trigamma. Used by Beta MLE (implicit) and Kumaraswamy MLE (explicit).

#pragma once

#include "../special/special.hpp"

#include <cmath>

namespace cppunivariate {

inline double inverse_digamma_scalar(double x) {
  constexpr double GAMMA_C = 0.57721566490153286060;
  constexpr double ZETA3_C = 1.2020569031595942854;
  constexpr double PI_C = M_PI;
  constexpr double PI_SQ_C = PI_C * PI_C;

  double y, r;
  if (x + GAMMA_C < -30.0) {
    const double t = x + GAMMA_C;
    const double t_inv = 1.0 / t;
    const double t3 = t_inv * t_inv * t_inv;
    y = -t_inv + (PI_SQ_C / 6.0) * t3 + (ZETA3_C / 2.0) * t3 * t_inv;
  } else {
    y = 1.0 / std::log1p(std::exp(-x));
    for (int i = 0; i < 2; ++i) {
      r = cppunivariate::special::digamma(y) - x;
      y -= r / cppunivariate::special::trigamma(y);
    }
  }
  for (int k = 0; k < 2; ++k) {
    r = cppunivariate::special::digamma(y) - x;
    if (std::fabs(r) < 1e-14) break;
    y -= r / cppunivariate::special::trigamma(y);
  }
  return y;
}

}  // namespace cppunivariate
