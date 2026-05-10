// Principal-branch Lambert W (W_0). Used by Zero-Inflated Poisson and
// Logarithmic-Series MLEs to express the closed-form maximiser. Fritsch
// iteration with relative convergence; series approximation near 0.

#pragma once

#include <cmath>

namespace cppunivariate {

inline double lambertw0(double z, double tol = 1e-12, int maxit = 30) {
  if (z == 0.0) return 0.0;
  double w;
  if (std::abs(z) < 1e-3) {
    w = z - z * z + 1.5 * z * z * z;
  } else {
    w = std::log(-z);
  }
  for (int i = 0; i < maxit; ++i) {
    const double e = std::exp(w);
    const double f = w * e - z;
    const double p = w + 1.0;
    const double t = f / (e * p);
    const double step = t * (1.0 + 0.5 * t * (w + 2.0) / p);
    w -= step;
    if (std::abs(step) < (std::abs(w) * tol + tol)) break;
  }
  return w;
}

}  // namespace cppunivariate
