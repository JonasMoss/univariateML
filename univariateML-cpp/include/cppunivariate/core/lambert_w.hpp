// Real Lambert W branches: W_0 for Zero-Inflated Poisson and W_{-1}
// for Logarithmic-Series MLEs. Fritsch iteration with relative convergence;
// series approximation near 0 on the principal branch.

#pragma once

#include <cmath>

namespace cppunivariate {

// Shared refinement; the initial value selects the real branch.
inline double lambertw_refine(double z, double w, double tol, int maxit) {
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

// ZIP needs W_0 in (-1, 0), while logarithmic-series needs W_{-1}.
inline double lambertw0(double z, double tol = 1e-12, int maxit = 30) {
  if (z == 0.0) return 0.0;
  const double w = std::abs(z) < 1e-3
                       ? z - z * z + 1.5 * z * z * z
                       : (z < 0.0 ? z : std::log1p(z));
  return lambertw_refine(z, w, tol, maxit);
}

inline double lambertwm1(double z, double tol = 1e-12, int maxit = 30) {
  return lambertw_refine(z, std::log(-z), tol, maxit);
}

}  // namespace cppunivariate
