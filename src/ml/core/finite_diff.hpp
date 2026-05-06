// Central-difference gradient + adapter that lets a Cost without an
// analytic gradient be passed to bfgs_minimize.
//
// Step h_i = sqrt(eps) * max(|theta_i|, 1). Standard rule of thumb for
// central differences in double precision; we don't need anything fancier
// because BFGS tolerates noisy gradients.
//
// `kFiniteDiffMaxDim = 32` caps the on-stack work buffer; all six current
// estimators are at most 4-D, so this is comfortably above what we need.

#pragma once

#include <cmath>

namespace mlcore {

inline constexpr int kFiniteDiffMaxDim = 32;

template <typename Cost>
inline void finite_diff_gradient(Cost &cost, const double *theta, double *g) {
  const int n = cost.dim();
  static constexpr double eps_root = 1.4901161193847656e-08;  // sqrt(eps)
  double t[kFiniteDiffMaxDim];
  for (int i = 0; i < n; ++i) t[i] = theta[i];
  for (int i = 0; i < n; ++i) {
    const double xi = t[i];
    const double h = eps_root * (std::fabs(xi) > 1.0 ? std::fabs(xi) : 1.0);
    t[i] = xi + h;
    const double f_plus = cost(t);
    t[i] = xi - h;
    const double f_minus = cost(t);
    t[i] = xi;
    g[i] = (f_plus - f_minus) / (2.0 * h);
  }
}

// Adapter so a Cost with only operator()/dim() can be passed to bfgs_minimize.
template <typename Cost>
struct finite_diff_cost {
  Cost &inner;
  int dim() const { return inner.dim(); }
  double operator()(const double *theta) const { return inner(theta); }
  void gradient(const double *theta, double *g) const {
    finite_diff_gradient(inner, theta, g);
  }
};

}  // namespace mlcore
