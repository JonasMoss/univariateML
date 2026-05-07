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
#include <cstddef>

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

// Central-difference Hessian. Diagonal: 3-point second derivative
//   H[i,i] = (f(x+h e_i) - 2 f(x) + f(x-h e_i)) / h^2.
// Off-diagonal: 4-point cross derivative
//   H[i,j] = (f(x+h_i e_i+h_j e_j) - f(x+h_i e_i-h_j e_j)
//             - f(x-h_i e_i+h_j e_j) + f(x-h_i e_i-h_j e_j)) / (4 h_i h_j).
// Step h_i = eps^(1/3) * max(|theta_i|, 1) — second-derivative noise minimised
// at h ~ eps^(1/3) under double precision (Press et al. NR §5.7).
// Output is row-major dim*dim, exactly symmetric (the transpose mirror is
// applied at the end so any FP asymmetry in the stencil is averaged away).
template <typename Cost>
inline void finite_diff_hessian(Cost &cost, const double *theta, double *H) {
  const int n = cost.dim();
  static constexpr double eps_cbrt = 6.0554544523933430e-06;  // eps^(1/3)
  double t[kFiniteDiffMaxDim];
  for (int i = 0; i < n; ++i) t[i] = theta[i];
  const double f0 = cost(t);

  double h[kFiniteDiffMaxDim];
  for (int i = 0; i < n; ++i) {
    const double xi = std::fabs(theta[i]);
    h[i] = eps_cbrt * (xi > 1.0 ? xi : 1.0);
  }

  // Diagonal entries.
  for (int i = 0; i < n; ++i) {
    const double xi = t[i];
    const double hi = h[i];
    t[i] = xi + hi;
    const double fp = cost(t);
    t[i] = xi - hi;
    const double fm = cost(t);
    t[i] = xi;
    H[static_cast<std::size_t>(i) * n + i] = (fp - 2.0 * f0 + fm) / (hi * hi);
  }

  // Off-diagonal entries (upper triangle, then mirrored).
  for (int i = 0; i < n; ++i) {
    for (int j = i + 1; j < n; ++j) {
      const double xi = t[i];
      const double xj = t[j];
      const double hi = h[i];
      const double hj = h[j];
      t[i] = xi + hi; t[j] = xj + hj;
      const double fpp = cost(t);
      t[j] = xj - hj;
      const double fpm = cost(t);
      t[i] = xi - hi; t[j] = xj + hj;
      const double fmp = cost(t);
      t[j] = xj - hj;
      const double fmm = cost(t);
      t[i] = xi; t[j] = xj;
      const double v = (fpp - fpm - fmp + fmm) / (4.0 * hi * hj);
      H[static_cast<std::size_t>(i) * n + j] = v;
      H[static_cast<std::size_t>(j) * n + i] = v;
    }
  }
}

// Adapter that synthesises both gradient() and hessian() via FD.
template <typename Cost>
struct finite_diff_hessian_cost {
  Cost &inner;
  int dim() const { return inner.dim(); }
  double operator()(const double *theta) const { return inner(theta); }
  void gradient(const double *theta, double *g) const {
    finite_diff_gradient(inner, theta, g);
  }
  void hessian(const double *theta, double *H) const {
    finite_diff_hessian(inner, theta, H);
  }
};

}  // namespace mlcore
