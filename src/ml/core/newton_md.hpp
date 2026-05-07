// Multi-dimensional Newton minimiser with Levenberg ridge.
//
// Cost concept (compile-time, not runtime):
//   int    dim() const
//   double operator()(const double *theta) const         // f(theta)
//   void   gradient(const double *theta, double *g) const
//   void   hessian (const double *theta, double *H) const // row-major, dim*dim
//
// Cost types without an analytic hessian can be wrapped in
// `finite_diff_hessian_cost<Cost>` (core/finite_diff.hpp) which synthesises
// both gradient and hessian via finite differences.
//
// Search direction: solve (H + lambda*I) p = -g via Cholesky. If the modified
// Hessian is not positive-definite (Cholesky fails) we ramp lambda by
// `ridge_grow` and retry; eventually a large enough lambda gives a gradient
// step. After an accepted step we reset lambda to `ridge0`.
//
// Line search: backtracking Armijo only, mirroring core/bfgs.hpp. On
// `max_ls` failure we ramp lambda once more before declaring failure.
//
// Convergence (any one fires):
//   ||g||_2                 < grad_tol
//   ||theta_new - theta||_inf < step_tol * (||theta||_inf + step_tol)
//   (f_old - f_new)         < reltol  * (|f_old|       + reltol)
//
// NaN bailout: any non-finite f, g, or H returns a failure status.

#pragma once

#include "cholesky.hpp"
#include "finite_diff.hpp"

#include <cmath>
#include <cstddef>
#include <limits>
#include <utility>
#include <vector>

namespace mlcore {

enum class NewtonMdStatus {
  Ok = 0,
  IterationFailure,
  IndefiniteHessian,
  BadInit
};

struct NewtonMdResult {
  std::vector<double> theta;
  double f;
  std::vector<double> grad;
  std::vector<double> hessian;  // last evaluated, row-major dim*dim
  double grad_norm;
  int iterations;
  int fevals;
  NewtonMdStatus status;
};

struct NewtonMdControl {
  double reltol = 1e-12;
  double grad_tol = 1e-6;
  double step_tol = 1e-8;
  int maxit = 100;
  int max_ls = 20;
  double ls_armijo = 1e-4;
  double ls_shrink = 0.5;
  double ridge0 = 1e-8;
  double ridge_grow = 10.0;
  double ridge_max = 1e8;
};

namespace detail {

inline bool finite_vec_md(const double *v, int n) {
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(v[i])) return false;
  }
  return true;
}

inline bool solve_with_ridge(const double *H, int n, double lambda,
                             const double *neg_g, double *p) {
  // Build A = H + lambda*I in a stack workspace, then Cholesky-solve A p = -g.
  constexpr int kMax = kFiniteDiffMaxDim;
  double A[kMax * kMax];
  const std::size_t nn = static_cast<std::size_t>(n) * n;
  for (std::size_t k = 0; k < nn; ++k) A[k] = H[k];
  for (int i = 0; i < n; ++i) {
    A[static_cast<std::size_t>(i) * n + i] += lambda;
  }
  if (!cholesky_factor(A, n)) return false;
  for (int i = 0; i < n; ++i) p[i] = neg_g[i];
  cholesky_solve(A, p, n);
  return true;
}

}  // namespace detail

template <typename Cost>
inline NewtonMdResult newton_md_minimize(Cost &cost, const double *theta0,
                                         NewtonMdControl ctl = {}) {
  const int n = cost.dim();
  std::vector<double> theta(theta0, theta0 + n);
  std::vector<double> theta_new(n);
  std::vector<double> grad(n);
  std::vector<double> grad_new(n);
  std::vector<double> p(n);
  std::vector<double> neg_g(n);
  std::vector<double> H(static_cast<std::size_t>(n) * n, 0.0);

  double f = cost(theta.data());
  int fevals = 1;
  if (!std::isfinite(f) || !detail::finite_vec_md(theta.data(), n)) {
    return {std::move(theta), f, std::move(grad), std::move(H),
            std::numeric_limits<double>::quiet_NaN(), 0, fevals,
            NewtonMdStatus::BadInit};
  }
  cost.gradient(theta.data(), grad.data());
  if (!detail::finite_vec_md(grad.data(), n)) {
    return {std::move(theta), f, std::move(grad), std::move(H),
            std::numeric_limits<double>::quiet_NaN(), 0, fevals,
            NewtonMdStatus::BadInit};
  }
  cost.hessian(theta.data(), H.data());
  if (!detail::finite_vec_md(H.data(), n * n)) {
    return {std::move(theta), f, std::move(grad), std::move(H),
            std::numeric_limits<double>::quiet_NaN(), 0, fevals,
            NewtonMdStatus::BadInit};
  }

  double lambda = ctl.ridge0;

  for (int iter = 1; iter <= ctl.maxit; ++iter) {
    for (int i = 0; i < n; ++i) neg_g[i] = -grad[i];

    // Find a descent direction by ramping the ridge until Cholesky succeeds
    // and the resulting step is genuinely descent.
    bool found = false;
    while (lambda <= ctl.ridge_max) {
      if (detail::solve_with_ridge(H.data(), n, lambda, neg_g.data(),
                                   p.data())) {
        double slope = 0.0;
        for (int i = 0; i < n; ++i) slope += grad[i] * p[i];
        if (slope < 0.0) {
          found = true;
          break;
        }
      }
      lambda *= ctl.ridge_grow;
    }
    if (!found) {
      double gnorm = 0.0;
      for (int i = 0; i < n; ++i) gnorm += grad[i] * grad[i];
      return {std::move(theta), f, std::move(grad), std::move(H),
              std::sqrt(gnorm), iter, fevals,
              NewtonMdStatus::IndefiniteHessian};
    }

    double slope = 0.0;
    for (int i = 0; i < n; ++i) slope += grad[i] * p[i];

    double t = 1.0;
    double f_new = 0.0;
    bool accepted = false;
    for (int ls = 0; ls < ctl.max_ls; ++ls) {
      for (int i = 0; i < n; ++i) theta_new[i] = theta[i] + t * p[i];
      f_new = cost(theta_new.data());
      ++fevals;
      if (std::isfinite(f_new) && f_new <= f + ctl.ls_armijo * t * slope) {
        accepted = true;
        break;
      }
      t *= ctl.ls_shrink;
    }
    if (!accepted) {
      // Bump the ridge and let the next iteration retry from the same theta.
      lambda *= ctl.ridge_grow;
      if (lambda > ctl.ridge_max) {
        double gnorm = 0.0;
        for (int i = 0; i < n; ++i) gnorm += grad[i] * grad[i];
        return {std::move(theta), f, std::move(grad), std::move(H),
                std::sqrt(gnorm), iter, fevals,
                NewtonMdStatus::IterationFailure};
      }
      continue;
    }

    cost.gradient(theta_new.data(), grad_new.data());
    if (!detail::finite_vec_md(grad_new.data(), n)) {
      double gnorm = 0.0;
      for (int i = 0; i < n; ++i) gnorm += grad[i] * grad[i];
      return {std::move(theta), f, std::move(grad), std::move(H),
              std::sqrt(gnorm), iter, fevals,
              NewtonMdStatus::IterationFailure};
    }

    double max_step = 0.0;
    double max_old = 0.0;
    for (int i = 0; i < n; ++i) {
      const double as = std::fabs(theta_new[i] - theta[i]);
      const double ao = std::fabs(theta[i]);
      if (as > max_step) max_step = as;
      if (ao > max_old) max_old = ao;
    }

    const double f_old = f;
    theta.swap(theta_new);
    grad.swap(grad_new);
    f = f_new;
    cost.hessian(theta.data(), H.data());
    if (!detail::finite_vec_md(H.data(), n * n)) {
      double gnorm = 0.0;
      for (int i = 0; i < n; ++i) gnorm += grad[i] * grad[i];
      return {std::move(theta), f, std::move(grad), std::move(H),
              std::sqrt(gnorm), iter, fevals,
              NewtonMdStatus::IterationFailure};
    }
    lambda = ctl.ridge0;

    double gnorm_sq = 0.0;
    for (int i = 0; i < n; ++i) gnorm_sq += grad[i] * grad[i];
    const double gnorm = std::sqrt(gnorm_sq);
    const bool conv_grad = gnorm < ctl.grad_tol;
    const bool conv_step =
        max_step < ctl.step_tol * (max_old + ctl.step_tol);
    const bool conv_f =
        (f_old - f) < ctl.reltol * (std::fabs(f_old) + ctl.reltol);
    if (conv_grad || conv_step || conv_f) {
      return {std::move(theta), f, std::move(grad), std::move(H), gnorm, iter,
              fevals, NewtonMdStatus::Ok};
    }
  }

  double gnorm_sq = 0.0;
  for (int i = 0; i < n; ++i) gnorm_sq += grad[i] * grad[i];
  return {std::move(theta), f, std::move(grad), std::move(H),
          std::sqrt(gnorm_sq), ctl.maxit, fevals,
          NewtonMdStatus::IterationFailure};
}

}  // namespace mlcore
