// Multi-dimensional BFGS quasi-Newton minimiser.
//
// Cost concept (compile-time, not runtime):
//   int    dim() const
//   double operator()(const double *theta) const
//   void   gradient(const double *theta, double *g) const
//
// A future explicit-Hessian Newton variant (`newton_md_minimize`) will
// additionally read `void hessian(const double *theta, double *H) const`,
// so the Cost interface is designed to extend without breaking call sites.
//
// Inverse-Hessian approximation H is stored row-major as `dim*dim`. Initialised
// to identity; updated each accepted step by the standard two-rank BFGS rule on
// H directly, so the next search direction is one matvec p = -H g.
//
// Line search: backtracking Armijo only (no curvature condition). On
// `max_ls` failure, restart H = I once and retry the same iteration; second
// failure returns LineSearchFailure.
//
// Convergence (any one fires):
//   ||g||_2                 < grad_tol
//   ||theta_new - theta||_inf < step_tol * (||theta||_inf + step_tol)
//   (f_old - f_new)         < reltol  * (|f_old|       + reltol)
//
// NaN bailout matches core/newton.hpp: any non-finite f or theta returns a
// failure status rather than continuing.

#pragma once

#include <cmath>
#include <cstddef>
#include <limits>
#include <utility>
#include <vector>

namespace mlcore {

enum class BfgsStatus { Ok = 0, IterationFailure, LineSearchFailure, BadInit };

struct BfgsResult {
  std::vector<double> theta;
  double f;
  std::vector<double> grad;
  double grad_norm;
  int iterations;
  int fevals;
  BfgsStatus status;
};

struct BfgsControl {
  // reltol is a "function is genuinely flat" backstop, not a primary
  // stopping rule: with the standard kFitTol = 1.22e-4 this would fire on
  // the asymptotically slow approach to an MLE long before the gradient
  // is actually small (e.g. ||grad|| ~ 0.1 on a 500-obs likelihood).
  // grad_tol is the meaningful convergence test; estimators that need to
  // satisfy a score-residual check should map their user-facing `tol`
  // onto grad_tol, not reltol.
  double reltol = 1e-12;
  double grad_tol = 1e-6;
  double step_tol = 1e-8;
  int maxit = 200;
  int max_ls = 20;
  double ls_armijo = 1e-4;
  double ls_shrink = 0.5;
};

namespace detail {

inline double inf_norm(const double *v, int n) {
  double m = 0.0;
  for (int i = 0; i < n; ++i) {
    const double a = std::fabs(v[i]);
    if (a > m) m = a;
  }
  return m;
}

inline double l2_norm(const double *v, int n) {
  double s = 0.0;
  for (int i = 0; i < n; ++i) s += v[i] * v[i];
  return std::sqrt(s);
}

inline void set_identity(double *H, int n) {
  for (int i = 0; i < n * n; ++i) H[i] = 0.0;
  for (int i = 0; i < n; ++i) H[i * n + i] = 1.0;
}

inline bool is_finite_vec(const double *v, int n) {
  for (int i = 0; i < n; ++i) {
    if (!std::isfinite(v[i])) return false;
  }
  return true;
}

}  // namespace detail

template <typename Cost>
inline BfgsResult bfgs_minimize(Cost &cost, const double *theta0,
                                BfgsControl ctl = {}) {
  const int n = cost.dim();
  std::vector<double> theta(theta0, theta0 + n);
  std::vector<double> theta_new(n);
  std::vector<double> grad(n);
  std::vector<double> grad_new(n);
  std::vector<double> p(n);
  std::vector<double> s(n);
  std::vector<double> y(n);
  std::vector<double> Hy(n);
  std::vector<double> H(static_cast<std::size_t>(n) * n, 0.0);
  detail::set_identity(H.data(), n);

  double f = cost(theta.data());
  int fevals = 1;
  if (!std::isfinite(f) || !detail::is_finite_vec(theta.data(), n)) {
    return {std::move(theta), f, std::move(grad),
            std::numeric_limits<double>::quiet_NaN(), 0, fevals,
            BfgsStatus::BadInit};
  }
  cost.gradient(theta.data(), grad.data());
  if (!detail::is_finite_vec(grad.data(), n)) {
    return {std::move(theta), f, std::move(grad),
            std::numeric_limits<double>::quiet_NaN(), 0, fevals,
            BfgsStatus::BadInit};
  }

  for (int iter = 1; iter <= ctl.maxit; ++iter) {
    bool reset_attempted = false;

    while (true) {
      // p = -H g
      for (int i = 0; i < n; ++i) {
        double sum = 0.0;
        const double *Hi = &H[static_cast<std::size_t>(i) * n];
        for (int j = 0; j < n; ++j) sum += Hi[j] * grad[j];
        p[i] = -sum;
      }
      double slope = 0.0;
      for (int i = 0; i < n; ++i) slope += grad[i] * p[i];

      if (!(slope < 0.0)) {
        // Non-descent direction (e.g. H drifted indefinite). Reset and retry.
        if (!reset_attempted) {
          detail::set_identity(H.data(), n);
          reset_attempted = true;
          continue;
        }
        return {std::move(theta), f, std::move(grad),
                detail::l2_norm(grad.data(), n), iter, fevals,
                BfgsStatus::LineSearchFailure};
      }

      double t = 1.0;
      double f_new = 0.0;
      bool accepted = false;
      for (int ls = 0; ls < ctl.max_ls; ++ls) {
        for (int i = 0; i < n; ++i) theta_new[i] = theta[i] + t * p[i];
        f_new = cost(theta_new.data());
        ++fevals;
        if (std::isfinite(f_new) &&
            f_new <= f + ctl.ls_armijo * t * slope) {
          accepted = true;
          break;
        }
        t *= ctl.ls_shrink;
      }

      if (!accepted) {
        if (!reset_attempted) {
          detail::set_identity(H.data(), n);
          reset_attempted = true;
          continue;
        }
        return {std::move(theta), f, std::move(grad),
                detail::l2_norm(grad.data(), n), iter, fevals,
                BfgsStatus::LineSearchFailure};
      }

      cost.gradient(theta_new.data(), grad_new.data());
      if (!detail::is_finite_vec(grad_new.data(), n)) {
        return {std::move(theta), f, std::move(grad),
                detail::l2_norm(grad.data(), n), iter, fevals,
                BfgsStatus::IterationFailure};
      }

      double max_step = 0.0;
      double max_old = 0.0;
      double sy = 0.0;
      for (int i = 0; i < n; ++i) {
        s[i] = theta_new[i] - theta[i];
        y[i] = grad_new[i] - grad[i];
        sy += s[i] * y[i];
        const double as = std::fabs(s[i]);
        const double ao = std::fabs(theta[i]);
        if (as > max_step) max_step = as;
        if (ao > max_old) max_old = ao;
      }

      // Two-rank BFGS update on H.
      // H <- (I - rho s y^T) H (I - rho y s^T) + rho s s^T
      // expanded: H += rho*(rho*yHy + 1) s s^T - rho*(s Hy^T + Hy s^T)
      if (sy > 1e-12) {
        const double rho = 1.0 / sy;
        for (int i = 0; i < n; ++i) {
          double sum = 0.0;
          const double *Hi = &H[static_cast<std::size_t>(i) * n];
          for (int j = 0; j < n; ++j) sum += Hi[j] * y[j];
          Hy[i] = sum;
        }
        double yHy = 0.0;
        for (int i = 0; i < n; ++i) yHy += y[i] * Hy[i];
        const double coeff = rho * (rho * yHy + 1.0);
        for (int i = 0; i < n; ++i) {
          double *Hi = &H[static_cast<std::size_t>(i) * n];
          const double si = s[i];
          const double Hyi = Hy[i];
          for (int j = 0; j < n; ++j) {
            Hi[j] += coeff * si * s[j] - rho * (si * Hy[j] + Hyi * s[j]);
          }
        }
      }

      const double f_old = f;
      theta.swap(theta_new);
      grad.swap(grad_new);
      f = f_new;

      const double gnorm = detail::l2_norm(grad.data(), n);
      const bool conv_grad = gnorm < ctl.grad_tol;
      const bool conv_step =
          max_step < ctl.step_tol * (max_old + ctl.step_tol);
      const bool conv_f =
          (f_old - f) < ctl.reltol * (std::fabs(f_old) + ctl.reltol);
      if (conv_grad || conv_step || conv_f) {
        return {std::move(theta), f, std::move(grad), gnorm, iter, fevals,
                BfgsStatus::Ok};
      }
      break;  // proceed to next iteration
    }
  }

  const double gnorm = detail::l2_norm(grad.data(), n);
  return {std::move(theta), f, std::move(grad), gnorm, ctl.maxit, fevals,
          BfgsStatus::IterationFailure};
}

}  // namespace mlcore
