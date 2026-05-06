// Skew-normal MLE on (mu, sigma, xi).
//
// Reparam phi = (mu, log sigma, log xi). Pure-libm log-density; mirrors the
// snorm_sigma + log_dsnorm_core decomposition used by dsnorm() so that the
// fitted-parameter likelihood matches sum(dsnorm(..., log = TRUE)) up to
// the lgamma-vs-Rmath drift documented in skewed_densities.hpp.
//
// f(x; mu, sigma, xi) = (sigma_u / sigma) * f_core(z; xi),
//   where (sigma_u, mu_u) = snorm_sigma(xi),
//   z = (x - mu)/sigma * sigma_u + mu_u.

#pragma once

#include "../core/bfgs.hpp"
#include "../core/finite_diff.hpp"
#include "../core/reductions.hpp"
#include "../core/reparam.hpp"
#include "../core/span.hpp"
#include "../special/skewed_densities.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

enum class SnormStatus { Ok = 0, BfgsFailure, InvalidStart };

struct SnormResult {
  double mu;
  double sigma;
  double xi;
  double logLik;
  int iterations;
  SnormStatus status;
};

struct SnormCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = mlcore::from_log(phi[1]);
    const double xi = mlcore::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(xi) || sigma <= 0.0 ||
        xi <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    double mu_u = 0.0;
    const double sigma_u = mlcore::dens::snorm_sigma(xi, mu_u);
    const double log_factor = std::log(sigma_u) - std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      neg -= log_factor + mlcore::dens::log_dsnorm_core(z, xi);
    }
    return neg;
  }
};

inline SnormResult snorm_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, SnormStatus::InvalidStart};

  const double mean_x = mlcore::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, SnormStatus::InvalidStart};
  }

  SnormCost cost{x};
  finite_diff_cost<SnormCost> fd{cost};
  const double phi0[3] = {mean_x, mlcore::to_log(sd_x), mlcore::to_log(1.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = mlcore::from_log(r.theta[1]);
  const double xi = mlcore::from_log(r.theta[2]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, xi, logLik, r.iterations, SnormStatus::BfgsFailure};
  }
  return {mu, sigma, xi, logLik, r.iterations, SnormStatus::Ok};
}

}  // namespace mlcore
