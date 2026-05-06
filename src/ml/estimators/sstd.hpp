// Skew Student-t MLE on (mu, sigma, nu, xi).
//
// Reparam phi = (mu, log sigma, log(nu - 2), log xi). Pure-libm log-density
// via sstd_sigma + log_dsstd_core; sstd's standardised core is the
// lgamma-based log_dstd_unit, so the standalone fit doesn't pull in Rmath.
//
// f(x; mu, sigma, nu, xi) = (sigma_u / sigma) * f_core(z; nu, xi),
//   where (sigma_u, mu_u) = sstd_sigma(nu, xi),
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

enum class SstdStatus { Ok = 0, BfgsFailure, InvalidStart };

struct SstdResult {
  double mu;
  double sigma;
  double nu;
  double xi;
  double logLik;
  int iterations;
  SstdStatus status;
};

struct SstdCost {
  span x;
  int dim() const { return 4; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = mlcore::from_log(phi[1]);
    const double nu = mlcore::from_log_shift(phi[2], 2.0);
    const double xi = mlcore::from_log(phi[3]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || !std::isfinite(xi) ||
        sigma <= 0.0 || nu <= 2.0 || xi <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    double mu_u = 0.0;
    const double sigma_u = mlcore::dens::sstd_sigma(nu, xi, mu_u);
    const double log_factor = std::log(sigma_u) - std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma * sigma_u + mu_u;
      neg -= log_factor + mlcore::dens::log_dsstd_core(z, nu, xi);
    }
    return neg;
  }
};

inline SstdResult sstd_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2)
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::InvalidStart};

  const double mean_x = mlcore::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, kNaN, 0, SstdStatus::InvalidStart};
  }

  SstdCost cost{x};
  finite_diff_cost<SstdCost> fd{cost};
  const double phi0[4] = {mean_x, mlcore::to_log(sd_x),
                          mlcore::to_log_shift(6.0, 2.0), mlcore::to_log(1.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = mlcore::from_log(r.theta[1]);
  const double nu = mlcore::from_log_shift(r.theta[2], 2.0);
  const double xi = mlcore::from_log(r.theta[3]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, xi, logLik, r.iterations, SstdStatus::BfgsFailure};
  }
  return {mu, sigma, nu, xi, logLik, r.iterations, SstdStatus::Ok};
}

}  // namespace mlcore
