// Generalized Error Distribution (GED, symmetric) MLE on (mu, sigma, nu).
//
// Reparam phi = (mu, log sigma, log nu). Pure-libm log-density via
// log_dged_unit, so the standalone fit doesn't need Rmath.
//
// Density at (mu, sigma, nu): f(x) = (1/sigma) * f_unit((x-mu)/sigma; nu)
// where f_unit is the standardised GED used by dged().

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

enum class GedStatus { Ok = 0, BfgsFailure, InvalidStart };

struct GedResult {
  double mu;
  double sigma;
  double nu;
  double logLik;
  int iterations;
  GedStatus status;
};

struct GedCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = mlcore::from_log(phi[1]);
    const double nu = mlcore::from_log(phi[2]);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    const double log_sigma = std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      neg += log_sigma - mlcore::dens::log_dged_unit(z, nu);
    }
    return neg;
  }
};

inline GedResult ged_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, GedStatus::InvalidStart};

  const double mean_x = mlcore::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, GedStatus::InvalidStart};
  }

  GedCost cost{x};
  finite_diff_cost<GedCost> fd{cost};
  const double phi0[3] = {mean_x, mlcore::to_log(sd_x), mlcore::to_log(2.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = mlcore::from_log(r.theta[1]);
  const double nu = mlcore::from_log(r.theta[2]);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, logLik, r.iterations, GedStatus::BfgsFailure};
  }
  return {mu, sigma, nu, logLik, r.iterations, GedStatus::Ok};
}

}  // namespace mlcore
