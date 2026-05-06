// Student-t MLE on (mu, sigma, nu).
//
// Reparam phi = (mu, log sigma, log(nu - 2)) — keeps sigma > 0 and nu > 2
// without bounds. Pure-libm log-density via log_dstd_unit (lgamma-based)
// rather than Rf_dt, so the standalone fit doesn't pull in Rmath.
//
// Density at (mu, sigma, nu): f(x) = (1/sigma) * f_unit((x-mu)/sigma; nu),
// where f_unit is the standardised t (mean 0, sd 1) used by dstd().

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

enum class StudentTStatus { Ok = 0, BfgsFailure, InvalidStart };

struct StudentTResult {
  double mu;
  double sigma;
  double nu;
  double logLik;
  int iterations;
  StudentTStatus status;
};

struct StudentTCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = mlcore::from_log(phi[1]);
    const double nu = mlcore::from_log_shift(phi[2], 2.0);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 2.0) {
      return std::numeric_limits<double>::infinity();
    }
    const double log_sigma = std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      neg += log_sigma - mlcore::dens::log_dstd_unit(z, nu);
    }
    return neg;
  }
};

inline StudentTResult student_t_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::InvalidStart};

  const double mean_x = mlcore::mean(x);
  double var_x = 0.0;
  for (auto v : x) {
    const double d = v - mean_x;
    var_x += d * d;
  }
  var_x /= static_cast<double>(n - 1);
  const double sd_x = std::sqrt(var_x);
  if (!std::isfinite(sd_x) || sd_x <= 0.0) {
    return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::InvalidStart};
  }

  StudentTCost cost{x};
  finite_diff_cost<StudentTCost> fd{cost};
  const double phi0[3] = {mean_x, mlcore::to_log(sd_x),
                          mlcore::to_log_shift(6.0, 2.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = mlcore::from_log(r.theta[1]);
  const double nu = mlcore::from_log_shift(r.theta[2], 2.0);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::BfgsFailure};
  }
  return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::Ok};
}

}  // namespace mlcore
