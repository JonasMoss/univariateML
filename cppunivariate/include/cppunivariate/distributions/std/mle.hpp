// Student-t MLE on (mu, sigma, nu).
//
// Reparam phi = (mu, log sigma, log(nu - 2)) — keeps sigma > 0 and nu > 2
// without bounds. Pure-libm log-density via log_dstd_unit (lgamma-based),
// independent of the math backend.
//
// Density at (mu, sigma, nu): f(x) = (1/sigma) * f_unit((x-mu)/sigma; nu),
// where f_unit is the standardised t (mean 0, sd 1) used by dstd().

#pragma once

#include "../../core/bfgs.hpp"
#include "../../core/finite_diff.hpp"
#include "../../core/reductions.hpp"
#include "../../core/reparam.hpp"
#include "../../core/span.hpp"
#include "../../special/skewed_densities.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class StudentTStatus { Ok = 0, BfgsFailure, InvalidStart };

struct StudentTResult {
  double mu;
  double sigma;
  double nu;
  double logLik;
  int iterations;
  StudentTStatus status;
};

// Per-obs nll in phi-space (with z = (x - mu)/sigma, m = nu - 2,
// y = z^2/m, R = 1 + y, sigma = exp(phi[1]), m = exp(phi[2])):
//   nll_per = phi[1] - lgamma((nu+1)/2) + lgamma(nu/2) + (1/2) log(pi m)
//             + (nu+1)/2 * log(R).
// Hessian summed over observations:
//   H[0][0] = (nu+1)/(sigma^2 m) * sum (1 - y_i) / R_i^2
//   H[0][1] = (2(nu+1)/(sigma m)) * sum z_i / R_i^2
//   H[0][2] = (1/(sigma m)) * sum (3 - z_i^2) z_i / R_i^2
//   H[1][1] = 2(nu+1) * sum y_i / R_i^2
//   H[1][2] = (1/m) * sum z_i^2 (3 - z_i^2) / R_i^2
//   H[2][2] = (nu+1)/2 * sum y_i / R_i^2 - sum z_i^2 / R_i
//             + (m/2) * sum log R_i
//             - n * m^2/4 * (trigamma((nu+1)/2) - trigamma(nu/2))
//             - n * m/2 * (digamma((nu+1)/2) - digamma(nu/2)).
// Sympy-derived; verified vs numerical Hessian to machine precision.
struct StudentTCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double nu = cppunivariate::from_log_shift(phi[2], 2.0);
    if (!std::isfinite(sigma) || !std::isfinite(nu) || sigma <= 0.0 ||
        nu <= 2.0) {
      return std::numeric_limits<double>::infinity();
    }
    const double log_sigma = std::log(sigma);
    double neg = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      neg += log_sigma - cppunivariate::dens::log_dstd_unit(z, nu);
    }
    return neg;
  }
  void hessian(const double *phi, double *H) const {
    const double mu = phi[0];
    const double sigma = cppunivariate::from_log(phi[1]);
    const double m = cppunivariate::from_log(phi[2]);  // nu - 2
    const double nu = m + 2.0;
    if (!std::isfinite(sigma) || !std::isfinite(m) || sigma <= 0.0 ||
        m <= 0.0) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      for (int i = 0; i < 9; ++i) H[i] = nan;
      return;
    }
    double S_invR2 = 0.0;       // sum 1/R^2
    double S_y_invR2 = 0.0;     // sum y / R^2
    double S_z_invR2 = 0.0;     // sum z / R^2
    double S_z2_invR = 0.0;     // sum z^2 / R
    double S_z3_invR2 = 0.0;    // sum z^3 / R^2
    double S_z2_invR2 = 0.0;    // sum z^2 / R^2
    double S_z4_invR2 = 0.0;    // sum z^4 / R^2
    double S_logR = 0.0;        // sum log R
    for (auto v : x) {
      const double z = (v - mu) / sigma;
      const double z2 = z * z;
      const double y = z2 / m;
      const double R = 1.0 + y;
      const double inv_R = 1.0 / R;
      const double inv_R2 = inv_R * inv_R;
      S_invR2 += inv_R2;
      S_y_invR2 += y * inv_R2;
      S_z_invR2 += z * inv_R2;
      S_z2_invR += z2 * inv_R;
      S_z3_invR2 += z * z2 * inv_R2;
      S_z2_invR2 += z2 * inv_R2;
      S_z4_invR2 += z2 * z2 * inv_R2;
      S_logR += std::log(R);
    }
    const double dn = static_cast<double>(x.size());
    const double nu1 = nu + 1.0;
    const double sigma_m = sigma * m;
    const double sigma2_m = sigma * sigma_m;
    // sum (1 - y) / R^2 = sum 1/R^2 - sum y/R^2.
    H[0] = nu1 / sigma2_m * (S_invR2 - S_y_invR2);
    // sum z / R^2.
    H[1] = (2.0 * nu1 / sigma_m) * S_z_invR2;
    // sum (3 - z^2) z / R^2 = 3 sum z/R^2 - sum z^3/R^2.
    H[2] = (3.0 * S_z_invR2 - S_z3_invR2) / sigma_m;
    H[3] = H[1];
    H[4] = 2.0 * nu1 * S_y_invR2;
    // sum z^2 (3 - z^2) / R^2 = 3 sum z^2/R^2 - sum z^4/R^2.
    H[5] = (3.0 * S_z2_invR2 - S_z4_invR2) / m;
    H[6] = H[2];
    H[7] = H[5];
    const double dpsi = cppunivariate::special::digamma(0.5 * nu1) -
                        cppunivariate::special::digamma(0.5 * nu);
    const double dtri = cppunivariate::special::trigamma(0.5 * nu1) -
                        cppunivariate::special::trigamma(0.5 * nu);
    H[8] = 0.5 * nu1 * S_y_invR2 - S_z2_invR + 0.5 * m * S_logR -
           0.25 * m * m * dn * dtri - 0.5 * m * dn * dpsi;
  }
};

inline StudentTResult student_t_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, kNaN, 0, StudentTStatus::InvalidStart};

  const double mean_x = cppunivariate::mean(x);
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
  const double phi0[3] = {mean_x, cppunivariate::to_log(sd_x),
                          cppunivariate::to_log_shift(6.0, 2.0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double mu = r.theta[0];
  const double sigma = cppunivariate::from_log(r.theta[1]);
  const double nu = cppunivariate::from_log_shift(r.theta[2], 2.0);
  const double logLik = -r.f;

  if (r.status != BfgsStatus::Ok) {
    return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::BfgsFailure};
  }
  return {mu, sigma, nu, logLik, r.iterations, StudentTStatus::Ok};
}

}  // namespace cppunivariate
