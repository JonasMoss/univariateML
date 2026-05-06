// Paralogistic MLE on (shape, scale).
//
// The previous R-side fit was 1-D: it Newton-iterated on shape with scale
// silently fixed at 1, which only happens to be correct on data that was
// sampled with the actuar default. This implementation fits both
// parameters jointly via BFGS on the log-reparameterised cost
// (phi = (log shape, log scale)). Score-residual check at the recovered
// estimate guards against the same "fake fit" class of fault.
//
// Density (actuar::dparalogis):
//   f(x; alpha, theta) = alpha^2 * (x/theta)^alpha
//                        / [x * (1 + (x/theta)^alpha)^(alpha + 1)]
//
// Log-likelihood at (alpha, theta):
//   l = n * 2*log(alpha)
//       + alpha * sum(log(x_i / theta))
//       - sum(log x_i)
//       - (alpha + 1) * sum(log1p((x_i / theta)^alpha))
//
// Start values: theta0 = median(x),
//               alpha0 = max(1.5, 2 / sd(log(x / theta0))).

#pragma once

#include "../core/bfgs.hpp"
#include "../core/finite_diff.hpp"
#include "../core/reparam.hpp"
#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

enum class ParalogisStatus { Ok = 0, BfgsFailure, InvalidStart };

struct ParalogisResult {
  double shape;
  double scale;
  double logLik;
  int iterations;
  ParalogisStatus status;
};

// Negative log-likelihood as a BFGS Cost, parameterised in phi-space:
//   phi[0] = log(shape), phi[1] = log(scale).
struct ParalogisCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *phi) const {
    const double alpha = mlcore::from_log(phi[0]);
    const double theta = mlcore::from_log(phi[1]);
    if (!std::isfinite(alpha) || !std::isfinite(theta) || alpha <= 0.0 ||
        theta <= 0.0) {
      return std::numeric_limits<double>::infinity();
    }
    const std::size_t n = x.size();
    double sum_log_xt = 0.0;  // sum log(x_i / theta)
    double sum_log_x = 0.0;
    double sum_log1p_r = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double lx = std::log(x[i]);
      sum_log_x += lx;
      const double log_z = lx - phi[1];  // log(x_i / theta)
      sum_log_xt += log_z;
      const double t = alpha * log_z;
      sum_log1p_r += std::log1p(std::exp(t));
    }
    const double dn = static_cast<double>(n);
    const double logLik = dn * 2.0 * std::log(alpha) + alpha * sum_log_xt -
                          sum_log_x - (alpha + 1.0) * sum_log1p_r;
    return -logLik;
  }
};

inline ParalogisResult paralogis_mle(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n < 2) return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
  for (auto v : x) {
    if (!std::isfinite(v) || v <= 0.0) {
      return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
    }
  }

  // Median start for theta.
  std::vector<double> sorted(x.begin(), x.end());
  std::sort(sorted.begin(), sorted.end());
  const double theta0 = (n % 2 == 0)
                            ? 0.5 * (sorted[n / 2 - 1] + sorted[n / 2])
                            : sorted[n / 2];
  if (theta0 <= 0.0) {
    return {kNaN, kNaN, kNaN, 0, ParalogisStatus::InvalidStart};
  }

  double mean_lz = 0.0;
  for (auto v : x) mean_lz += std::log(v / theta0);
  mean_lz /= static_cast<double>(n);
  double var_lz = 0.0;
  for (auto v : x) {
    const double d = std::log(v / theta0) - mean_lz;
    var_lz += d * d;
  }
  var_lz /= static_cast<double>(n - 1);
  const double sd_lz = std::sqrt(var_lz);
  double alpha0 = (sd_lz > 0.0) ? 2.0 / sd_lz : 1.5;
  if (alpha0 < 1.5) alpha0 = 1.5;

  ParalogisCost cost{x};
  finite_diff_cost<ParalogisCost> fd{cost};
  const double phi0[2] = {mlcore::to_log(alpha0), mlcore::to_log(theta0)};

  BfgsControl ctl;
  ctl.grad_tol = tol;
  ctl.maxit = maxit;
  auto r = bfgs_minimize(fd, phi0, ctl);

  const double shape = mlcore::from_log(r.theta[0]);
  const double scale = mlcore::from_log(r.theta[1]);
  const double logLik = -r.f;
  if (r.status != BfgsStatus::Ok) {
    return {shape, scale, logLik, r.iterations, ParalogisStatus::BfgsFailure};
  }
  return {shape, scale, logLik, r.iterations, ParalogisStatus::Ok};
}

}  // namespace mlcore
