// Logistic MLE: bivariate Newton on (mu, log-scale).
//
// Initialization via Welford one-pass mean/variance; theta seeded so
// scale ≈ sqrt(var) * sqrt(3) / pi (matches Logistic distribution variance).
//
// Convergence is absolute on max(|dmu|, |dtheta|) -- a 2D Newton, so
// outside the scope of cppunivariate::newton_raphson_1d. Used as the inner
// engine of Log-Logistic via a log-transform.

#pragma once

#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct LogisticResult {
  double mu;
  double scale;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Logistic(mu, scale=s):
//   log f = -z - log(s) - 2 log(1 + e^{-z}),  z = (x-mu)/s.
// Per-obs Hessian (P = 1/(1+e^{-z}), so 2 P (1-P) = sech^2(z/2)/2):
//   H_mm = 2 P(1-P) / s^2
//   H_ms = (2P - 1 + 2 P(1-P) z) / s^2
//   H_ss = (2(2P-1) z + 2 P(1-P) z^2 - 1) / s^2
struct LogisticCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double s = theta[1];
    if (!(s > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double acc = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      // numerically stable: log(1 + e^{-z}) = max(0,-z) + log1p(exp(-|z|))
      const double az = std::fabs(z);
      const double log1pe = std::log1p(std::exp(-az)) + (z < 0.0 ? -z : 0.0);
      acc += -z - 2.0 * log1pe;
    }
    return n * std::log(s) - acc;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double s = theta[1];
    double dnll_dmu = 0.0, dnll_ds = 0.0;
    const double n = static_cast<double>(x.size());
    for (auto v : x) {
      const double z = (v - mu) / s;
      // P = 1/(1+e^{-z}); stable for z in either sign.
      const double P = (z >= 0.0)
                           ? 1.0 / (1.0 + std::exp(-z))
                           : std::exp(z) / (1.0 + std::exp(z));
      // dlogf/dmu = (2P-1)/s; dlogf/ds = -[(1-2P) z + 1]/s.
      dnll_dmu += -(2.0 * P - 1.0) / s;
      dnll_ds += ((1.0 - 2.0 * P) * z + 1.0) / s;
    }
    g[0] = dnll_dmu;
    g[1] = dnll_ds;
    (void)n;
  }
  void hessian(const double *theta, double *H) const {
    const double mu = theta[0];
    const double s = theta[1];
    const double s2 = s * s;
    double H_mm = 0.0, H_ms = 0.0, H_ss = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      const double P = (z >= 0.0)
                           ? 1.0 / (1.0 + std::exp(-z))
                           : std::exp(z) / (1.0 + std::exp(z));
      const double PQ = P * (1.0 - P);
      const double two_Pm1 = 2.0 * P - 1.0;
      H_mm += 2.0 * PQ;
      H_ms += two_Pm1 + 2.0 * PQ * z;
      H_ss += 2.0 * two_Pm1 * z + 2.0 * PQ * z * z - 1.0;
    }
    H[0] = H_mm / s2;
    H[1] = H_ms / s2;
    H[2] = H[1];
    H[3] = H_ss / s2;
  }
};

inline LogisticResult logistic_mle(span x, double tol, int maxit) {
  const std::size_t n = x.size();

  // One-pass Welford mean/variance for the seed.
  double mean = 0.0;
  double M2 = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double diff = x[i] - mean;
    mean += diff / static_cast<double>(i + 1);
    M2 += diff * (x[i] - mean);
  }

  double mu = mean;
  double sample_var = (n > 1) ? M2 / static_cast<double>(n - 1) : 0.0;
  if (sample_var <= 0.0) sample_var = 1.0;
  double theta = std::log(std::sqrt(sample_var) * std::sqrt(3.0) / M_PI);

  int iter = 0;
  for (iter = 0; iter < maxit; ++iter) {
    const double inv_s = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double U1 = 0.0, U2 = 0.0;
    double sum_c2 = 0.0, sum_tz = 0.0, sum_z2c2 = 0.0;
    for (auto xi : x) {
      const double z = (xi - mu) * inv_s;
      const double th = std::tanh(0.5 * z);
      const double c2 = 1.0 - th * th;
      U1 += th;
      U2 += z * th;
      sum_c2 += c2;
      sum_tz += th + 0.5 * z * c2;
      sum_z2c2 += z * z * c2;
    }
    U1 *= inv_s;
    U2 -= static_cast<double>(n);

    const double J11 = 0.5 * sum_c2 * inv_s2;
    const double J12 = inv_s * sum_tz;
    const double J22 = (U2 + static_cast<double>(n)) + 0.5 * sum_z2c2;

    const double det = J11 * J22 - J12 * J12;
    const double dmu = (J22 * U1 - J12 * U2) / det;
    const double dtheta = (-J12 * U1 + J11 * U2) / det;

    mu += dmu;
    theta += dtheta;

    if (std::max(std::fabs(dmu), std::fabs(dtheta)) < tol) break;
  }

  const double scale = std::exp(theta);
  double sum_z = 0.0, sum_log1p_exp_neg_z = 0.0;
  for (auto xi : x) {
    const double z = (xi - mu) / scale;
    sum_z += z;
    sum_log1p_exp_neg_z += std::log1p(std::exp(-z));
  }
  const double logLik = -static_cast<double>(n) * std::log(scale) - sum_z -
                        2.0 * sum_log1p_exp_neg_z;
  return {mu, scale, logLik, iter == maxit ? maxit : iter + 1};
}

}  // namespace cppunivariate
