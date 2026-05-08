// Cauchy MLE: bivariate Newton on (mu, log-scale).
//
// Initialization: median for mu, log of MAD for log-scale. Then Newton
// iterations using the closed-form score and Hessian of the Cauchy
// log-likelihood. Returns iterations == 0 only on empty input (caller
// turns that into an error).

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

struct CauchyResult {
  double mu;
  double scale;
  double logLik;
  int iterations;
};

// Negative log-likelihood of Cauchy(mu, scale=s):
//   log f = -log(pi) - log(s) - log(1 + ((x-mu)/s)^2)
//   nll = -sum log f.
// Per-obs Hessian (z = (x-mu)/s):
//   H_mm = 2(1-z^2) / (s^2 (1+z^2)^2)
//   H_ms = 4z       / (s^2 (1+z^2)^2)
//   H_ss = (z^4 + 4z^2 - 1) / (s^2 (1+z^2)^2)
struct CauchyCost {
  span x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double mu = theta[0];
    const double s = theta[1];
    if (!(s > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double sum = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      sum += std::log(1.0 + z * z);
    }
    return n * (std::log(M_PI) + std::log(s)) + sum;
  }
  void gradient(const double *theta, double *g) const {
    const double mu = theta[0];
    const double s = theta[1];
    const double n = static_cast<double>(x.size());
    double dnll_dmu = 0.0, dnll_ds = 0.0;
    for (auto v : x) {
      const double z = (v - mu) / s;
      const double w = 1.0 / (1.0 + z * z);
      // d log f / dmu = 2z / (s(1+z^2)) = 2z*w/s
      // d log f / ds  = -1/s + 2z^2/(s(1+z^2)) = (2z^2*w - 1)/s
      dnll_dmu += -2.0 * z * w / s;
      dnll_ds += -(2.0 * z * z * w - 1.0) / s;
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
      const double zz = z * z;
      const double w = 1.0 / (1.0 + zz);
      const double w2 = w * w;
      H_mm += 2.0 * (1.0 - zz) * w2;
      H_ms += 4.0 * z * w2;
      H_ss += (zz * zz + 4.0 * zz - 1.0) * w2;
    }
    H[0] = H_mm / s2;
    H[1] = H_ms / s2;
    H[2] = H[1];
    H[3] = H_ss / s2;
  }
};

inline CauchyResult fit_cauchy(span x, double tol, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kEps = std::numeric_limits<double>::epsilon();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0};

  std::vector<double> data(x.begin(), x.end());
  std::nth_element(data.begin(), data.begin() + n / 2, data.end());
  double mu = data[n / 2];

  for (std::size_t i = 0; i < n; ++i) data[i] = std::fabs(data[i] - mu);
  std::nth_element(data.begin(), data.begin() + n / 2, data.end());
  double theta = std::log(data[n / 2] + kEps);

  double dmu = 0.0, dth = 0.0;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double inv_s = std::exp(-theta);
    const double inv_s2 = inv_s * inv_s;

    double sum1 = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0, sum5 = 0.0;
    for (std::size_t i = 0; i < n; ++i) {
      const double z = (x[i] - mu) * inv_s;
      const double z2 = z * z;
      const double d1 = 1.0 + z2;
      const double i1 = 1.0 / d1;
      const double i2 = i1 * i1;
      sum1 += z * i1;
      sum2 += (z2 - 1) * i1;
      sum3 += (1 - z2) * i2;
      sum4 += z * i2;
      sum5 += z2 * i2;
    }

    const double U_mu = 2 * inv_s * sum1;
    const double U_th = sum2;
    const double H_mm = 2 * inv_s2 * sum3;
    const double H_mth = 4 * inv_s * sum4;
    const double H_thth = 4 * sum5;

    const double det = H_mm * H_thth - H_mth * H_mth;
    dmu = (H_thth * U_mu - H_mth * U_th) / det;
    dth = (-H_mth * U_mu + H_mm * U_th) / det;

    mu += dmu;
    theta += dth;
    if (std::max(std::fabs(dmu), std::fabs(dth)) < tol) break;
  }

  const double scale = std::exp(theta);
  double sum_log1p_z2 = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    const double z = (x[i] - mu) / scale;
    sum_log1p_z2 += std::log1p(z * z);
  }
  const double logLik =
      -static_cast<double>(n) * std::log(M_PI * scale) - sum_log1p_z2;
  return {mu, scale, logLik, iter};
}

}  // namespace cppunivariate
