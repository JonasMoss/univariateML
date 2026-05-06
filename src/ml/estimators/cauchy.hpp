// Cauchy MLE: bivariate Newton on (mu, log-scale).
//
// Initialization: median for mu, log of MAD for log-scale. Then Newton
// iterations using the closed-form score and Hessian of the Cauchy
// log-likelihood. Returns iterations == 0 only on empty input (caller
// turns that into an error).

#pragma once

#include "../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace mlcore {

struct CauchyResult {
  double mu;
  double scale;
  double logLik;
  int iterations;
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

}  // namespace mlcore
