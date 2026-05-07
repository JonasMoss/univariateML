// Logarithmic-series MLE. Closed-form via Lambert W. The all-ones case
// is degenerate (theta is unidentified); flagged via the result struct.

#pragma once

#include "../core/lambert_w.hpp"
#include "../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace mlcore {

struct LgserResult {
  double theta;
  double logLik;
  bool all_ones;
};

// Negative log-likelihood of LogSeries(theta), x in {1,2,...}:
//   pmf: p(x) = -theta^x / (x * log(1 - theta))
//   nll(theta) = -sum(x) * log(theta) + n * log(-log(1 - theta)) + const.
// Hessian (sympy-derived):
//   H = sum(x)/theta^2 - n/((theta-1)^2 * log(1-theta))
//                       - n/((theta-1)^2 * log(1-theta)^2)
struct LgserCost {
  ispan x;
  int dim() const { return 1; }
  double operator()(const double *theta_p) const {
    const double t = theta_p[0];
    if (!(t > 0.0 && t < 1.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    return -S_x * std::log(t) + n * std::log(-std::log(1.0 - t));
  }
  void gradient(const double *theta_p, double *g) const {
    const double t = theta_p[0];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    g[0] = -S_x / t + n / ((t - 1.0) * std::log(1.0 - t));
  }
  void hessian(const double *theta_p, double *H) const {
    const double t = theta_p[0];
    const double n = static_cast<double>(x.size());
    double S_x = 0.0;
    for (auto v : x) S_x += static_cast<double>(v);
    const double L = std::log(1.0 - t);
    const double tm1sq = (t - 1.0) * (t - 1.0);
    H[0] = S_x / (t * t) - n / (tm1sq * L) - n / (tm1sq * L * L);
  }
};

inline LgserResult lgser_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, kNaN, false};

  double sumx = 0.0;
  for (auto v : x) sumx += static_cast<double>(v);
  const double dn = static_cast<double>(n);
  const double x_bar = sumx / dn;
  if (sumx == dn) return {kNaN, kNaN, true};

  const double z = -std::exp(-1.0 / x_bar) / x_bar;
  const double xi = -x_bar * lambertw0(z) - 1.0;
  const double theta = xi / (xi + 1.0);

  double sum_log_x = 0.0;
  for (auto v : x) sum_log_x += std::log(static_cast<double>(v));
  const double logLik = -dn * std::log(-std::log1p(-theta)) +
                        sumx * std::log(theta) - sum_log_x;
  return {theta, logLik, false};
}

}  // namespace mlcore
