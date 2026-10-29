// Logarithmic-series MLE. The likelihood equation
//   mean(x) = -theta / ((1 - theta) log(1 - theta))
// becomes, with u = -log(1 - theta) > 0, expm1(u)/u = mean(x), i.e.
//   q(u) = (expm1(u) - u)/u = delta,   delta = (sum(x) - n)/n > 0.
// q is increasing from 0, so the root is unique; it is bracketed and bisected
// on a log scale. This replaces a closed form through the Lambert W branch
// W_{-1}, which loses all precision as mean(x) -> 1 (the branch point) and
// returned theta outside (0, 1) there. The all-ones case is degenerate
// (theta -> 0); flagged via the result struct.

#pragma once

#include "../../core/span.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct LgserResult {
  double theta;
  double logLik;
  bool all_ones;
};

namespace lgser_detail {
// (expm1(u) - u) / u, accurate for small u.
inline double q(double u) {
  if (u < 1e-3) {
    // u/2 + u^2/6 + u^3/24 + u^4/120 + u^5/720
    return u * (0.5 + u * (1.0 / 6.0 + u * (1.0 / 24.0 + u * (1.0 / 120.0 + u / 720.0))));
  }
  return (std::expm1(u) - u) / u;
}
}  // namespace lgser_detail

inline LgserResult lgser_mle(ispan x, double fixed=std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, kNaN, false};

  if(!std::isnan(fixed)) {
    if(!(fixed>0 && fixed<1 && std::isfinite(fixed)))return {kNaN,kNaN,false};
    double ll=0;for(int v:x) {if(v<1)return {kNaN,kNaN,false};ll+=v*std::log(fixed)-std::log(v)-std::log(-std::log1p(-fixed));}
    return {fixed,ll,false};
  }
  double sumx = 0.0;
  for (auto v : x) sumx += static_cast<double>(v);
  const double dn = static_cast<double>(n);
  if (sumx == dn) return {kNaN, kNaN, true};
  // Integer counts: sumx - n is exact, so delta carries one rounding.
  const double delta = (sumx - dn) / dn;
  if (!(delta > 0.0) || !std::isfinite(delta)) return {kNaN, kNaN, false};

  double hi = 1.0;
  while (lgser_detail::q(hi) < delta) hi *= 2.0;
  double lo = hi;
  while (lgser_detail::q(lo) >= delta) lo *= 0.5;
  for (int i = 0; i < 200 && hi / lo - 1.0 > 4.0 * std::numeric_limits<double>::epsilon(); ++i) {
    const double mid = std::sqrt(lo) * std::sqrt(hi);
    if (lgser_detail::q(mid) < delta) lo = mid; else hi = mid;
  }
  const double u = std::sqrt(lo) * std::sqrt(hi);  // u = -log(1 - theta)
  const double theta = -std::expm1(-u);

  double sum_log_x = 0.0;
  for (auto v : x) sum_log_x += std::log(static_cast<double>(v));
  const double logLik = -dn * std::log(u) + sumx * std::log(theta) - sum_log_x;
  return {theta, logLik, false};
}

}  // namespace cppunivariate
