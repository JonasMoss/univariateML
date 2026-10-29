// Normal MLE: closed form.
//
// The mean is formed from deviations from the first observation, which are
// exact for clustered data (Sterbenz), so the low bits of nearly tied or
// offset data survive; the sum of squares is scaled by the largest deviation
// so that data near 1e300 or 1e-300 neither overflow nor underflow.
//
// Standalone C++ implementation, free of R / Armadillo dependencies.
//
#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct NormResult {
  double mean;
  double sd;
  double logLik;
};

namespace norm_detail {
struct Moments {
  double mean;
  double sd;
};

inline Moments moments(span x) {
  const std::size_t n = x.size();
  const double dn = static_cast<double>(n);
  double c = x[0];
  long double sum_d = 0.0L;
  for (auto v : x) sum_d += static_cast<long double>(v) - c;
  if (!std::isfinite(sum_d)) {
    // Deviations overflow (data of both signs near the double range).
    c = 0.0;
    sum_d = 0.0;
    for (auto v : x) sum_d += v / dn;
    sum_d *= dn;
  }
  const double mean_hat = static_cast<double>(c + sum_d / dn);
  // Profile scale at the representable mean actually returned. For a
  // cloud narrower than one ulp this matters to the reported likelihood.
  const long double mean_d = static_cast<long double>(mean_hat) - c;
  double big = 0.0;
  for (auto v : x) big = std::max(big, static_cast<double>(std::abs((static_cast<long double>(v) - c) - mean_d)));
  double ss = 0.0;
  if (big > 0.0 && std::isfinite(big)) {
    for (auto v : x) {
      const double z = ((static_cast<long double>(v) - c) - mean_d) / big;
      ss += z * z;
    }
  }
  return {mean_hat, big > 0.0 ? big * std::sqrt(ss / dn) : 0.0};
}
}  // namespace norm_detail

inline NormResult norm_mle(span x) {
  const std::size_t n = x.size();
  if (n == 0) {
    constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
    return {kNaN, kNaN, kNaN};
  }
  const auto m = norm_detail::moments(x);
  const double dn = static_cast<double>(n);
  const double logLik = -0.5 * dn * (std::log(2.0 * M_PI) + 1.0) - dn * std::log(m.sd);
  return {m.mean, m.sd, logLik};
}

}  // namespace cppunivariate
