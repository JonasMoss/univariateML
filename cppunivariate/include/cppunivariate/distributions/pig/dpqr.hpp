// Stable PIG log masses. The R shell continues to use actuar's dpqr.
#pragma once
#include <cmath>
#include <limits>
#include <vector>
namespace cppunivariate {
namespace pig_detail {
inline double logadd(double a, double b) {
  const double m = a > b ? a : b;
  if (!std::isfinite(m)) return m;
  return m + std::log1p(std::exp((a > b ? b : a) - m));
}
// Ratio recurrence avoids both mass underflow and cancellation in p0.
inline std::vector<double> log_masses(int maxk, double mean, double shape) {
  std::vector<double> out(static_cast<std::size_t>(maxk) + 1);
  const double lm = std::log(mean), ls = std::log(shape), l2 = std::log(2.0);
  double la, lb, lr;
  if (std::isinf(mean)) {
    out[0] = -std::sqrt(2.0 * shape);
    la = 0; lb = ls-l2; lr = 0.5*lb;
  } else {
    const double lu = l2 + 2*lm-ls;
    const double lv = logadd(0, lu);
    out[0] = -std::exp(l2+lm-logadd(0, 0.5*lv));
    la = lu-lv; lb = 2*lm-lv; lr = lm-0.5*lv;
  }
  if (maxk > 0) out[1] = out[0]+lr;
  for (int k=2; k<=maxk; ++k) {
    const double dk = static_cast<double>(k);
    lr = logadd(la+std::log1p(-1.5/dk), lb-std::log(dk)-std::log(dk-1)-lr);
    out[k] = out[k-1]+lr;
  }
  return out;
}
}
namespace distributions {
inline double dpig(double x, double mean, double shape, bool log = false) {
  if (!(mean > 0 && shape > 0) || !std::isfinite(shape) || std::isnan(x))
    return std::numeric_limits<double>::quiet_NaN();
  if (!std::isfinite(x) || x < 0 || x != std::floor(x))
    return log ? -std::numeric_limits<double>::infinity() : 0;
  // Counts beyond the loop's integer range are unsupported.
  if (x >= std::numeric_limits<int>::max()) return std::numeric_limits<double>::quiet_NaN();
  const double v = pig_detail::log_masses(static_cast<int>(x), mean, shape).back();
  return log ? v : std::exp(v);
}
}
}
