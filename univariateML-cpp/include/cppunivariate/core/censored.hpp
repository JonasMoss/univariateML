// Non-owning interval observations. Non-point cells mean (lower, upper].
#pragma once
#include "span.hpp"
#include <cmath>
#include <limits>
namespace cppunivariate {
struct CensoredData { span lower; span upper; };
enum class CensoredStatus { Ok, InvalidInput };
struct CensoredCounts {
  std::size_t exact = 0, right = 0, left = 0, interval = 0;
  CensoredStatus status = CensoredStatus::Ok;
};
// Exact points take precedence, then right censoring with a finite lower endpoint.
// A full-line cell is left-censored; infinite exact points are invalid.
inline CensoredCounts classify_censored(CensoredData x,
    double support_lower = -std::numeric_limits<double>::infinity()) {
  CensoredCounts c;
  if (x.lower.size() != x.upper.size() || std::isnan(support_lower)) {
    c.status = CensoredStatus::InvalidInput; return c;
  }
  for (std::size_t i=0; i<x.lower.size(); ++i) {
    const double l=x.lower[i], u=x.upper[i];
    if (std::isnan(l) || std::isnan(u) || l>u || (l==u && !std::isfinite(l))) {
      c.status=CensoredStatus::InvalidInput; return c;
    }
    if (l==u) ++c.exact;
    else if (std::isfinite(l) && u==std::numeric_limits<double>::infinity()) ++c.right;
    else if (l==-std::numeric_limits<double>::infinity() || l<=support_lower) ++c.left;
    else ++c.interval;
  }
  return c;
}
} // namespace cppunivariate
