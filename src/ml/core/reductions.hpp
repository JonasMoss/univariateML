// Common scalar reductions over an mlcore::span. Centralises what every
// estimator otherwise hand-rolls. All inline; zero overhead vs writing the
// loop directly.

#pragma once

#include "span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>

namespace mlcore {

inline double sum(span x) {
  double s = 0.0;
  for (auto v : x) s += v;
  return s;
}

inline double sum_log(span x) {
  double s = 0.0;
  for (auto v : x) s += std::log(v);
  return s;
}

inline double sum_sq(span x) {
  double s = 0.0;
  for (auto v : x) s += v * v;
  return s;
}

inline double sum_log_sq(span x) {
  double s = 0.0;
  for (auto v : x) {
    const double l = std::log(v);
    s += l * l;
  }
  return s;
}

inline double mean(span x) { return sum(x) / static_cast<double>(x.size()); }

inline double mean_log(span x) {
  return sum_log(x) / static_cast<double>(x.size());
}

inline double min_value(span x) {
  double m = x[0];
  for (std::size_t i = 1; i < x.size(); ++i) m = std::min(m, x[i]);
  return m;
}

inline double max_value(span x) {
  double m = x[0];
  for (std::size_t i = 1; i < x.size(); ++i) m = std::max(m, x[i]);
  return m;
}

}  // namespace mlcore
