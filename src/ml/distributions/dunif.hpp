// Discrete uniform distribution on integers {min, min+1, ..., max}.
// Parameters: min, max integers with min <= max.
//   p(x)   = 1/(max-min+1) for integer x in [min, max], 0 otherwise.
//   F(x)   = (floor(x) - min + 1) / (max - min + 1) on [min, max].
//   F^-1(p)= ceil(p*(max-min+1) + min - 1) for p in (0, 1].
// Sampler: inverse CDF on U ~ Uniform(0, 1).

#pragma once

#include "../special/constants.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

namespace detail {
inline bool dunif_valid_params(double min, double max) {
  return std::isfinite(min) && std::isfinite(max) && min <= max &&
         min == std::floor(min) && max == std::floor(max);
}
} // namespace detail

inline double ddunif(double x, double min, double max, bool log) {
  if (!detail::dunif_valid_params(min, max) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x < min || x > max || x != std::floor(x)) {
    return log ? kNegInf : 0.0;
  }
  const double pmf = 1.0 / (max - min + 1.0);
  return log ? std::log(pmf) : pmf;
}

inline double pdunif(double q, double min, double max, bool lower_tail,
                     bool log_p) {
  if (!detail::dunif_valid_params(min, max) || !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q < min) {
    prob = 0.0;
  } else if (q >= max) {
    prob = 1.0;
  } else {
    prob = (std::floor(q) - min + 1.0) / (max - min + 1.0);
  }
  if (!lower_tail) {
    prob = 1.0 - prob;
  }
  return log_p ? std::log(prob) : prob;
}

inline double qdunif(double p, double min, double max, bool lower_tail,
                     bool log_p) {
  if (!detail::dunif_valid_params(min, max) || !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) {
    p = std::exp(p);
  }
  if (!lower_tail) {
    p = 1.0 - p;
  }
  if (p == 0.0 || min == max) {
    return min;
  }
  return std::ceil(p * (max - min + 1.0) + min - 1.0);
}

template <typename Rng>
inline double rdunif(Rng &rng, double min, double max) {
  const double u = rng.runif(0.0, 1.0);
  return std::ceil(u * (max - min + 1.0) + min - 1.0);
}

} // namespace distributions
} // namespace mlcore
