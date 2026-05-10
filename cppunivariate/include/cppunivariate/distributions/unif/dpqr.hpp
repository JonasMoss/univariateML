// Uniform distribution on [min, max].
//   f(x)    = 1/(max-min)              for x in [min, max]
//   F(x)    = (x - min)/(max - min)    for x in [min, max]
//   F^-1(p) = min + p * (max - min)
// R semantics: density at endpoints is 1/(max-min) (closed interval).

#pragma once

#include "../../special/constants.hpp"

#include <cmath>

namespace cppunivariate {
namespace distributions {

using cppunivariate::special::kNaN;
using cppunivariate::special::kNegInf;

inline double dunif(double x, double min_, double max_, bool log) {
  if (!std::isfinite(min_) || !std::isfinite(max_) || !(max_ > min_) ||
      !std::isfinite(x)) {
    return kNaN;
  }
  if (x < min_ || x > max_) {
    return log ? kNegInf : 0.0;
  }
  const double density = 1.0 / (max_ - min_);
  return log ? std::log(density) : density;
}

inline double punif(double q, double min_, double max_, bool lower_tail,
                    bool log_p) {
  if (!std::isfinite(min_) || !std::isfinite(max_) || !(max_ > min_) ||
      !std::isfinite(q)) {
    return kNaN;
  }
  double prob;
  if (q <= min_) prob = 0.0;
  else if (q >= max_) prob = 1.0;
  else prob = (q - min_) / (max_ - min_);
  if (!lower_tail) prob = 1.0 - prob;
  return log_p ? std::log(prob) : prob;
}

inline double qunif(double p, double min_, double max_, bool lower_tail,
                    bool log_p) {
  if (!std::isfinite(min_) || !std::isfinite(max_) || !(max_ > min_) ||
      !std::isfinite(p)) {
    return kNaN;
  }
  if (log_p) p = std::exp(p);
  if (!lower_tail) p = 1.0 - p;
  return min_ + p * (max_ - min_);
}

template <typename Rng>
inline double runif_(Rng &rng, double min_, double max_) {
  return rng.runif(min_, max_);
}

}  // namespace distributions
}  // namespace cppunivariate
