// Port of instructions/nmath/plogis.c. Logistic CDF; closed form via
// log1pexp for numerical stability.

#include "nmath/dpq.hpp"
#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cmath>

namespace mlcore {
namespace nmath {

namespace {

// log(1 + exp(x)) without overflow; fast for x > 18.
inline double log1pexp(double x) {
  if (x <= 18.) return std::log1p(std::exp(x));
  if (x > 33.3) return x;
  return x + std::exp(-x);
}

}  // namespace

double plogis(double x, double location, double scale, int lower_tail,
              int log_p) {
  if (ISNAN(x) || ISNAN(location) || ISNAN(scale))
    return x + location + scale;
  if (scale <= 0.0) ML_WARN_return_NAN;

  x = (x - location) / scale;
  if (ISNAN(x)) ML_WARN_return_NAN;
  R_P_bounds_Inf_01(x);

  if (log_p) {
    return -log1pexp(lower_tail ? -x : x);
  } else {
    return 1 / (1 + std::exp(lower_tail ? -x : x));
  }
}

}  // namespace nmath
}  // namespace mlcore
