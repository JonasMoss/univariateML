// Port of instructions/nmath/qlogis.c. Logistic quantile (closed form).

#include "nmath/dpq.hpp"
#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cmath>

namespace mlcore {
namespace nmath {

double qlogis(double p, double location, double scale, int lower_tail,
              int log_p) {
  if (ISNAN(p) || ISNAN(location) || ISNAN(scale))
    return p + location + scale;
  R_Q_P01_boundaries(p, ML_NEGINF, ML_POSINF);

  if (scale < 0.) ML_WARN_return_NAN;
  if (scale == 0.) return location;

  // p := logit(p) = log(p / (1-p))
  if (log_p) {
    if (lower_tail) {
      p = p - R_Log1_Exp(p);
    } else {
      p = R_Log1_Exp(p) - p;
    }
  } else {
    p = std::log(lower_tail ? (p / (1. - p)) : ((1. - p) / p));
  }

  return location + scale * p;
}

}  // namespace nmath
}  // namespace mlcore
