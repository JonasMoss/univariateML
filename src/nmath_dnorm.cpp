// Port of instructions/nmath/dnorm.c. Computes the normal density. Uses
// Welinder's split for tail accuracy (PR#15620). Pure libm.
//
// The dpq.hpp macro `#define give_log log_p` aliases the parameter name
// so the R_D__0 / R_D__1 macros (which reference `log_p`) resolve to the
// function's `give_log` argument transparently.

#include "nmath/dpq.hpp"
#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cfloat>
#include <cmath>

namespace mlcore {
namespace nmath {

double dnorm4(double x, double mu, double sigma, int give_log) {
  if (ISNAN(x) || ISNAN(mu) || ISNAN(sigma)) return x + mu + sigma;
  if (sigma < 0) ML_WARN_return_NAN;
  if (!R_FINITE(sigma)) return R_D__0;
  if (!R_FINITE(x) && mu == x) return ML_NAN;  // x-mu is NaN
  if (sigma == 0) return (x == mu) ? ML_POSINF : R_D__0;
  x = (x - mu) / sigma;

  if (!R_FINITE(x)) return R_D__0;

  x = std::fabs(x);
  if (x >= 2 * std::sqrt(DBL_MAX)) return R_D__0;
  if (give_log) return -(M_LN_SQRT_2PI + 0.5 * x * x + std::log(sigma));

  // M_1_SQRT_2PI = 1 / sqrt(2 * pi)
  if (x < 5) return M_1_SQRT_2PI * std::exp(-0.5 * x * x) / sigma;

  // x*x may lose up to ~2 digits accuracy for "large" x; use Welinder split.
  if (x > std::sqrt(-2 * M_LN2 * (DBL_MIN_EXP + 1 - DBL_MANT_DIG))) return 0.0;

  double x1 = std::ldexp(R_forceint(std::ldexp(x, 16)), -16);
  double x2 = x - x1;
  return M_1_SQRT_2PI / sigma *
         (std::exp(-0.5 * x1 * x1) * std::exp((-0.5 * x2 - x1) * x2));
}

}  // namespace nmath
}  // namespace mlcore
