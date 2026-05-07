// Chebyshev series evaluation. Port of instructions/nmath/chebyshev.c.
// `chebyshev_init` from upstream isn't called by anything our dpqr uses,
// so it's omitted; only the eval routine ports.

#include "nmath/nmath.hpp"
#include "nmath/rmath_macros.hpp"

#include <cmath>

namespace mlcore {
namespace nmath {

double chebyshev_eval(double x, const double *a, int n) {
  double b0, b1, b2, twox;
  int i;

  if (n < 1 || n > 1000) ML_WARN_return_NAN;
  if (x < -1.1 || x > 1.1) ML_WARN_return_NAN;

  twox = x * 2;
  b2 = b1 = 0;
  b0 = 0;
  for (i = 1; i <= n; i++) {
    b2 = b1;
    b1 = b0;
    b0 = twox * b1 - b2 + a[n - i];
  }
  return (b0 - b2) * 0.5;
}

}  // namespace nmath
}  // namespace mlcore
