// Pure-libm shims for the small set of R helpers that polygamma.cpp consults.
// Lets the dpsifn port compile without linking Rmath: the standalone C++
// test harness no longer needs an R install.
//
// Values mirror what Rmath returns on IEEE-754 platforms:
//   Rf_d1mach(4) = DBL_EPSILON                 (machine roundoff)
//   Rf_d1mach(5) = log10(2)
//   Rf_i1mach(14) = DBL_MANT_DIG (= 53 for double)
//   Rf_i1mach(15) = DBL_MIN_EXP  (= -1021)
//   Rf_i1mach(16) = DBL_MAX_EXP  (= 1024)
// Other indices aren't called by polygamma.cpp; this header keeps the API
// surface small on purpose.
//
// The src/nmath_*.cpp dpqr primitives use a *different* set of helpers
// (ML_NAN macros, R_FINITE, ISNAN, ...) that live in src/nmath/rmath_macros.hpp
// so we don't leak them into anything that #includes polygamma.cpp.

#pragma once

#include <cfloat>
#include <cmath>
#include <limits>

namespace mlcore {
namespace rmath {

inline constexpr int n_max = 100;

inline double ML_NAN_value() {
  return std::numeric_limits<double>::quiet_NaN();
}
inline double ML_POSINF_value() {
  return std::numeric_limits<double>::infinity();
}

inline double Rf_d1mach(int i) {
  switch (i) {
    case 4:  return DBL_EPSILON;
    case 5:  return 0.301029995663981195;  // log10(2)
    default: return std::numeric_limits<double>::quiet_NaN();
  }
}

inline int Rf_i1mach(int i) {
  switch (i) {
    case 14: return DBL_MANT_DIG;
    case 15: return DBL_MIN_EXP;
    case 16: return DBL_MAX_EXP;
    default: return 0;
  }
}

inline int imin2(int a, int b) { return a < b ? a : b; }
inline double fmax2(double a, double b) { return a > b ? a : b; }
inline double fmin2(double a, double b) { return a < b ? a : b; }

// Integer power via repeated squaring; matches R's R_pow_di on positive,
// zero, and negative exponents.
inline double R_pow_di(double x, int n) {
  if (n == 0) return 1.0;
  double result = 1.0;
  double base = x;
  int exp = n < 0 ? -n : n;
  while (exp > 0) {
    if (exp & 1) result *= base;
    exp >>= 1;
    if (exp > 0) base *= base;
  }
  return n < 0 ? 1.0 / result : result;
}

}  // namespace rmath
}  // namespace mlcore
