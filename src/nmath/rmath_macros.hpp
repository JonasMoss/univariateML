// nmath.h-style preprocessor macros that the vendored R nmath sources
// consult. Defined as macros (not inline functions) because the upstream
// .c sources reference them as bare names inside arithmetic expressions.
//
// This header is ONLY included by src/nmath_*.cpp files (each its own
// translation unit), so the macros do not leak into anything else.
// polygamma.cpp uses src/ml/special/rmath_compat.hpp's namespaced inline
// functions instead — kept separate to avoid polluting ml_exports.cpp,
// which #includes polygamma.cpp.

#pragma once

#include <cfloat>
#include <cmath>
#include <limits>

#ifndef ML_NAN
#define ML_NAN          (std::numeric_limits<double>::quiet_NaN())
#define ML_POSINF       (std::numeric_limits<double>::infinity())
#define ML_NEGINF       (-std::numeric_limits<double>::infinity())
#endif

#ifndef ISNAN
#define ISNAN(x)        (std::isnan(x))
#endif

#ifndef R_FINITE
#define R_FINITE(x)     (std::isfinite(x))
#endif

#ifndef R_forceint
#define R_forceint(x)   (std::nearbyint(x))
#endif

#ifndef ML_WARN_return_NAN
#define ML_WARN_return_NAN return ML_NAN
#endif

#ifndef attribute_hidden
#define attribute_hidden /* not exporting from a shared lib here */
#endif

// nmath.h math constants. <cmath> on glibc gives us M_PI / M_LN2 / M_SQRT2;
// the rest are R-specific.
#ifndef M_LN_SQRT_2PI
#define M_LN_SQRT_2PI   0.918938533204672741780329736406  /* log(sqrt(2*pi)) */
#endif
#ifndef M_1_SQRT_2PI
#define M_1_SQRT_2PI    0.398942280401432677939946059934  /* 1/sqrt(2*pi) */
#endif
#ifndef M_2PI
#define M_2PI           6.283185307179586476925286766559  /* 2 * pi */
#endif
#ifndef M_SQRT_32
#define M_SQRT_32       5.656854249492380195206754896838  /* sqrt(32) */
#endif
#ifndef M_LN_SQRT_PId2
#define M_LN_SQRT_PId2  0.225791352644727432363097614947  /* log(sqrt(pi/2)) */
#endif
#ifndef M_PI
#define M_PI            3.141592653589793238462643383280
#endif
#ifndef M_LN2
#define M_LN2           0.693147180559945309417232121458
#endif
#ifndef M_SQRT2
#define M_SQRT2         1.414213562373095048801688724210
#endif
