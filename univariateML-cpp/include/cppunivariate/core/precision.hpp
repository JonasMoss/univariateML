// One type spelling for extended intermediates throughout the core.
// BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS selects double arithmetic and
// disables Boost's long-double backend. The literal macro preserves native
// long-double tokens and selects the double literal under the same switch.
#pragma once
#if defined(__EMSCRIPTEN__) && !defined(BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS)
#define BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#endif
#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
#define CPPUNIV_EXTENDED_REAL double
#define CPPUNIV_EXTENDED_LITERAL(d, l) d
#else
#define CPPUNIV_EXTENDED_REAL long double
#define CPPUNIV_EXTENDED_LITERAL(d, l) l
#endif

// Compatibility spelling for the special-function backend; this aliases the
// same type macro and has no independent configuration.
namespace cppunivariate { typedef CPPUNIV_EXTENDED_REAL ExtendedReal; }
