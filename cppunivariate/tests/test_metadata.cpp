// Compile-time exercise of the per-distribution metadata.hpp
// canonical form. All checks are static_assert; the runtime body
// only exists so the file compiles into a test binary that ctest
// can register. A successful compile is the whole verification.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "cppunivariate/distributions/exp/metadata.hpp"
#include "cppunivariate/distributions/lomax/metadata.hpp"

#include <cstring>

namespace lomax_meta = cppunivariate::distributions::lomax;
namespace exp_meta   = cppunivariate::distributions::exp;

// Lomax: 2 parameters (lambda, kappa) on (0, inf).
static_assert(lomax_meta::n_params == 2, "lomax must have two parameters");
static_assert(lomax_meta::params[0].estimable, "lambda is estimable today");
static_assert(lomax_meta::params[1].estimable, "kappa is estimable today");
static_assert(
    lomax_meta::support.type == cppunivariate::SupportType::Real,
    "lomax support is real-valued");
static_assert(lomax_meta::support.lower == 0.0, "lomax lower bound is 0");
static_assert(!lomax_meta::support.lower_closed, "lomax lower is open");

// Exponential: 1 parameter (rate) on [0, inf).
static_assert(exp_meta::n_params == 1, "exponential must have one parameter");
static_assert(exp_meta::params[0].estimable, "rate is estimable today");
static_assert(
    exp_meta::support.type == cppunivariate::SupportType::Real,
    "exp support is real-valued");
static_assert(exp_meta::support.lower == 0.0, "exp lower bound is 0");
static_assert(exp_meta::support.lower_closed, "exp lower is closed");

TEST_CASE("metadata: id strings match folder names") {
  CHECK(std::strcmp(lomax_meta::id, "lomax") == 0);
  CHECK(std::strcmp(exp_meta::id, "exp") == 0);
}

TEST_CASE("metadata: param names are accessible") {
  CHECK(std::strcmp(lomax_meta::params[0].name, "lambda") == 0);
  CHECK(std::strcmp(lomax_meta::params[1].name, "kappa") == 0);
  CHECK(std::strcmp(exp_meta::params[0].name, "rate") == 0);
}
