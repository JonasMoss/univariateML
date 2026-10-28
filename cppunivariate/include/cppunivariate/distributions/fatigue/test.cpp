#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("fatigue") {
  auto g = mltest::load_distribution_golden("fatigue");
  auto r = cppunivariate::fatigue_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("alpha")));
  CHECK(r.beta == C_APPROX(g.estimates.at("beta")));
  // Wrapper hardcodes mu=0; golden also stores 0.
  CHECK(g.estimates.at("mu") == 0.0);
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD
