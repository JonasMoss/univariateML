#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/beta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("beta") {
  auto g = mltest::load_distribution_golden("beta");
  auto r = cppunivariate::beta_mle(mltest::as_span(g.data), 1.490116e-08, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("shape1")));
  CHECK(r.beta == C_APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("beta dpqr: U(0,1) special case (shape1=shape2=1)") {
  CHECK(cppunivariate::distributions::dbeta(0.3, 1.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::pbeta(0.4, 1.0, 1.0, true, false) == doctest::Approx(0.4));
  CHECK(cppunivariate::distributions::qbeta(0.7, 1.0, 1.0, true, false) == doctest::Approx(0.7));
  // Asymmetric round-trip.
  const double q = cppunivariate::distributions::qbeta(0.6, 2.0, 5.0, true, false);
  CHECK(cppunivariate::distributions::pbeta(q, 2.0, 5.0, true, false) == doctest::Approx(0.6));
}
