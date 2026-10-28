// Compile-time exercise of the per-distribution metadata.hpp
// canonical form. All checks are static_assert; the runtime body
// only exists so the file compiles into a test binary that ctest
// can register. A successful compile is the whole verification.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"


// Compile-check the per-distribution dpqr headers that were added in
// the base-R-backend gap-fill. Successful compilation is the smoke test.
#include "cppunivariate/distributions/beta/dpqr.hpp"
#include "cppunivariate/distributions/cauchy/dpqr.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/logis/dpqr.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include "cppunivariate/distributions/unif/dpqr.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"


// Smoke checks for the newly-added per-distribution dpqr headers.
// These don't claim bit-equivalence to R; they just exercise CDF/quantile
// round-trips and known closed-form values so an outright bug in the
// math fails the test.

namespace dist = cppunivariate::distributions;

TEST_CASE("norm dpqr: standard-normal CDF / quantile round-trip") {
  CHECK(dist::pnorm(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(dist::qnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = dist::qnorm(0.7, 0.0, 1.0, true, false);
  CHECK(dist::pnorm(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

TEST_CASE("unif dpqr: density, CDF, quantile") {
  CHECK(dist::dunif(0.5, 0.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(dist::punif(0.3, 0.0, 1.0, true, false) == doctest::Approx(0.3));
  CHECK(dist::qunif(0.8, 0.0, 1.0, true, false) == doctest::Approx(0.8));
}

TEST_CASE("cauchy dpqr: CDF symmetric at location") {
  CHECK(dist::pcauchy(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(dist::qcauchy(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = dist::qcauchy(0.75, 0.0, 1.0, true, false);
  CHECK(dist::pcauchy(q, 0.0, 1.0, true, false) == doctest::Approx(0.75));
}

TEST_CASE("weibull dpqr: known CDF value and quantile inverse") {
  // F(scale; shape, scale) = 1 - exp(-1) for any shape.
  CHECK(dist::pweibull(1.0, 2.0, 1.0, true, false) ==
        doctest::Approx(1.0 - std::exp(-1.0)));
  const double q = dist::qweibull(0.6, 1.5, 2.0, true, false);
  CHECK(dist::pweibull(q, 1.5, 2.0, true, false) == doctest::Approx(0.6));
}

TEST_CASE("lnorm dpqr: median = exp(meanlog)") {
  CHECK(dist::qlnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(1.0));
  const double q = dist::qlnorm(0.4, 1.0, 0.5, true, false);
  CHECK(dist::plnorm(q, 1.0, 0.5, true, false) == doctest::Approx(0.4));
}

TEST_CASE("logis dpqr: density at location = 1/(4*scale)") {
  CHECK(dist::dlogis(0.0, 0.0, 1.0, false) == doctest::Approx(0.25));
  CHECK(dist::plogis(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  const double q = dist::qlogis(0.7, 0.0, 1.0, true, false);
  CHECK(dist::plogis(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

TEST_CASE("gamma dpqr: rate parameterisation round-trip") {
  // mean = shape / rate.
  const double q = dist::qgamma(0.5, 2.0, 1.0, true, false);
  CHECK(dist::pgamma(q, 2.0, 1.0, true, false) == doctest::Approx(0.5));
  // density at mean is positive and finite.
  CHECK(dist::dgamma(2.0, 2.0, 1.0, false) > 0.0);
}

TEST_CASE("beta dpqr: U(0,1) special case (shape1=shape2=1)") {
  CHECK(dist::dbeta(0.3, 1.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(dist::pbeta(0.4, 1.0, 1.0, true, false) == doctest::Approx(0.4));
  CHECK(dist::qbeta(0.7, 1.0, 1.0, true, false) == doctest::Approx(0.7));
  // Asymmetric round-trip.
  const double q = dist::qbeta(0.6, 2.0, 5.0, true, false);
  CHECK(dist::pbeta(q, 2.0, 5.0, true, false) == doctest::Approx(0.6));
}
