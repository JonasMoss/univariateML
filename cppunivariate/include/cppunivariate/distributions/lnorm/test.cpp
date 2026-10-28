#include "test_support.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("lnorm") {
  auto g = mltest::load_distribution_golden("lnorm");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("meanlog")).epsilon(mltest::kTol));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("sdlog")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lnorm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/lnorm/fixture-outlier.json");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("lnorm dpqr: median = exp(meanlog)") {
  CHECK(cppunivariate::distributions::qlnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(1.0));
  const double q = cppunivariate::distributions::qlnorm(0.4, 1.0, 0.5, true, false);
  CHECK(cppunivariate::distributions::plnorm(q, 1.0, 0.5, true, false) == doctest::Approx(0.4));
}
