#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("norm") {
  auto g = mltest::load_distribution_golden("norm");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.sd == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("norm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/norm/fixture-outlier.json");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sd == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("norm dpqr: standard-normal CDF / quantile round-trip") {
  CHECK(cppunivariate::distributions::pnorm(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(cppunivariate::distributions::qnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = cppunivariate::distributions::qnorm(0.7, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pnorm(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

TEST_CASE("norm: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("norm");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::norm_mle(mltest::as_span(g.data),((mask & 1) ? g.estimates.at("mean") : nan),((mask & 2) ? g.estimates.at("sd") : nan));
    if(mask & 1)CHECK(r.mean==g.estimates.at("mean"));
    if(mask & 2)CHECK(r.sd==g.estimates.at("sd"));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("norm: held scale removes constant-sample collapse") {
  const std::vector<double> x={2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::norm_mle(mltest::as_span(x),2,nan).status==cppunivariate::NormStatus::Unbounded);
  CHECK(cppunivariate::norm_mle(mltest::as_span(x),nan,1).status==cppunivariate::NormStatus::Ok);
}
