#include "test_support.hpp"
#include "cppunivariate/distributions/cauchy/mle.hpp"
#include "cppunivariate/distributions/cauchy/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("cauchy") {
  auto g = mltest::load_distribution_golden("cauchy");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("location")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("cauchy: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/cauchy/fixture-outlier.json");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
}

TEST_CASE("cauchy: three observations and nonexistent MLE") {
  const std::vector<double> x{-2.2425190273413, 0.723253413022922, -2.56072856385821};
  auto r = cppunivariate::fit_cauchy(mltest::as_span(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
  CHECK(r.mu == doctest::Approx(-2.369470147).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(0.272740500).epsilon(1e-4));
  CHECK(r.logLik >= -4.99686179 - 1e-6);
  for (const std::vector<double> &bad : {std::vector<double>{}, {1}, {1, 2}, {1, 1, 2}, {1, 1, 2, 3}}) {
    CHECK(cppunivariate::fit_cauchy(mltest::as_span(bad), mltest::kFitTol, mltest::kFitMaxit).status == cppunivariate::CauchyStatus::NoMLE);
  }
}

TEST_CASE("cauchy dpqr: CDF symmetric at location") {
  CHECK(cppunivariate::distributions::pcauchy(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(cppunivariate::distributions::qcauchy(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = cppunivariate::distributions::qcauchy(0.75, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pcauchy(q, 0.0, 1.0, true, false) == doctest::Approx(0.75));
}
