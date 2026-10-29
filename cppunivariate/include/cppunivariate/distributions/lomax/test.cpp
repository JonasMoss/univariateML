// lomax: golden regression, start value, existence condition, d/p/q/r.

#include "test_support.hpp"

#include "cppunivariate/distributions/lomax/dpqr.hpp"
#include "cppunivariate/distributions/lomax/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <vector>

TEST_CASE("lomax: golden") {
  auto g = mltest::load_distribution_golden("lomax");
  auto r = cppunivariate::lomax_mle(mltest::as_span(g.data), mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::Ok);
  CHECK(r.lambda == doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kTol));
  CHECK(r.kappa == doctest::Approx(g.estimates.at("kappa")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lomax: an explicit start converges to the same fit") {
  auto g = mltest::load_distribution_golden("lomax");
  const auto x = mltest::as_span(g.data);
  auto a = cppunivariate::lomax_mle(x, 1e-12, 200);
  auto b = cppunivariate::lomax_mle(x, 1e-12, 200, 3.0 * a.lambda);
  CHECK(b.status == cppunivariate::LomaxStatus::Ok);
  CHECK(b.lambda == doctest::Approx(a.lambda).epsilon(1e-8));
  CHECK(b.kappa == doctest::Approx(a.kappa).epsilon(1e-8));
}

TEST_CASE("lomax: concentrated sample selects the exponential limit") {
  const std::vector<double> x = {1.0, 1.1, 0.9, 1.05, 0.95};
  auto r = cppunivariate::lomax_mle({x.data(), x.size()}, mltest::kFitTol,
                                    mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::LomaxStatus::MeanVarianceCondition);
}

TEST_CASE("lomax: rlomax is positive; qlomax and plomax round-trip") {
  cppunivariate::distributions::StdRng<> rng(127);
  for (int i = 0; i < 5000; ++i) {
    CHECK(cppunivariate::distributions::rlomax(rng, 2.0, 3.0) > 0.0);
  }
  const double q = cppunivariate::distributions::qlomax(0.7, 2.0, 3.0, true, false);
  const double back = cppunivariate::distributions::plomax(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}
