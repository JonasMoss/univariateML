#include "test_support.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("exp") {
  auto g = mltest::load_distribution_golden("exp");
  auto r = cppunivariate::exp_mle(mltest::as_span(g.data));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}
