#include "test_support.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include "cppunivariate/distributions/invweibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("rinvweibull samples are positive; CDF round-trip at p=0.5") {
  cppunivariate::distributions::StdRng<> rng(157);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvweibull(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qinvweibull(0.5, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pinvweibull(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.5) < 1e-10);
}

TEST_CASE("invweibull") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto r =
      cppunivariate::invweibull_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}


TEST_CASE("invweibull: explicit shape start and automatic default") {
  auto g = mltest::load_distribution_golden("invweibull");
  auto a = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200);
  auto b = cppunivariate::invweibull_mle(mltest::as_span(g.data), 1e-12, 200, 1.2 * a.shape);
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-10));
}
