#include "test_support.hpp"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include "cppunivariate/distributions/invgauss/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rinvgauss samples are positive; sample mean ~ mu") {
  cppunivariate::distributions::StdRng<> rng(151);
  const int n = 20000;
  const double mu = 2.0, shape = 5.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvgauss(rng, mu, shape);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = mu for invgauss
  CHECK(std::abs(sum / n - mu) < 0.1);
}

TEST_CASE("invgauss") {
  auto g = mltest::load_distribution_golden("invgauss");
  auto r = cppunivariate::invgauss_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.lambda == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}
