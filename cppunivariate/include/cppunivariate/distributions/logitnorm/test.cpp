#include "test_support.hpp"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include "cppunivariate/distributions/logitnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rlogitnorm samples lie in (0, 1) and match the CDF at median") {
  cppunivariate::distributions::StdRng<> rng(7);
  const int n = 5000;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rlogitnorm(rng, 0.0, 1.0);
    CHECK(x > 0.0);
    CHECK(x < 1.0);
    sum += x;
  }
  // mu=0 means logit(X) ~ N(0, 1), so median(X) = logistic(0) = 0.5.
  // Empirical mean is biased toward 0.5 but with high spread; sanity-check
  // the CDF at 0.5 returns 0.5.
  const double cdf_half =
      cppunivariate::distributions::plogitnorm(0.5, 0.0, 1.0, true, false);
  CHECK(std::abs(cdf_half - 0.5) < 1e-12);
  CHECK(std::abs(sum / n - 0.5) < 0.05);
}

TEST_CASE("logitnorm") {
  auto g = mltest::load_distribution_golden("logitnorm");
  auto r = cppunivariate::logitnorm_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}
