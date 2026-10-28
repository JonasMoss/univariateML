#include "test_support.hpp"
#include "cppunivariate/distributions/gumbel/mle.hpp"
#include "cppunivariate/distributions/gumbel/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("rgumbel samples; mean = mu + sigma*gamma_const, var = pi^2*sigma^2/6") {
  cppunivariate::distributions::StdRng<> rng(131);
  const int n = 20000;
  const double mu = 1.0, sigma = 2.0;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rgumbel(rng, mu, sigma);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  const double euler_mascheroni = 0.5772156649015329;
  const double expected_mean = mu + sigma * euler_mascheroni;
  const double expected_var = M_PI * M_PI * sigma * sigma / 6.0;
  CHECK(std::abs(mean - expected_mean) < 0.1);
  CHECK(std::abs(var - expected_var) < 0.5);
}

TEST_CASE("gumbel") {
  auto g = mltest::load_distribution_golden("gumbel");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("gumbel: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/gumbel/fixture-outlier.json");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

