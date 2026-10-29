#include "test_support.hpp"
#include "cppunivariate/distributions/rayleigh/mle.hpp"
#include "cppunivariate/distributions/rayleigh/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rrayleigh samples are positive; mean = sigma * sqrt(pi/2)") {
  cppunivariate::distributions::StdRng<> rng(103);
  const int n = 20000;
  const double sigma = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rrayleigh(rng, sigma);
    CHECK(x >= 0.0);
    sum += x;
  }
  const double expected = sigma * std::sqrt(M_PI / 2.0);
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("rayleigh") {
  auto g = mltest::load_distribution_golden("rayleigh");
  auto r = cppunivariate::rayleigh_mle(mltest::as_span(g.data));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("rayleigh: any zero makes the likelihood identically zero") {
  for (auto x : {std::vector<double>{0,0,1,2}, std::vector<double>{0,1}, std::vector<double>{0,0}})
    CHECK(cppunivariate::rayleigh_mle(mltest::as_span(x)).status == cppunivariate::RayleighStatus::ZeroLikelihood);
}
