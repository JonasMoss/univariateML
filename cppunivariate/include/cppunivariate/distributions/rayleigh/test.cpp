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

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("rayleigh: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("rayleigh",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::drayleigh(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::prayleigh(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qrayleigh(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rrayleigh(rng, p[0]); });
}

TEST_CASE("rayleigh: any zero makes the likelihood identically zero") {
  for (auto x : {std::vector<double>{0,0,1,2}, std::vector<double>{0,1}, std::vector<double>{0,0}})
    CHECK(cppunivariate::rayleigh_mle(mltest::as_span(x)).status == cppunivariate::RayleighStatus::ZeroLikelihood);
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x*x/2); Q=lambda l:m.sqrt(-2*l)
TEST_CASE("rayleigh: extreme tail regressions") {
  CHECK(cppunivariate::distributions::prayleigh(100,1,false,true) == doctest::Approx(-5000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qrayleigh(-5000,1,false,true) == doctest::Approx(100).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::prayleigh(1e-300,1,true,true) == doctest::Approx(2*std::log(1e-300)-std::log(2.0)).epsilon(1e-12).scale(1e-300));
}
