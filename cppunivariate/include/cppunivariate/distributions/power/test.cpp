#include "test_support.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include "cppunivariate/distributions/power/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpower samples lie in (0, alpha); mean = alpha*beta/(beta+1)") {
  cppunivariate::distributions::StdRng<> rng(109);
  const int n = 20000;
  const double alpha = 3.0, beta = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpower(rng, alpha, beta);
    CHECK(x > 0.0);
    CHECK(x < alpha);
    sum += x;
  }
  const double expected = alpha * beta / (beta + 1.0);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("power") {
  auto g = mltest::load_distribution_golden("power");
  auto r = cppunivariate::power_mle(mltest::as_span(g.data), 1.490116e-08);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kTol));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("power: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("power",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpower(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppower(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpower(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpower(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: x**4; Q=lambda l: mp.exp(l/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("power: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::ppower(1e-200,1,4,true,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpower(-1000,1,4,true,true) == doctest::Approx(std::exp(-250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpower(-INFINITY,3,4,false,true) == 3);
  CHECK(std::isnan(cppunivariate::distributions::qpower(1.1,1,4,true,false)));
  CHECK(cppunivariate::distributions::dpower(0,3,.5,true) == INFINITY);
  CHECK(cppunivariate::distributions::dpower(3,3,2,false) == doctest::Approx(2.0/3).scale(0.0));
  // mp.mp.dps=400; a=mp.mpf(float(1e-320)); h=-mp.log1p(-mp.exp(-750))/a
  // Quantile mp.exp(-h); log upper tail at x=1/e tends to mp.log(a).
  CHECK(cppunivariate::distributions::qpower(-750,1,1e-320,false,true) == doctest::Approx(std::exp(-std::exp(-750-std::log(1e-320)))).scale(0.0));
  CHECK(cppunivariate::distributions::ppower(std::exp(-1.0),1,1e-320,false,true) == doctest::Approx(std::log(1e-320)).scale(0.0));

}
