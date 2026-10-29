#include "test_support.hpp"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include "cppunivariate/distributions/llogis/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("rllogis samples are positive; CDF round-trip at p=0.7") {
  cppunivariate::distributions::StdRng<> rng(163);
  for (int i = 0; i < 5000; ++i) {
    const double x = cppunivariate::distributions::rllogis(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qllogis(0.7, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pllogis(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("llogis") {
  auto g = mltest::load_distribution_golden("llogis");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("llogis: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/llogis/fixture-outlier.json");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.rate == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}


// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("llogis: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("llogis",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dllogis(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pllogis(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qllogis(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rllogis(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: 1/(1+x**-4); Q=lambda l: mp.exp((l-mp.log1p(-mp.exp(l)))/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("llogis: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pllogis(1e200,4,1,false,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dllogis(1e200,4,1,true) == doctest::Approx(std::log(4.0)-1000*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qllogis(-1000,4,1,true,true) == doctest::Approx(std::exp(-250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qllogis(-INFINITY,4,1,false,true) == INFINITY);
}
