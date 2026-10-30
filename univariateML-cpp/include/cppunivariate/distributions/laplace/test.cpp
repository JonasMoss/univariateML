#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/laplace/mle.hpp"
#include "cppunivariate/distributions/laplace/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rlaplace samples are symmetric around mu, var = 2*sigma^2") {
  cppunivariate::distributions::StdRng<> rng(101);
  const int n = 20000;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rlaplace(rng, 1.0, 2.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean - 1.0) < 0.1);
  CHECK(std::abs(var - 8.0) < 0.5); // 2*sigma^2 = 8
  // CDF at mu should be exactly 0.5.
  CHECK(std::abs(
            cppunivariate::distributions::plaplace(1.0, 1.0, 2.0, true, false) -
            0.5) < 1e-12);
}

TEST_CASE("laplace") {
  auto g = mltest::load_distribution_golden("laplace");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("laplace: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/laplace/fixture-outlier.json");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("laplace: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("laplace",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlaplace(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plaplace(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlaplace(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlaplace(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:-x-m.log(2); Q=lambda l:l+m.log(2)
TEST_CASE("laplace: extreme tail regressions") {
  CHECK(cppunivariate::distributions::plaplace(1000,0,1,false,true) == doctest::Approx(-1000-std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qlaplace(-10000,0,1,true,true) == doctest::Approx(-10000+std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qlaplace(-10000,0,1,false,true) == doctest::Approx(10000-std::log(2.0)).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(2))))
// Invert the family's CDF at this p, retaining p-m.mpf('.5').
TEST_CASE("laplace: rounded log half retains median offset") {
  CHECK(cppunivariate::distributions::qlaplace(-std::log(2.0),0,1,true,true) == doctest::Approx(2.3190468138462997e-17).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qlaplace(-std::log(2.0),0,1,false,true) == doctest::Approx(-2.3190468138462997e-17).epsilon(1e-12));
}


TEST_CASE("laplace: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("laplace");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::laplace_mle(mltest::as_span(g.data),((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("laplace: held scale removes constant-sample collapse") {
  const std::vector<double> x={2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::laplace_mle(mltest::as_span(x),2,nan).status==cppunivariate::LaplaceStatus::Unbounded);
  CHECK(cppunivariate::laplace_mle(mltest::as_span(x),nan,1).status==cppunivariate::LaplaceStatus::Ok);
}
