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

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("invweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("invweibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvweibull(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.exp(-1/x**2); Q=lambda l:1/m.sqrt(-l)
TEST_CASE("invweibull: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pinvweibull(1e300,2,1,false,true) == doctest::Approx(-2*std::log(1e300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qinvweibull(-10000,2,1,true,true) == doctest::Approx(0.01).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dinvweibull(INFINITY,2,1,true) == -INFINITY);
}
