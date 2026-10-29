#include "test_support.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("weibull") {
  auto g = mltest::load_distribution_golden("weibull");
  auto r = cppunivariate::weibull_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}


TEST_CASE("weibull: explicit shape start and automatic default") {
  auto g = mltest::load_distribution_golden("weibull");
  auto a = cppunivariate::weibull_mle(mltest::as_span(g.data), 1e-12, 200);
  auto b = cppunivariate::weibull_mle(mltest::as_span(g.data), 1e-12, 200, 1.2 * a.shape);
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-10));
}

TEST_CASE("weibull dpqr: known CDF value and quantile inverse") {
  // F(scale; shape, scale) = 1 - exp(-1) for any shape.
  CHECK(cppunivariate::distributions::pweibull(1.0, 2.0, 1.0, true, false) ==
        doctest::Approx(1.0 - std::exp(-1.0)));
  const double q = cppunivariate::distributions::qweibull(0.6, 1.5, 2.0, true, false);
  CHECK(cppunivariate::distributions::pweibull(q, 1.5, 2.0, true, false) == doctest::Approx(0.6));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("weibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("weibull",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dweibull(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pweibull(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qweibull(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rweibull(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x*x); Q=lambda l:m.sqrt(-l)
TEST_CASE("weibull: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pweibull(100,2,1,false,true) == doctest::Approx(-10000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qweibull(-10000,2,1,false,true) == doctest::Approx(100).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pweibull(1e-300,2,1,true,true) == doctest::Approx(2*std::log(1e-300)).epsilon(1e-12).scale(1e-300));
}
