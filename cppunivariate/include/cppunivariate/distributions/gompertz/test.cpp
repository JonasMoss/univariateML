#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/gompertz/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("gompertz") {
  auto g = mltest::load_distribution_golden("gompertz");
  auto r = cppunivariate::gompertz_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.a == C_APPROX(g.estimates.at("a")));
  CHECK(r.b == C_APPROX(g.estimates.at("b")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("gompertz: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gompertz",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgompertz(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgompertz(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgompertz(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgompertz(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:-m.expm1(x); Q=lambda l:m.log1p(-l)
TEST_CASE("gompertz: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pgompertz(1e-300,1,1,true,true) == doctest::Approx(std::log(1e-300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qgompertz(-10000,1,1,false,true) == doctest::Approx(std::log(10001.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dgompertz(0,1,1,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
}
