#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/fatigue/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("fatigue") {
  auto g = mltest::load_distribution_golden("fatigue");
  auto r = cppunivariate::fatigue_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("alpha")));
  CHECK(r.beta == C_APPROX(g.estimates.at("beta")));
  // Wrapper hardcodes mu=0; golden also stores 0.
  CHECK(g.estimates.at("mu") == 0.0);
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("fatigue: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("fatigue",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfatigue(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfatigue(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfatigue(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfatigue(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// F=lambda x:m.erfc(-(m.sqrt(x)-1/m.sqrt(x))/m.sqrt(2))/2
TEST_CASE("fatigue: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pfatigue(1,1,1,0,true,true) == doctest::Approx(-std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qfatigue(-INFINITY,1,1,0,true,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pfatigue(INFINITY,1,1,0,false,true) == -INFINITY);
}

// mpmath: mp.dps=100; z=findroot(lambda z:log(erfc(z/sqrt(2))/2)+10000,141)
// h=3*z/2; lower=2/(sqrt(1+h*h)+h)**2; upper=2*(sqrt(1+h*h)+h)**2
TEST_CASE("fatigue: normal transform in extreme log tails") {
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,true,true) == doctest::Approx(1.1117514052021604e-5).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qfatigue(-10000,3,2,0,false,true) == doctest::Approx(359792.6641948019).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pfatigue(359792.6641948019,3,2,0,false,true) == doctest::Approx(-10000).epsilon(1e-12));
}
