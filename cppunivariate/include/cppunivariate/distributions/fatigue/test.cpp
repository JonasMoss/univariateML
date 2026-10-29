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
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("fatigue: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("fatigue",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfatigue(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfatigue(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfatigue(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfatigue(rng, p[0], p[1], p[2]); });
}

TEST_CASE("fatigue: exact held-beta alpha and held-alpha profile") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.2,.2,1,3};
  auto r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,nan,1);
  double ss=0;for(double v:x)ss+=v+1/v-2;
  CHECK(r.beta==1);CHECK(r.alpha==doctest::Approx(std::sqrt(ss/4)));
  for(double a:{.7,3.}) {
    r=cppunivariate::fatigue_mle(mltest::as_span(x),1e-10,200,a);
    CHECK(r.alpha==a);CHECK(std::isfinite(r.logLik));
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dfatigue(v,a,r.beta,0,true);
    CHECK(r.logLik==doctest::Approx(ll));
  }
  const std::vector<double> c{1,1};
  CHECK(std::isnan(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,nan,1).logLik));
  CHECK(std::isfinite(cppunivariate::fatigue_mle(mltest::as_span(c),1e-10,200,3,1).logLik));
}
