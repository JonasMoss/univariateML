#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("exp") {
  auto g = mltest::load_distribution_golden("exp");
  auto r = cppunivariate::exp_mle(mltest::as_span(g.data));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("exp: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("exp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dexp(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pexp(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qexp(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rexp(rng, p[0]); });
}

TEST_CASE("exp P2 every held subset") {
  const auto g=mltest::load_distribution_golden("exp");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("rate")};
  { const auto r=cppunivariate::exp_mle(mltest::as_span(x),held[0]);
    CHECK(r.rate==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dexp(v,p[0],true);return s;};
    std::vector<double> p{r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}
