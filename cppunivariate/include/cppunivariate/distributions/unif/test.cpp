#include "test_support.hpp"
#include "cppunivariate/distributions/unif/mle.hpp"
#include "cppunivariate/distributions/unif/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("unif") {
  auto g = mltest::load_distribution_golden("unif");
  auto r = cppunivariate::unif_mle(mltest::as_span(g.data));
  CHECK(r.min == doctest::Approx(g.estimates.at("min")).epsilon(mltest::kTol));
  CHECK(r.max == doctest::Approx(g.estimates.at("max")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("unif dpqr: density, CDF, quantile") {
  CHECK(cppunivariate::distributions::dunif(0.5, 0.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::punif(0.3, 0.0, 1.0, true, false) == doctest::Approx(0.3));
  CHECK(cppunivariate::distributions::qunif(0.8, 0.0, 1.0, true, false) == doctest::Approx(0.8));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("unif: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("unif",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dunif(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::punif(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qunif(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::runif_(rng, p[0], p[1]); });
}
