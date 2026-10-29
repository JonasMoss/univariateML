#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/gamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("gamma") {
  auto g = mltest::load_distribution_golden("gamma");
  auto r = cppunivariate::gamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("gamma dpqr: rate parameterisation round-trip") {
  // mean = shape / rate.
  const double q = cppunivariate::distributions::qgamma(0.5, 2.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pgamma(q, 2.0, 1.0, true, false) == doctest::Approx(0.5));
  // density at mean is positive and finite.
  CHECK(cppunivariate::distributions::dgamma(2.0, 2.0, 1.0, false) > 0.0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("gamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("gamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgamma(rng, p[0], p[1]); });
}
