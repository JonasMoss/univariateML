#include "test_support.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include "cppunivariate/distributions/power/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpower samples lie in (0, alpha); mean = alpha*beta/(beta+1)") {
  cppunivariate::distributions::StdRng<> rng(109);
  const int n = 20000;
  const double alpha = 3.0, beta = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpower(rng, alpha, beta);
    CHECK(x > 0.0);
    CHECK(x < alpha);
    sum += x;
  }
  const double expected = alpha * beta / (beta + 1.0);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("power") {
  auto g = mltest::load_distribution_golden("power");
  auto r = cppunivariate::power_mle(mltest::as_span(g.data), 1.490116e-08);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kTol));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("power: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("power",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpower(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppower(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpower(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpower(rng, p[0], p[1]); });
}
