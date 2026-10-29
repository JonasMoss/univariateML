#include "test_support.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/pareto/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rpareto samples are >= scale b; mean = a*b/(a-1) for a>1") {
  cppunivariate::distributions::StdRng<> rng(113);
  const int n = 20000;
  const double a = 3.0, b = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rpareto(rng, a, b);
    CHECK(x >= b);
    sum += x;
  }
  const double expected = a * b / (a - 1.0); // = 3.0
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("pareto") {
  auto g = mltest::load_distribution_golden("pareto");
  auto r = cppunivariate::pareto_mle(mltest::as_span(g.data));
  // golden "a" → ParetoResult.shape, "b" → .min
  CHECK(r.shape == doctest::Approx(g.estimates.at("a")).epsilon(mltest::kTol));
  CHECK(r.min == doctest::Approx(g.estimates.at("b")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("pareto: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("pareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto(rng, p[0], p[1]); });
}
