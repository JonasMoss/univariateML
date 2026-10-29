#include "test_support.hpp"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include "cppunivariate/distributions/invgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("invgamma") {
  auto g = mltest::load_distribution_golden("invgamma");
  auto r = cppunivariate::invgamma_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}


// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("invgamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("invgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvgamma(rng, p[0], p[1]); });
}
