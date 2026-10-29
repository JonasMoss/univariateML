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
TEST_CASE("pareto: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// S=lambda x: x**-4; Q=lambda l: mp.exp(-l/4)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("pareto: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::ppareto(1e200,4,1,false,true) == doctest::Approx(-800*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dpareto(1e200,4,1,true) == doctest::Approx(std::log(4.0)-1000*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto(-1000,4,1,false,true) == doctest::Approx(std::exp(250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto(-INFINITY,4,3,true,true) == 3);
  // mp.mp.dps=400; a=mp.mpf(float(1e-320)); h=-mp.log1p(-mp.exp(-750))/a
  // Quantile mp.exp(h); log lower tail at x=e tends to mp.log(a).
  CHECK(cppunivariate::distributions::qpareto(-750,1e-320,1,true,true) == doctest::Approx(std::exp(std::exp(-750-std::log(1e-320)))).scale(0.0));
  CHECK(cppunivariate::distributions::ppareto(std::exp(1.0),1e-320,1,true,true) == doctest::Approx(std::log(1e-320)).scale(0.0));

}
