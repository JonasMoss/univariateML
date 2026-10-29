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
TEST_CASE("gamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("gamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgamma(rng, p[0], p[1]); });
}

// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(2,0,z)/mp.gamma(2)
// Q=lambda z:mp.gammainc(2,z,mp.inf)/mp.gamma(2)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/2))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [lo/2,hi/2]
TEST_CASE("gamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 2.0, true, true) == doctest::Approx(1.3542294089587789e-174).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qgamma(-800.0, 2.0, 2.0, false, true) == doctest::Approx(403.34709175055565).epsilon(1e-11));
}

TEST_CASE("gamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 2.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 2.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qgamma(cppunivariate::special::kNegInf, 2.0, 2.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qgamma(0.0, 2.0, 2.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("gamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dgamma(1e100, 2.0, 2.0, true) == doctest::Approx(-2e+100).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 2.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pgamma(1e100, 2.0, 2.0, false, true) == doctest::Approx(-2e+100).epsilon(1e-11));
}
