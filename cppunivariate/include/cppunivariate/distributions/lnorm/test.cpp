#include "test_support.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/lnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("lnorm") {
  auto g = mltest::load_distribution_golden("lnorm");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("meanlog")).epsilon(mltest::kTol));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("sdlog")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("lnorm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/lnorm/fixture-outlier.json");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("lnorm dpqr: median = exp(meanlog)") {
  CHECK(cppunivariate::distributions::qlnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(1.0));
  const double q = cppunivariate::distributions::qlnorm(0.4, 1.0, 0.5, true, false);
  CHECK(cppunivariate::distributions::plnorm(q, 1.0, 0.5, true, false) == doctest::Approx(0.4));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("lnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("lnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlnorm(rng, p[0], p[1]); });
}

// mpmath generator: import mpmath as mp; mp.mp.dps=100
// Q=lambda z:mp.erfc(z/mp.sqrt(2))/2
// z=mp.findroot(lambda z:mp.log(Q(z))+800,40); [mp.exp(-z),mp.exp(z)]
TEST_CASE("lnorm: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qlnorm(-800.0, 0.0, 1.0, true, true) == doctest::Approx(4.767570450553154e-18).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qlnorm(-800.0, 0.0, 1.0, false, true) == doctest::Approx(2.0975044005568406e+17).epsilon(1e-11));
}

TEST_CASE("lnorm: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qlnorm(cppunivariate::special::kNegInf, 0.0, 1.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qlnorm(0.0, 0.0, 1.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qlnorm(cppunivariate::special::kNegInf, 0.0, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qlnorm(0.0, 0.0, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("lnorm: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dlnorm(1e100, 0.0, 1.0, true) == doctest::Approx(-26740.6680002246).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plnorm(1e100, 0.0, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::plnorm(1e100, 0.0, 1.0, false, true) == doctest::Approx(-26515.84871241671).epsilon(1e-11));
}
