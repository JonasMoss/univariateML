#include "test_support.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/norm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("norm") {
  auto g = mltest::load_distribution_golden("norm");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.sd == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("norm: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/norm/fixture-outlier.json");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sd == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

TEST_CASE("norm dpqr: standard-normal CDF / quantile round-trip") {
  CHECK(cppunivariate::distributions::pnorm(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  CHECK(cppunivariate::distributions::qnorm(0.5, 0.0, 1.0, true, false) == doctest::Approx(0.0));
  const double q = cppunivariate::distributions::qnorm(0.7, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::pnorm(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("norm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("norm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnorm(rng, p[0], p[1]); });
}

// mpmath generator: import mpmath as mp; mp.mp.dps=100
// Q=lambda z:mp.erfc(z/mp.sqrt(2))/2
// z=mp.findroot(lambda z:mp.log(Q(z))+800,40); [-z,z]
TEST_CASE("norm: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qnorm(-800.0, 0.0, 1.0, true, true) == doctest::Approx(-39.88469483825668).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qnorm(-800.0, 0.0, 1.0, false, true) == doctest::Approx(39.88469483825668).epsilon(1e-11));
}

TEST_CASE("norm: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qnorm(cppunivariate::special::kNegInf, 0.0, 1.0, true, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qnorm(0.0, 0.0, 1.0, false, true) == cppunivariate::special::kNegInf);
  CHECK(cppunivariate::distributions::qnorm(cppunivariate::special::kNegInf, 0.0, 1.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qnorm(0.0, 0.0, 1.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("norm: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dnorm(1e100, 0.0, 1.0, true) == doctest::Approx(-5e+199).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pnorm(1e100, 0.0, 1.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::pnorm(1e100, 0.0, 1.0, false, true) == doctest::Approx(-5e+199).epsilon(1e-11));
}

// import mpmath as mp; mp.mp.dps=100
// -2-mp.log(mp.mpf(float(1e308)))-mp.log(2*mp.pi)/2
// mp.log(mp.erfc(mp.sqrt(2))/2)
TEST_CASE("norm: standardization and affine overflow") {
  CHECK(cppunivariate::distributions::dnorm(1e308,-1e308,1e308,true) == doctest::Approx(-712.1151471753707).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pnorm(1e308,-1e308,1e308,false,true) == doctest::Approx(-3.783184333682032).epsilon(1e-12));
}
