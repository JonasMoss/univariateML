#include "test_support.hpp"
#include "cppunivariate/distributions/betapr/mle.hpp"
#include "cppunivariate/distributions/betapr/dpqr.hpp"
#include "cppunivariate/core/finite_diff.hpp"
#include <cmath>
#include <vector>
#include "oracle-fixtures.inc"

TEST_CASE("betapr golden and all three scores") {
  auto g = mltest::load_distribution_golden("betapr");
  auto r = cppunivariate::betapr_mle(mltest::as_span(g.data));
  CHECK(r.status == cppunivariate::BetaPrimeStatus::Ok);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("shape1")).epsilon(1e-6));
  CHECK(r.beta == doctest::Approx(g.estimates.at("shape2")).epsilon(1e-6));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(1e-6));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  cppunivariate::BetaPrimeLogCost cost{mltest::as_span(g.data)};
  const double theta[3] = {std::log(r.alpha), std::log(r.beta), std::log(r.scale)};
  double analytic[3], numeric[3];
  cost.gradient(theta, analytic);
  cppunivariate::finite_diff_gradient(cost, theta, numeric);
  double norm = 0;
  for (int j = 0; j < 3; ++j) {
    norm += numeric[j] * numeric[j];
    CHECK(std::abs(analytic[j] - numeric[j]) < 1e-3);
  }
  CHECK(std::sqrt(norm) < 1e-3);
}

TEST_CASE("betapr n20 independent slow oracle fixtures") {
  for (const auto &fixture : betapr_oracle_fixtures()) {
    auto r = cppunivariate::betapr_mle(mltest::as_span(fixture.x));
    CHECK(r.status == fixture.status);
    CHECK(r.logLik == doctest::Approx(fixture.logLik).epsilon(2e-8));
  }
}

TEST_CASE("betapr invalid data and scale density") {
  for (const auto &x : std::vector<std::vector<double>>{{}, {1}, {1,1}, {0,1}, {-1,2}, {1,INFINITY}})
    CHECK(cppunivariate::betapr_mle(mltest::as_span(x)).status ==
          cppunivariate::BetaPrimeStatus::InvalidStart);
  const double a=1.2, b=2.3, scale=4;
  CHECK(cppunivariate::distributions::dbetapr(2,a,b,true,scale) ==
        doctest::Approx(cppunivariate::distributions::dbetapr(2/scale,a,b,true)-std::log(scale)));
  const double q=cppunivariate::distributions::qbetapr(.3,a,b,true,false,scale);
  CHECK(cppunivariate::distributions::pbetapr(q,a,b,true,false,scale) == doctest::Approx(.3));
}

TEST_CASE("betapr: historical fixed scale golden and equivariance") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/betapr/fixed-scale-golden.json");
  auto r = cppunivariate::betapr_mle_fixed_scale(mltest::as_span(g.data), 1);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("shape1")).epsilon(1e-10));
  CHECK(r.beta == doctest::Approx(g.estimates.at("shape2")).epsilon(1e-10));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(r.scale == 1);
  for (auto &v : g.data) v *= 7;
  auto moved = cppunivariate::betapr_mle_fixed_scale(mltest::as_span(g.data), 7);
  CHECK(moved.alpha == doctest::Approx(r.alpha).epsilon(1e-10));
  CHECK(moved.beta == doctest::Approx(r.beta).epsilon(1e-10));
  CHECK(moved.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(7.0)).epsilon(1e-10));
  for (double bad : {0.0, -1.0, double(INFINITY), double(NAN)})
    CHECK(cppunivariate::betapr_mle_fixed_scale(mltest::as_span(g.data), bad).status == cppunivariate::BetaPrimeStatus::InvalidStart);
}

TEST_CASE("betapr: fixed-scale independent scipy oracle") {
  // scipy minimization of the reference log density, in log parameters.
  std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  auto r = cppunivariate::betapr_mle_fixed_scale(mltest::as_span(x), 1);
  CHECK(r.logLik == doctest::Approx(-13.790426340537064).epsilon(1e-10));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("betapr: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("betapr",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbetapr(x, p[0], p[1], log, p[2]); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbetapr(x, p[0], p[1], lower, log, p[2]); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbetapr(x, p[0], p[1], lower, log, p[2]); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbetapr(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// S=lambda x: 1/(1+x); Q=lambda l: mp.expm1(-l)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("betapr: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pbetapr(1e200,1,1,false,true) == doctest::Approx(-200*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dbetapr(1e200,1,1,true) == doctest::Approx(-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qbetapr(-500,1,1,false,true) == doctest::Approx(std::exp(500.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qbetapr(-10000,1,1,true,true) == 0);
  CHECK(cppunivariate::distributions::qbetapr(-INFINITY,1,1,false,true) == INFINITY);
  CHECK(cppunivariate::distributions::dbetapr(0,.5,2,true) == INFINITY);
  CHECK(cppunivariate::distributions::dbetapr(0,1,2,false,3) == doctest::Approx(2.0/3).scale(0.0));
  // mp.mp.dps=400; (4/mp.mpf('1e-300'))**(mp.mpf(1)/3)
  CHECK(cppunivariate::distributions::qbetapr(1e-300,2,3,false,false) == doctest::Approx(std::exp((std::log(4.0)+300*std::log(10.0))/3)).scale(0.0).epsilon(1e-10));
  // mp.sqrt(mp.mpf('1e-300')/465): leading lower tail I_x(2,30)=465*x^2.
  CHECK(cppunivariate::distributions::qbetapr(1e-300,2,30,true,false) == doctest::Approx(std::sqrt(1e-300/465)).scale(0.0).epsilon(1e-10));

}
