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
  CHECK(cppunivariate::distributions::dbetapr(2, a, b, scale, true) ==
        doctest::Approx(cppunivariate::distributions::dbetapr(2/scale, a, b, 1.0, true)-std::log(scale)));
  const double q=cppunivariate::distributions::qbetapr(.3, a, b, scale, true, false);
  CHECK(cppunivariate::distributions::pbetapr(q, a, b, scale, true, false) == doctest::Approx(.3));
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
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbetapr(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbetapr(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbetapr(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbetapr(rng, p[0], p[1], p[2]); });
}

TEST_CASE("betapr: every new hold and exact scale score") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.05,.05,.3,2,15};
  for(double a:{nan,.7})for(double b:{nan,1.2})for(double s:{nan,1.3}) {
    if(std::isnan(a)&&std::isnan(b))continue;
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),a,b,s);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
    if(!std::isnan(a))CHECK(r.alpha==a);
    if(!std::isnan(b))CHECK(r.beta==b);
    if(!std::isnan(s))CHECK(r.scale==s);
    double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbetapr(v, r.alpha, r.beta, r.scale, true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-9));
    if(std::isnan(s)) {
      double score=0;for(double v:x)score+=-r.alpha+(r.alpha+r.beta)*v/(v+r.scale);
      CHECK(std::abs(score)<1e-5);
    }
  }
  CHECK(cppunivariate::betapr_mle_holds(mltest::as_span(x),0,nan,nan).status==cppunivariate::BetaPrimeStatus::InvalidStart);
}
TEST_CASE("betapr: held shapes retain their own escaping faces") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{2,2};
  const auto g=cppunivariate::betapr_mle_holds(mltest::as_span(x),.7,nan,nan);
  CHECK(g.status==cppunivariate::BetaPrimeStatus::GammaBoundary);
  CHECK(g.logLik==doctest::Approx(2*(.7*std::log(.7/2)-std::lgamma(.7)-.3*std::log(2.)-.7)));
  const auto i=cppunivariate::betapr_mle_holds(mltest::as_span(x),nan,1.2,nan);
  CHECK(i.status==cppunivariate::BetaPrimeStatus::InvGammaBoundary);
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// S=lambda x: 1/(1+x); Q=lambda l: mp.expm1(-l)
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("betapr: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pbetapr(1e200, 1, 1, 1.0, false, true) == doctest::Approx(-200*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dbetapr(1e200, 1, 1, 1.0, true) == doctest::Approx(-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qbetapr(-500, 1, 1, 1.0, false, true) == doctest::Approx(std::exp(500.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qbetapr(-10000, 1, 1, 1.0, true, true) == 0);
  CHECK(cppunivariate::distributions::qbetapr(-INFINITY, 1, 1, 1.0, false, true) == INFINITY);
  CHECK(cppunivariate::distributions::dbetapr(0, .5, 2, 1.0, true) == INFINITY);
  CHECK(cppunivariate::distributions::dbetapr(0, 1, 2, 3, false) == doctest::Approx(2.0/3).scale(0.0));
  // mp.mp.dps=400; (4/mp.mpf('1e-300'))**(mp.mpf(1)/3)
  CHECK(cppunivariate::distributions::qbetapr(1e-300, 2, 3, 1.0, false, false) == doctest::Approx(std::exp((std::log(4.0)+300*std::log(10.0))/3)).scale(0.0).epsilon(1e-10));
  // mp.sqrt(mp.mpf('1e-300')/465): leading lower tail I_x(2,30)=465*x^2.
  CHECK(cppunivariate::distributions::qbetapr(1e-300, 2, 30, 1.0, true, false) == doctest::Approx(std::sqrt(1e-300/465)).scale(0.0).epsilon(1e-10));

}

TEST_CASE("betapr: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("betapr");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::betapr_mle_fixed_scale(x,1,1e-8,1).status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

TEST_CASE("betapr: P3 a capped face solve cannot produce a beta-prime fit") {
  const std::vector<double> values{.01,.2,.3,.5,2,10,50};
  const auto r=cppunivariate::betapr_mle(mltest::as_span(values),1e-10,1);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(std::isnan(r.alpha));
  CHECK(std::isnan(r.beta));
  CHECK(std::isfinite(r.scale));
}

TEST_CASE("betapr: face comparison cannot resolve rounded extreme objectives") {
  const double magnitude=1e99;
  CHECK(cppunivariate::compare_face(-magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::NumericalTie);
  CHECK(cppunivariate::compare_face(-magnitude,-2*magnitude,2e-8)==cppunivariate::FaceComparison::FirstWins);
  CHECK(cppunivariate::compare_face(-2*magnitude,-magnitude,2e-8)==cppunivariate::FaceComparison::SecondWins);
  CHECK(cppunivariate::compare_face(-10.,-10.,2e-8)==cppunivariate::FaceComparison::FirstWins);
}

TEST_CASE("betapr: audited huge held-parameter magnitude") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x{1,2};
  cppunivariate::span s{x.data(),x.size()};
  const auto r=cppunivariate::betapr_mle_holds(s,1e100,nan,nan);
  CHECK((r.status==cppunivariate::BetaPrimeStatus::Ok || r.status==cppunivariate::BetaPrimeStatus::BfgsFailure));
  CHECK(r.status!=cppunivariate::BetaPrimeStatus::GammaBoundary);
}

TEST_CASE("beta-prime held unit shapes inherit Lomax certificates") {
 const std::vector<double> x{1,2,3,4};
 const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),1,NAN,NAN,1e-10,1000);
 const auto p=cppunivariate::lomax_mle(mltest::as_span(x),1e-10,1000);
 REQUIRE(r.status==cppunivariate::BetaPrimeStatus::ExpBoundary);
 CHECK(r.stop_detail.limit_loglik==doctest::Approx(p.stop_detail.limit_loglik));
 std::vector<double> y;for(double v:x)y.push_back(1/v);
 const auto inv=cppunivariate::betapr_mle_holds(mltest::as_span(y),NAN,1,NAN,1e-10,1000);
 CHECK(inv.status==cppunivariate::BetaPrimeStatus::InvExpBoundary);
 CHECK(inv.stop_detail.limit_coef==r.stop_detail.limit_coef);
}

TEST_CASE("beta-prime unit beta shapes preserve finite Lomax coefficients") {
 const auto g=mltest::load_distribution_golden("lomax");
 const auto p=cppunivariate::lomax_mle(mltest::as_span(g.data),1e-10,1000);
 REQUIRE(p.status==cppunivariate::LomaxStatus::Ok);
 const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(g.data),1,NAN,NAN,1e-10,1000);
 REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
 CHECK(r.alpha==1);CHECK(r.beta==doctest::Approx(p.kappa));CHECK(r.scale==doctest::Approx(1/p.lambda));
 double ll=0,jac=0;std::vector<double> reciprocal;
 for(double v:g.data){ll+=cppunivariate::distributions::dbetapr(v,r.alpha,r.beta,r.scale,true);reciprocal.push_back(1/v);jac+=2*std::log(v);}
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-8));
 const auto inv=cppunivariate::betapr_mle_holds(mltest::as_span(reciprocal),NAN,1,NAN,1e-10,1000);
 REQUIRE(inv.status==cppunivariate::BetaPrimeStatus::Ok);
 CHECK(inv.alpha==doctest::Approx(p.kappa));CHECK(inv.beta==1);
 CHECK(inv.scale==doctest::Approx(p.lambda));CHECK(inv.logLik==doctest::Approx(p.logLik+jac).epsilon(1e-8));
}
