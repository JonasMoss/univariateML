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
  CHECK(r.alpha == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.beta == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_betapr).scale(0));
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

TEST_CASE("betapr conditional certificate closes finite tails and transports bounds") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  std::vector<double> inverse;double jac=0;
  for(double v:x){inverse.push_back(1/v);jac+=2*std::log(v);}
  for(double a:{.7,2.,5.}) {
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),a,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
    REQUIRE(!r.stop_detail.candidates.empty());
    CHECK(r.stop_detail.loglik_lower<=r.logLik);
    CHECK(r.stop_detail.loglik_upper>=r.logLik);
    double score=0;
    for(double v:x)score+=cppunivariate::special::digamma(a+r.beta)-cppunivariate::special::digamma(r.beta)-std::log1p(v/r.scale);
    CHECK(std::abs(score)<1e-5);
    const auto inv=cppunivariate::betapr_mle_holds(mltest::as_span(inverse),NAN,a,NAN,1e-10,1000);
    REQUIRE(inv.status==cppunivariate::BetaPrimeStatus::Ok);
    CHECK(inv.alpha==doctest::Approx(r.beta).epsilon(1e-6));
    CHECK(inv.scale==doctest::Approx(1/r.scale).epsilon(1e-6));
    CHECK(inv.logLik==doctest::Approx(r.logLik+jac).epsilon(1e-10));
    CHECK(inv.stop_detail.candidates.front().model==cppunivariate::StopModel::InvGamma);
    CHECK(inv.stop_detail.loglik_lower==doctest::Approx(r.stop_detail.loglik_lower+jac).epsilon(1e-10));
    CHECK(inv.stop_detail.loglik_upper==doctest::Approx(r.stop_detail.loglik_upper+jac).epsilon(1e-10));
    CHECK(inv.stop_detail.segment_loglik_upper==doctest::Approx(r.stop_detail.segment_loglik_upper+jac).epsilon(1e-8));
  }
}

TEST_CASE("betapr conditional certificate retains faces and rejects unresolved searches") {
  const std::vector<double> constant{2,2,2},ordinary{1,2,3,4};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(constant),2,NAN,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::GammaBoundary);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::Gamma);
  CHECK(r.stop_detail.limit_coef==std::vector<double>{2,1});
  CHECK(r.stop_detail.loglik_lower<=r.logLik);
  CHECK(r.stop_detail.loglik_upper>=r.logLik);
  const auto unresolved=cppunivariate::betapr_mle_holds(mltest::as_span(ordinary),1e100,NAN,NAN);
  CHECK(unresolved.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(unresolved.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
  CHECK(std::isinf(unresolved.stop_detail.loglik_upper));
  CHECK(!unresolved.stop_detail.candidates.empty());
  const auto short_run=cppunivariate::betapr_mle_holds(mltest::as_span(ordinary),.7,NAN,NAN,1e-10,1);
  CHECK(short_run.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

TEST_CASE("betapr inverse gamma face uses reciprocal gamma scale") {
  const std::vector<double> x{3,3};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,2,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::InvGammaBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(6));
}

TEST_CASE("betapr signed tail selects nonconstant gamma faces") {
  const std::vector<double> x{1,2,3,4};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),.7,NAN,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::GammaBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==.7);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(2.5/.7));
  CHECK(r.stop_detail.loglik_upper>=r.logLik);
  const std::vector<double> y{1,.5,1./3,.25};
  const auto inv=cppunivariate::betapr_mle_holds(mltest::as_span(y),NAN,.7,NAN);
  REQUIRE(inv.status==cppunivariate::BetaPrimeStatus::InvGammaBoundary);
  CHECK(inv.stop_detail.limit_coef[1]==doctest::Approx(.7/2.5));
}

TEST_CASE("betapr conditional coefficients match independent scipy profiles") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const double shapes[]{.7,2,5},b[]{1.6969258659791764,.6398504179662102,.48632248971440617},
    scales[]{3.092136533992608,.12184378490069656,.021291062908283555},
    ll[]{-13.69991969846438,-14.337177096752114,-14.632869134819313};
  for(int j=0;j<3;++j) {
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),shapes[j],NAN,NAN);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
    CHECK(r.beta==doctest::Approx(b[j]).epsilon(1e-6));
    CHECK(r.scale==doctest::Approx(scales[j]).epsilon(1e-6));
    CHECK(r.logLik==doctest::Approx(ll[j]).epsilon(1e-10));
  }
}

TEST_CASE("betapr conditional reciprocal arithmetic stops before overflow") {
  const std::vector<double> x{std::numeric_limits<double>::denorm_min(),1};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,2,NAN);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
  std::vector<double> y{.03,.1,.4,.7,1,2,4,12};
  const auto base=cppunivariate::betapr_mle_holds(mltest::as_span(y),2,NAN,NAN);
  for(double& v:y)v*=1e-200;
  const auto moved=cppunivariate::betapr_mle_holds(mltest::as_span(y),2,NAN,NAN);
  REQUIRE(moved.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(moved.beta==doctest::Approx(base.beta).epsilon(1e-6));
  CHECK(moved.scale/1e-200==doctest::Approx(base.scale).epsilon(1e-6));
}

TEST_CASE("betapr conditional certificate compares separated local maxima") {
  const std::vector<double> x{1.9901008294052028e-5,2.209857117039297e-5,
    .0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,
    2.898690421966857,1.2684036923492177,400970.63293690106};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),.22690393135372058,NAN,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(r.beta==doctest::Approx(.22150182242584585).epsilon(1e-6));
  CHECK(r.scale==doctest::Approx(2.5493370924264878).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(-28.556114324015226).epsilon(1e-10));
}

TEST_CASE("betapr rounded conditional crossing keeps an uncertified fit and both witnesses") {
  std::vector<double> x{1.9901008294052028e-5,2.209857117039297e-5,
    .0002450722434289249,12.288342947780086,942.976056905759,16.239512580216314,
    2.898690421966857,1.2684036923492177,400970.63293690106};
  x.back()*=std::exp(5.646438395028704);
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),.22690393135372058,NAN,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK_FALSE(r.certified);
  REQUIRE(r.stop_detail.comparison_tie);
  CHECK(std::isfinite(r.stop_detail.loglik_upper));
  CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  bool left=false,right=false;
  for(const auto& c:r.stop_detail.candidates)if(c.coef.size()==3) {
    left=left||c.coef[1]<.12;right=right||c.coef[1]>.12;
  }
  CHECK(left);CHECK(right);
}

TEST_CASE("betapr stable digamma increments retain large-shape relative accuracy") {
  for(double b:{32.,1000.,100000.})for(double a:{1.,2.}) {
    const auto g=cppunivariate::betapr_cert_detail::increment(a,b);
    const double exact=1/b+(a==2?1/(b+1):0);
    CHECK(std::abs(g.value/exact-1)<1e-14);
    CHECK(std::abs(g.value-exact)<=g.error+4*std::numeric_limits<double>::epsilon()*exact);
  }
}

TEST_CASE("betapr non-unit faces stop when subnormal means lose coefficient accuracy") {
  const double v=3*std::numeric_limits<double>::denorm_min();
  const std::vector<double> x{v,v};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),2,NAN,NAN);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
}

TEST_CASE("betapr flat gamma-adjacent conditional maximum retains golden coefficient accuracy") {
  // Seed-8104 R hold sweep; high-precision stationarity oracle: flat.py.
  const std::vector<double> x{
    0.2802410396791599,1.3436077191767044,0.3472030471432838,0.2441657957218209,0.37701961388941624,
    0.6833708903622272,0.6278389000252419,0.38052450377474095,0.36732564867894735,0.29465588306090007,
    1.6337413655534816,0.5004872207354496,0.25644728823570834,0.30489045402771314,0.43156701444273454,
    0.3417576191295499,0.6299669667981216,0.4052784001144798,0.4067675092434208,0.4723512023354753,
    0.23656005267689753,0.2642974860041385,0.9126126455038472,0.32651442909992406,0.46787154802779246,
    0.1628156505909309,0.3788340921782178,0.835567043527286,0.3323693396507257,0.3235089636008228,
    0.5129659623359408,0.6237662717585744,0.2941132697632576,0.529388645275596,0.9425930099919333,
    1.2345113855705605,0.29093411659111756,0.800363767645574,0.5766059381215861,0.6692187265125815,
    0.20447619833715736,0.49035079695672834,0.16071566219449587,0.6181761242554693,0.2267230146767822,
    0.12765259697091694,0.41677556632417445,0.4059483378259152,0.8440600080403166,0.20514995369266417,
    0.4942728850503481,0.26190221637225936,0.26275727790533004,0.9032343024104332,0.7268230941680275,
    0.286034486110887,1.4472051241771995,1.0248139974432968,0.275316269565675,0.23313051172217203,
    1.4110065473371882,0.4366764752390559,0.7648443462094332,0.17455770282334382,0.2530796200266882,
    0.2894980917784592,1.3011788730484601,0.5665417375334098,0.2566793308738982,0.32048779836743785,
    0.23663945218855686,0.5998659518183862,0.2678331031882215,0.5032930931900615,0.6874786603810399,
    0.3214440324208265,0.14211011008082958,0.819017807409365,0.4105961256971879,0.5740438256003082,
    0.21131695936725742,0.3306741868864489,0.3793284722734561,0.7089210134356432,2.128146114762274,
    0.2130429847932904,1.6613597787317225,0.22276444863059733,0.2896374900002532,6.6784395851634635,
    0.3039549833938396,0.28494128883588377,0.40624286664406933,0.22108556498133647,0.507836372617378,
    0.18205988265034304,0.2673444641589703,0.9012350282744386,0.3236899952843548,0.7274713901255461};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),.7,NAN,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(r.beta==doctest::Approx(95.516952).epsilon(1e-6));
  CHECK(r.scale==doctest::Approx(78.232314).epsilon(1e-6));
  CHECK(r.logLik==doctest::Approx(-58.0668187021122).epsilon(1e-10));
  CHECK(r.iterations<=cppunivariate::certificate_budget(1000));
  const auto budget=cppunivariate::betapr_mle_holds(mltest::as_span(x),.66110553857791232,NAN,NAN,1e-6,64);
  REQUIRE(budget.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK_FALSE(budget.certified);
  CHECK(budget.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget);
  CHECK(budget.stop_detail.budget_exhausted);
  CHECK(budget.iterations<=64);
  CHECK(!budget.stop_detail.candidates.empty());
}

TEST_CASE("betapr reciprocal face enclosures retain large-Jacobian accuracy") {
  const std::vector<double> x(100000,1e200);
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,2,NAN);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::InvGammaBoundary);
  const double exact=x.size()*(2*std::log(2.)-2-std::log(1e200));
  CHECK(r.stop_detail.loglik_lower<=exact);
  CHECK(r.stop_detail.loglik_upper>=exact);
  CHECK(r.stop_detail.loglik_upper-r.stop_detail.loglik_lower<1e-4);
}

TEST_CASE("betapr joint exteriors bound small and simultaneously large shapes") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const auto fit=cppunivariate::betapr_mle(mltest::as_span(x));
  REQUIRE(fit.status==cppunivariate::BetaPrimeStatus::Ok);
  const auto& b=fit.joint_exterior;
  REQUIRE(b.status==cppunivariate::BetaPrimeJointBoundStatus::FiniteExterior);
  CHECK(b.witness_lower<fit.logLik);
  const double h=fit.alpha/(1+fit.alpha/fit.beta);
  CHECK(h>b.harmonic_lower);
  CHECK(h<b.harmonic_upper);
  CHECK(b.gamma_tail_upper<b.witness_lower);
  CHECK(b.invgamma_tail_upper<b.witness_lower);
  // These are uniform over scale, including arbitrarily unequal shapes.
  for(double p:{.01,.3,1.,10.,100.})for(double q:{.02,.5,2.,20.,200.})
    for(double scale:{.001,.3,1.,100.,1e6}) {
      const double harmonic=p/(1+p/q);
      double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbetapr(v,p,q,scale,true);
      const double small=x.size()*(std::log(harmonic)-b.logmean);
      CHECK(ll<=small+1e-9);
      const double large=cppunivariate::betapr_joint_detail::large_upper(
        harmonic,x.size(),b.logmean,b.pair_loss);
      CHECK(ll<=large+1e-9);
    }
}

TEST_CASE("betapr joint exterior encloses the frozen golden without moving its fit") {
  const auto g=mltest::load_distribution_golden("betapr");
  const auto r=cppunivariate::betapr_mle(mltest::as_span(g.data));
  const auto& b=r.joint_exterior;
  REQUIRE(b.status==cppunivariate::BetaPrimeJointBoundStatus::FiniteExterior);
  CHECK(r.alpha==doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.beta==doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.scale==doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_betapr).scale(0));
  CHECK(r.alpha<b.invgamma_shape1);
  CHECK(r.beta<b.gamma_shape2);
  CHECK(b.evaluations<=cppunivariate::certificate_budget(1000));
}

TEST_CASE("betapr joint bounds commute with reciprocals and extreme rescaling") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const double p=.7,q=1.7,s=3.1;
  const auto b=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(x),p,q,s);
  REQUIRE(b.status==cppunivariate::BetaPrimeJointBoundStatus::FiniteExterior);
  std::vector<double> reciprocal,scaled;double jac=0;
  for(double v:x){reciprocal.push_back(1/v);scaled.push_back(v*1e200);jac+=2*std::log(v);}
  const auto inv=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(reciprocal),q,p,1/s);
  const auto big=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(scaled),p,q,s*1e200);
  REQUIRE(inv.status==cppunivariate::BetaPrimeJointBoundStatus::FiniteExterior);
  REQUIRE(big.status==cppunivariate::BetaPrimeJointBoundStatus::FiniteExterior);
  CHECK(inv.harmonic_upper==doctest::Approx(b.harmonic_upper).epsilon(1e-10));
  CHECK(inv.gamma_upper==doctest::Approx(b.invgamma_upper+jac).epsilon(1e-9));
  CHECK(inv.invgamma_upper==doctest::Approx(b.gamma_upper+jac).epsilon(1e-9));
  CHECK(big.harmonic_upper==doctest::Approx(b.harmonic_upper).epsilon(1e-10));
  CHECK(big.gamma_upper==doctest::Approx(b.gamma_upper-x.size()*200*std::log(10.)).epsilon(1e-9));
}

TEST_CASE("betapr joint bounds distinguish geometric exclusion from face selection") {
  const std::vector<double> x{1,2,3,4},constant{2,2},invalid{0,1};
  const auto b=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(x),1,1,2);
  CHECK(b.status==cppunivariate::BetaPrimeJointBoundStatus::Geometry);
  CHECK(std::isinf(b.gamma_shape2));
  CHECK(std::isinf(b.invgamma_shape1));
  CHECK(cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(constant),1,1,2).status==
    cppunivariate::BetaPrimeJointBoundStatus::Arithmetic);
  CHECK(cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(invalid),1,1,2).status==
    cppunivariate::BetaPrimeJointBoundStatus::Arithmetic);
  CHECK(cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(x),1,1,2,0).status==
    cppunivariate::BetaPrimeJointBoundStatus::Budget);
}

TEST_CASE("betapr joint harmonic exclusion avoids overflowing an unequal shape ratio") {
  cppunivariate::BetaPrimeJointBounds b;
  b.harmonic_lower=.01;b.harmonic_upper=1;
  CHECK_FALSE(b.excludes(1e308,.1));
  CHECK_FALSE(b.excludes(.1,1e308));
  CHECK(b.excludes(1e308,.001));
  CHECK(b.excludes(.001,1e308));
}

TEST_CASE("betapr joint signed gamma tail is uniform in the other shape") {
  const std::vector<double> x{1,2,3,4};
  const auto b=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(x),1,1,2);
  CHECK(std::isfinite(b.gamma_signed_shape2));
  CHECK(b.gamma_signed_shape2>2*b.harmonic_upper);
  // An exterior signed bound alone does not select a face or close the box.
  CHECK(b.status==cppunivariate::BetaPrimeJointBoundStatus::Geometry);
  std::vector<double> inverse;for(double v:x)inverse.push_back(1/v);
  const auto inv=cppunivariate::betapr_joint_exterior_bounds(mltest::as_span(inverse),1,1,.5);
  CHECK(inv.invgamma_signed_shape1==doctest::Approx(b.gamma_signed_shape2).epsilon(1e-9));
  CHECK(b.evaluations<=cppunivariate::certificate_budget(1000));
}

TEST_CASE("betapr joint gamma tangent cannot hide unavailable arithmetic in a maximum") {
  int evaluations=0;
  const auto failed=cppunivariate::betapr_joint_detail::gamma_tangent(
    1,1e308,0,1,2,1,evaluations);
  CHECK(failed.upper==INFINITY);
  CHECK(failed.lower==-INFINITY);
  CHECK(std::isnan(failed.shape));
  CHECK(evaluations<=1);
}

TEST_CASE("betapr joint witness retains both practical likelihood bounds") {
  const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  const auto r=cppunivariate::betapr_mle(mltest::as_span(x));
  CHECK(r.joint_exterior.witness_lower<=r.logLik);
  CHECK(r.joint_exterior.witness_upper>=r.logLik);
  CHECK(std::isfinite(r.joint_exterior.witness_upper));
}

TEST_CASE("betapr all-free cover encloses the global golden maximizer") {
  const auto g=mltest::load_distribution_golden("betapr");
  const auto r=cppunivariate::betapr_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  REQUIRE(r.joint_exterior.global_certified);
  REQUIRE(r.joint_exterior.coefficient_lower.size()==3);
  REQUIRE(r.joint_exterior.coefficient_upper.size()==3);
  const std::vector<double> estimates{r.alpha,r.beta,r.scale};
  for(std::size_t k=0;k<3;++k) {
    CHECK(r.joint_exterior.coefficient_lower[k]<=estimates[k]);
    CHECK(r.joint_exterior.coefficient_upper[k]>=estimates[k]);
    CHECK(r.joint_exterior.coefficient_upper[k]/r.joint_exterior.coefficient_lower[k]-1<1e-6);
  }
  CHECK(r.iterations<=cppunivariate::certificate_budget(1000));
  CHECK(r.stop_detail.loglik_lower<=r.logLik);
  CHECK(r.stop_detail.loglik_upper>=r.logLik);
  CHECK(r.joint_exterior.retained_maxima==1);
}

TEST_CASE("betapr all-free face selection has a closed cover and proper witnesses") {
  const std::vector<double> x{1,2,3,4};
  const auto g=cppunivariate::betapr_mle(mltest::as_span(x));
  REQUIRE(g.status==cppunivariate::BetaPrimeStatus::GammaBoundary);
  CHECK(g.joint_exterior.global_certified);
  CHECK(g.stop_detail.limit_model==cppunivariate::StopModel::Gamma);
  REQUIRE(g.stop_detail.limit_coef.size()==2);
  CHECK(g.stop_detail.limit_coef[0]==doctest::Approx(4.26542805477655).epsilon(1e-9));
  CHECK(std::isfinite(g.stop_detail.loglik_upper));
  std::vector<double> y;for(double v:x)y.push_back(1/v);
  const auto ig=cppunivariate::betapr_mle(mltest::as_span(y));
  REQUIRE(ig.status==cppunivariate::BetaPrimeStatus::InvGammaBoundary);
  CHECK(ig.joint_exterior.global_certified);
  CHECK(ig.stop_detail.limit_model==cppunivariate::StopModel::InvGamma);
}

TEST_CASE("betapr all-free unresolved symmetric faces stop as a comparison tie") {
  const std::vector<double> x{.5,1,2};
  const auto r=cppunivariate::betapr_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK_FALSE(r.joint_exterior.global_certified);
  CHECK(r.stop_detail.comparison_tie);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::None);
  bool gamma=false,invgamma=false;
  for(const auto& c:r.stop_detail.candidates){gamma=gamma||c.model==cppunivariate::StopModel::Gamma;invgamma=invgamma||c.model==cppunivariate::StopModel::InvGamma;}
  CHECK(gamma);CHECK(invgamma);
}

TEST_CASE("betapr all-free certificate allowance retains an uncertified converged search") {
  const auto g=mltest::load_distribution_golden("betapr");
  const auto r=cppunivariate::betapr_mle(mltest::as_span(g.data),1e-6,16);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Budget);
  CHECK(r.stop_detail.budget_exhausted);
  CHECK_FALSE(r.joint_exterior.global_certified);
  CHECK(r.iterations<=cppunivariate::certificate_budget(16));
  CHECK(!r.stop_detail.candidates.empty());
  CHECK(std::isinf(r.stop_detail.loglik_upper));
}

TEST_CASE("betapr strict coefficient request retains an uncertified converged search") {
  const auto g=mltest::load_distribution_golden("betapr");
  const auto r=cppunivariate::betapr_mle(mltest::as_span(g.data),1e-14,1000);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
  CHECK_FALSE(r.joint_exterior.global_certified);
  CHECK(!r.stop_detail.candidates.empty());
}

TEST_CASE("betapr completed-square tail closes a nearly flat gamma-face sample") {
  const std::vector<double> x{.5785984598506846,6.444883202937109,5.775033695452986,.8258634064615794,1.0870520306262386,2.0005965476322385,5.747358256019271,4.559600112478463,1.2019432688689116,1.6187100158941814,2.1690795112119656,1.4243459092803628,.9863928238183352,2.8868931810321508,1.1828957819270425,.4462530309097321,5.002827229326903,1.8897560739317296,5.678764367303953,2.6395434132700193};
  const auto r=cppunivariate::betapr_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::GammaBoundary);
  CHECK(r.joint_exterior.global_certified);
  CHECK(r.iterations<=cppunivariate::certificate_budget(2000));
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(1.86080018422581).epsilon(1e-9));
}

TEST_CASE("betapr centered shape roots reject an exhausted search") {
  const std::vector<double> x{.999999999998,.999999999999,1.,1.000000000001,1.000000000003};
  const auto r=cppunivariate::betapr_mle_fixed_scale(mltest::as_span(x),1.0594104501122898,1e-10,1);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(std::isnan(r.logLik));
}

// Raw score roots checked independently at 350 and 700 digits.
TEST_CASE("betapr: TASK-101.7.3 raw likelihood regressions") {
  struct Fixture { int index; std::vector<double> x; double a,b,s,ll; };
  const std::vector<Fixture> fixtures{
    {248,{0.6510020100386483,0.6510020165486686,0.6510020230586888,0.651002029568709,0.6510020425887495},5454373785714345.0,8876194498901787.0,1.0594104501122898,84.44195054705918},
    {249,{0.6510020230573869,0.6510020230580378,0.6510020230586888,0.6510020230593399,0.6510020230606418},5.454453294772166e+23,8.876323905999515e+23,1.0594104501122898,130.4936888613748},
    {251,{0.99999998,0.99999999,1.0,1.00000001,1.00000003},6567301472501320.0,6957467795090793.0,1.0594104501122898,82.29573789639241},
    {252,{0.999999999998,0.999999999999,1.0,1.000000000001,1.000000000003},6.56723725758789e+23,6.957399779053996e+23,1.0594104501122898,128.34741532364148},
    {254,{1e-12,2e-12,3e-12,5e-12,9e-12},2.0262551677149374,536658974818.8154,1.0594104501122898,126.82230092559585},
    {255,{1e-17,2e-17,3e-17,5.0000000000000005e-17,9.000000000000001e-17},2.0262551677082414,5.366589748160354e+16,1.0594104501122898,184.38692825044689},
    {256,{1e-200,2e-200,3e-200,5e-200,9e-200},2.0262551677082414,5.366589748160354e+199,1.0594104501122898,2291.252288339999},
    {257,{1.8666916871781815e-151,2.7261096086292664e-151,1.484646969214721e-151,3.988768449995832e-151,4.39429737837367e-151,1.901375652366606e-150,8.50681333541539e-151,1.0314927998288996e-151,1.7856179604413502e-150,1.5002084830804323e-150,1.3375151204526194e-150,6.510020230586889e-151},1.3768062808236863,1.8278993726688574e+150,1.0594104501122898,4135.705570780781},
    {260,{0.6510007210546427,0.6510013720566658,0.6510020230586888,0.6510026740607118,0.651003325062735},807247310521.9342,1313676772576.6814,1.0594104501122898,62.39620481822838},
  };
  for(const auto& f:fixtures) {
    CAPTURE(f.index);
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(f.x),NAN,NAN,f.s);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
    CHECK(r.alpha==doctest::Approx(f.a).epsilon(1e-10));
    CHECK(r.beta==doctest::Approx(f.b).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(f.s).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(f.ll).epsilon(1e-10));
  }
}

TEST_CASE("betapr: TASK-101.7.3 held-shape scale increment regression") {
  const std::vector<double> x{1e-06,2e-06,3e-06,4.9999999999999996e-06,9e-06};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),2.014271442794272,NAN,1.0594104501122898);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(r.alpha==doctest::Approx(2.014271442794272).epsilon(1e-10));
  CHECK(r.beta==doctest::Approx(533486.0575303183).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(1.0594104501122898).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(57.74469737827288).epsilon(1e-10));
}

TEST_CASE("betapr centered fit keeps finite scales when scale plus data overflows") {
  const std::vector<double> x{.99999998,.99999999,1,1.00000001,1.00000003};
  std::vector<double> large;
  for(double v:x)large.push_back(std::ldexp(v,1023));
  const double scale=1.0594104501122898;
  const auto a=cppunivariate::betapr_mle_fixed_scale(mltest::as_span(x),scale,1e-10,1000);
  const auto b=cppunivariate::betapr_mle_fixed_scale(mltest::as_span(large),std::ldexp(scale,1023),1e-10,1000);
  REQUIRE(a.status==cppunivariate::BetaPrimeStatus::Ok);
  REQUIRE(b.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(b.alpha==doctest::Approx(a.alpha).epsilon(1e-10));
  CHECK(b.beta==doctest::Approx(a.beta).epsilon(1e-10));
  CHECK(b.logLik==doctest::Approx(a.logLik-x.size()*1023*std::log(2.0)).epsilon(1e-10));
}

TEST_CASE("betapr centered profile rejects a separated shape restoration loss") {
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> x{1-2*e,1-e,1,1+e,1+3*e};
  // Both shapes occupy the same binary exponent bin. Their ratio cannot
  // encode the much smaller centered mean displacement at this precision.
  const auto r=cppunivariate::betapr_mle_fixed_scale(mltest::as_span(x),1,1e-10,1000);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
  CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
}

TEST_CASE("betapr held-shape score rejects an underflowed data statistic") {
  const std::vector<double> x{1e-100,2e-100,3e-100,5e-100,9e-100};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),1e-320,NAN,1e300);
  CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

TEST_CASE("beta-prime fallback keeps local convergence and rejects a separated witness") {
 const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};const auto data=mltest::as_span(x);
 auto r=cppunivariate::betapr_mle(data,1e-14,16);
 REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
 double ll=0;for(double v:x)ll+=cppunivariate::distributions::dbetapr(v,r.alpha,r.beta,r.scale,true);
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
 cppunivariate::BetaPrimeResult certificate{NAN,NAN,NAN,NAN,0,cppunivariate::BetaPrimeStatus::BfgsFailure};
 const auto empty=cppunivariate::betapr_detail::fallback(certificate,data,NAN,NAN,NAN,1e-8,1000);
 REQUIRE(empty.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(empty.certified);
 certificate.stop_detail=r.stop_detail;certificate.stop_detail.loglik_lower=r.logLik+1;
 CHECK(cppunivariate::betapr_detail::fallback(certificate,data,NAN,NAN,NAN,1e-8,1000).status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

TEST_CASE("beta-prime unit-shape margin preferences do not prove limit stops") {
 std::vector<double> x{1,1,1,1,6.0001};
 const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),1,NAN,NAN);
 REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
 for(double& v:x)v=1/v;
 const auto q=cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,1,NAN);
 REQUIRE(q.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(q.certified);
}

TEST_CASE("beta-prime unit shapes retain large finite scalar proposals") {
 std::vector<double> x{1,1,1,1,6.00000001};
 for(bool inverse:{false,true}) {
   std::vector<double> y=x;if(inverse)for(double& v:y)v=1/v;
   const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(y),inverse?NAN:1,inverse?1:NAN,NAN,1e-6,1000);
   REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
   CHECK((inverse?r.alpha:r.beta)>1e6);CHECK(std::isfinite(r.logLik));
 }
}

TEST_CASE("beta-prime retains an unclosed face as a refuting witness") {
 const std::vector<double> x{1,1,1,1,5.99998};const auto data=mltest::as_span(x);
 auto certificate=cppunivariate::betapr_mle_holds_certificate(data,1,NAN,NAN,1e-6,16);
 REQUIRE(certificate.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
 REQUIRE(std::isfinite(certificate.stop_detail.loglik_lower));
 CHECK(certificate.stop_detail.limit_coef.empty());CHECK(certificate.stop_detail.limit_model==cppunivariate::StopModel::None);
 REQUIRE(!certificate.stop_detail.candidates.empty());
 CHECK(certificate.stop_detail.candidates.back().model==cppunivariate::StopModel::Exp);
 auto without_bound=certificate;without_bound.stop_detail.loglik_lower=-INFINITY;
 const auto unchecked=cppunivariate::betapr_detail::fallback(without_bound,data,1,NAN,NAN,1e-6,16);
 REQUIRE(unchecked.status==cppunivariate::BetaPrimeStatus::Ok);
 const double margin=2048*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(unchecked.logLik));
 CHECK(certificate.stop_detail.loglik_lower>unchecked.logLik+margin);
 CHECK(cppunivariate::betapr_detail::fallback(certificate,data,1,NAN,NAN,1e-6,16).status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

TEST_CASE("unit face bounds survive unrepresentable face coefficients") {
 cppunivariate::StopDetail d;d.limit_coef={INFINITY};d.limit_loglik=42;
 cppunivariate::fallback_retain_unclosed_face(d,cppunivariate::StopModel::Exp);
 CHECK(d.loglik_lower==42);CHECK(d.candidates.empty());CHECK(d.limit_coef.empty());CHECK(std::isnan(d.limit_loglik));
 const double u=1e-320;const std::vector<double> x{u,u,u,u,6*u};
 const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),1,NAN,NAN,1e-6,16);
 REQUIRE(std::isfinite(r.stop_detail.loglik_lower));
 if(r.status==cppunivariate::BetaPrimeStatus::Ok) {
   CHECK_FALSE(r.certified);CHECK(r.scale>0);CHECK(std::isfinite(r.logLik));
   const double margin=2048*std::numeric_limits<double>::epsilon()*(x.size()+std::abs(r.logLik));
   CHECK(r.logLik+margin>=r.stop_detail.loglik_lower);
 } else CHECK(r.status==cppunivariate::BetaPrimeStatus::BfgsFailure);
}

// Independent 1600-digit exact-input reference; beta-smoke.py.
TEST_CASE("betapr: centered large-shape density") {
  CHECK(cppunivariate::distributions::dbetapr(1.0,100000000.0,100000000.0,1,true)==
        doctest::Approx(7.944828247241538).epsilon(1e-10).scale(0));
}

TEST_CASE("betapr: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dbetapr(0.6510020230593399,5.454590759462655e+23,8.876547609650913e+23,1.0594104501122898,true)==doctest::Approx(26.490608930512803).epsilon(1e-10));
}

TEST_CASE("beta-prime unit-shape fallback meets coefficient accuracy") {
  const std::vector<double> x{1,1,1,1,6.0001};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),1,NAN,NAN,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.beta==doctest::Approx(66667.7187444793946102).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(133334.770828333628047).scale(0.).epsilon(1e-10));
}

TEST_CASE("beta-prime final fallback preserves both held unit-submodel shapes") {
  const std::vector<double> x{.2,.5,1,2,4};
  cppunivariate::BetaPrimeResult certificate{NAN,NAN,NAN,NAN,0,cppunivariate::BetaPrimeStatus::BfgsFailure};
  const auto r=cppunivariate::betapr_detail::accurate_fallback(certificate,mltest::as_span(x),1,2,NAN,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);
  CHECK(r.alpha==1);CHECK(r.beta==2);CHECK_FALSE(r.certified);
}

TEST_CASE("Beta prime inverse gamma fallback meets coefficient accuracy") {
  const std::vector<double> x{3.5683567301379977,0.7442648518071554,2.880159054558412,4.095577748896918,2.652381900463434,1.4633342070949795,1.592765277780328,2.6279516564115144,2.7223800014956954,3.3937893573070728,0.6120919878044578,1.9980530142818287,3.8994368272706015,3.2798665448184368,2.3171372383296265,2.9260503468715076,1.587384819687631,2.4674396654683,2.458406773589118,2.1170688145931207,4.227256414107402,3.7836152553654694,1.0957551431340367,3.0626517877222734,2.1373387721806885,6.14191569649817,2.639677950445818,1.1967920560612015,3.0087011065787896,3.0911044592690122,1.9494470850389507,1.6031645910906915,3.4000506022898462,1.8889713803351542,1.060903263019697,0.8100370816246663,3.437204311811297,1.2494318713873003,1.734286683306988,1.4942797629277036,4.890544758422772,2.0393563265448234,6.22216893080286,1.6176619587247874,4.410668239506283,7.833761503714881,2.3993729018706067,2.463367642679781,1.1847498880106104,4.874483186567535,2.023173898965016,3.8182189286196504,3.8057937270924778,1.107132443189249,1.3758506134765434,3.496081936121262,0.6909870503454336,0.9757868281412991,3.6321863636230045,4.289442821588824,0.7087139332465692,2.290024896469534,1.3074555691703778,5.728764665355511,3.9513256732981747,3.454254202011315,0.7685338432041671,1.765095020807763,3.8959116676647465,3.1202435945892217,4.2258380449731145,1.667039105934716,3.733668422970268,1.9869138152912504,1.454590604231618,3.1109614711740083,7.036797026131488,1.2209746735093583,2.435483282512738,1.7420272728380046,4.7322278486037375,3.0241247719266116,2.6362376491451576,1.4105943836446617,0.46989254782052675,4.69388842336335,0.6019165823090994,4.48904664163116,3.452591720771803,0.14973557628964068,3.2899608647122696,3.509494900108931,2.461581684525066,4.523135646980922,1.969138198680061,5.492698256433353,3.7404926380124066,1.1095884742902893,3.0893756821909837,1.37462450561447};
  const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),NAN,0.6611055385779123,NAN,1e-10,64);
  REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
  // Independent high-precision stationary point of the original density.
  CHECK(r.alpha==doctest::Approx(903.4245984850465401617863215484544388420379575287735626554719464).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(0.001264250680378879150812707124035623646577326098999374020035308876).scale(0.).epsilon(1e-10));
  CHECK(r.beta==0.6611055385779123);
}

TEST_CASE("Beta prime compensated Gamma departure bounds coefficient error") {
  { const std::vector<double> x{1.0,1.0,1.0,1.0,8.985168161033918};
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),0.6611055385779123,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok); CHECK_FALSE(r.certified);
    CHECK(r.beta==doctest::Approx(71618.47142739572031154388700920621).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(281336.2936878014405012764396800171).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{1.0,1.0,1.0,1.0,8.985105265485746};
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),0.6611055385779123,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok); CHECK_FALSE(r.certified);
    CHECK(r.beta==doctest::Approx(238725.6884041646489954132088074790).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(937782.2533692742702742166843418656).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{1.0,1.0,1.0,1.0,8.985087295329125};
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),0.6611055385779123,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok); CHECK_FALSE(r.certified);
    CHECK(r.beta==doctest::Approx(716174.8798100404613229502093774770).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(2813342.138375284610124319377009914).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("Beta prime compensated finish handles non-small observation ratios") {
  { std::vector<double> x(99,1.); x.push_back(7.374830600608956);
    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;
    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));
    CHECK(q==doctest::Approx(374.88836755126440990156921771869741).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(132.56954076665546204386005735725110).scale(0.).epsilon(1e-10));
  }
  { std::vector<double> x(99,1.); x.push_back(7.518031194795538);
    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;
    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));
    CHECK(q==doctest::Approx(228.25013545547284050334007997895613).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(80.679419633580429980858623485156981).scale(0.).epsilon(1e-10));
  }
  { std::vector<double> x(99,1.); x.push_back(7.73283208607541);
    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;
    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));
    CHECK(q==doctest::Approx(145.74713059697825275709911646626930).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(51.484748772168213396011330564120988).scale(0.).epsilon(1e-10));
  }
  { std::vector<double> x(99,1.); x.push_back(8.019233274448574);
    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;
    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));
    CHECK(q==doctest::Approx(99.889444145470257136063353453462679).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(35.257748983085543278573452640287820).scale(0.).epsilon(1e-10));
  }
  { std::vector<double> x(99,1.); x.push_back(8.5920356511949);
    double p=3,q=64,s=64*(99+x.back())/300,ll=NAN;
    REQUIRE(cppunivariate::burr_power_one_detail::beta_gamma_fit(mltest::as_span(x),p,q,s,{{false,true,true}},1e-10,1000,ll));
    CHECK(q==doctest::Approx(63.159620835798900632837502463984144).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(22.261237871687099335545528442514219).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("Beta prime comparable large shapes retain coefficient accuracy") {
  { const std::vector<double> x{0.9799722532057399,0.9852508068243196,0.9881961522676651,0.9903994220407385,0.9922328226931237,0.9938488332290568,0.9953271554482268,0.9967163631459319,0.99804979742889,0.9993529413517348,1.0006474776042487,1.0019540132928577,1.0032944546467601,1.0046947825407908,1.006189237804866,1.0078279786046531,1.0096936425300809,1.011944842838386,1.0149699884268253,1.0204370549561421};
    cppunivariate::BetaPrimeResult proposal{20000.458802487380216880751993739499*(1+1e-5),20000.458802500447702104752173266946*(1-1e-5),1.0000000000006533977848971577682253*(1+1e-5),0,0,cppunivariate::BetaPrimeStatus::Ok};proposal.certified=false;
    const auto r=cppunivariate::betapr_detail::accurate_fallback(proposal,mltest::as_span(x),NAN,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.alpha==doctest::Approx(20000.458802487380216880751993739499).scale(0.).epsilon(1e-10));
    CHECK(r.beta==doctest::Approx(20000.458802500447702104752173266946).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(1.0000000000006533977848971577682253).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{0.9939490748014492,0.9955522076553079,0.9964441173014446,0.9971100956520769,0.997663484230479,0.9981506631933517,0.9985958478192408,0.999013774694044,0.9994145394413054,0.9998058384274802,1.0001941992785572,1.0005858035235526,1.0009871989064998,1.0014061265964862,1.0018527631898093,1.0023419878610904,1.0028982801001862,1.0035685721224241,1.0044676635845822,1.0060877617897672};
    cppunivariate::BetaPrimeResult proposal{222222.68102745302314514004796877808*(1+1e-5),222222.68103211733738557870874340043*(1-1e-5),1.0000000000209894311796031276671251*(1+1e-5),0,0,cppunivariate::BetaPrimeStatus::Ok};proposal.certified=false;
    const auto r=cppunivariate::betapr_detail::accurate_fallback(proposal,mltest::as_span(x),NAN,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.alpha==doctest::Approx(222222.68102745302314514004796877808).scale(0.).epsilon(1e-10));
    CHECK(r.beta==doctest::Approx(222222.68103211733738557870874340043).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(1.0000000000209894311796031276671251).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{0.9799722532057399,0.9852508068243196,0.9881961522676651,0.9903994220407385,0.9922328226931237,0.9938488332290568,0.9953271554482268,0.9967163631459319,0.99804979742889,0.9993529413517348,1.0006474776042487,1.0019540132928577,1.0032944546467601,1.0046947825407908,1.006189237804866,1.0078279786046531,1.0096936425300809,1.011944842838386,1.0149699884268253,1.0204370549561421};
    cppunivariate::BetaPrimeResult proposal{20000.45880248738,20000.458802500446883557820750074264*(1-1e-5),1.0000000000006533159283185190268680*(1+1e-5),0,0,cppunivariate::BetaPrimeStatus::Ok};proposal.certified=false;
    const auto r=cppunivariate::betapr_detail::accurate_fallback(proposal,mltest::as_span(x),20000.45880248738,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.alpha==doctest::Approx(20000.458802487381035462021827697753).scale(0.).epsilon(1e-10));
    CHECK(r.beta==doctest::Approx(20000.458802500446883557820750074264).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(1.0000000000006533159283185190268680).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("Beta prime deeper Gamma face uses coefficient-specific error bounds") {
  { const std::vector<double> x{1.0,1.0,1.0,1.0,8.985079208758647};
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),0.6611055385779123,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.beta==doctest::Approx(7161738.9593434688).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(28133400.568535466).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{1.0,1.0,1.0,1.0,8.985078400101598};
    const auto r=cppunivariate::betapr_mle_holds(mltest::as_span(x),0.6611055385779123,NAN,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BetaPrimeStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.beta==doctest::Approx(71617380.19217217).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(281333986.58875811).scale(0.).epsilon(1e-10));
  }
}
