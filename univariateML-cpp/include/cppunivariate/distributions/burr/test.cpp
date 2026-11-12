#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include <vector>
#include <cmath>
#include <string>
namespace {
#include "../burr/oracle-fixtures.inc"
}
TEST_CASE("burr three parameter golden and reciprocal fit") {
 auto g = mltest::load_distribution_golden("burr");
 auto r = cppunivariate::burr_mle(mltest::as_span(g.data),1e-8,1000);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 // The fit above is only asked to converge to gradtol=1e-8 (adapter.cpp's
 // own default, used here directly); a golden comparison tighter than
 // that -- 1e-12 chases whichever platform's BFGS/finite-difference-
 // gradient trajectory landed closest to, not a real difference.
 CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_burr).scale(0));
 CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_burr).scale(0));
 CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_burr).scale(0));
 CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_burr).scale(0));
 // Independent full actuar multistart oracle on this ordinary sample.
 CHECK(r.logLik == doctest::Approx(-613.67979675083461).epsilon(1e-8));
 // Independent full likelihood scores, including the freely fitted scale.
 double g0=-static_cast<double>(g.data.size()), g1=g0, g2=0;
 for (double x : g.data) {
   const double z = r.shape2 * (std::log(x)-std::log(r.scale));
   const double sp = std::max(z,0.0)+std::log1p(std::exp(-std::abs(z)));
   const double sigmoid = z>=0 ? 1/(1+std::exp(-z)) : std::exp(z)/(1+std::exp(z));
   g0 += r.shape1*sp;
   g1 += -z+(r.shape1+1)*z*sigmoid;
   g2 += r.shape2*(1-(r.shape1+1)*sigmoid);
 }
 CHECK(std::sqrt(g0*g0+g1*g1+g2*g2)<1e-3);
 const double factor = 1000.; // All Burr values exceed 1; all inverse values are below 1.
 std::vector<double> reciprocal, scaled;
 double jacobian = 0;
 for (double x : g.data) { reciprocal.push_back(1/x); scaled.push_back(factor*x); jacobian += 2*std::log(x); }
 auto inv = cppunivariate::invburr_mle(mltest::as_span(reciprocal),1e-8,1000);
 CHECK(inv.shape1 == doctest::Approx(r.shape1).epsilon(1e-5));
 CHECK(inv.shape2 == doctest::Approx(r.shape2).epsilon(1e-5));
 CHECK(inv.scale == doctest::Approx(1/r.scale).epsilon(1e-5));
 CHECK(inv.logLik == doctest::Approx(r.logLik+jacobian).epsilon(1e-8));
 auto s = cppunivariate::burr_mle(mltest::as_span(scaled),1e-8,1000);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-4));
 CHECK(s.scale == doctest::Approx(factor*r.scale).epsilon(1e-4));
 CHECK(s.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(factor)).epsilon(1e-8));
}
TEST_CASE("burr limits and invalid inputs") {
 std::vector<double> w,p;
 for (int i=1;i<=200;++i) {
   double u=(i-0.5)/200.;
   w.push_back(std::pow(-std::log(1-u),0.5));
   p.push_back(std::pow(1-u,-0.5));
 }

 auto a=cppunivariate::burr_mle(mltest::as_span(w),1e-8,1000);
 auto b=cppunivariate::burr_mle(mltest::as_span(p),1e-8,1000);
 CHECK(a.status == cppunivariate::BurrStatus::WeibullBoundary);
 CHECK(b.status == cppunivariate::BurrStatus::ParetoBoundary);
 const std::vector<double> bad = {0,1};
 CHECK(cppunivariate::burr_mle(mltest::as_span(bad),1e-8,1000).status == cppunivariate::BurrStatus::InvalidInit);
}

TEST_CASE("burr n20 actuar multistart oracle fixtures") {
 for (const auto &fixture : burr_oracle_fixtures) {
   std::vector<double> x = fixture.x;
   double jacobian=0;

   auto fit=cppunivariate::burr_mle(mltest::as_span(x),1e-8,1000);
   CHECK(fit.logLik == doctest::Approx(fixture.logLik+jacobian).epsilon(1e-8));
   if (fixture.face=="interior") CHECK(fit.status==cppunivariate::BurrStatus::Ok);
   if (fixture.face=="weibull") CHECK(fit.status==cppunivariate::BurrStatus::WeibullBoundary);
   if (fixture.face=="pareto") CHECK(fit.status==cppunivariate::BurrStatus::ParetoBoundary);
 }
}

TEST_CASE("burr fixed scale historical regression and equivariance") {
 const auto golden = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/burr/fixed-scale-golden.json");
 const auto &x = golden.data;
 auto r = cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1.0);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 CHECK(std::abs(r.shape1 - 1.077036666067376) < 1e-10);
 CHECK(std::abs(r.shape2 - 1.9857562895421235) < 1e-10);
 CHECK(std::abs(r.logLik - -614.507368787803) < 1e-10);
 auto y=x; for (double &v : y) v*=3.0;
 auto s = cppunivariate::burr_mle_fixed_scale(mltest::as_span(y),3.0);
 CHECK(s.scale == 3.0);
 CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-10));
 CHECK(s.shape2 == doctest::Approx(r.shape2).epsilon(1e-10));
 CHECK(s.logLik == doctest::Approx(r.logLik-x.size()*std::log(3.0)).epsilon(1e-10));
 const std::vector<double> escape = { 3,4,5 };
 CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(escape),3).status == cppunivariate::BurrStatus::AllGeOne);
 for (double bad : {0.0,-1.0,std::numeric_limits<double>::infinity()})
   CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),bad).status == cppunivariate::BurrStatus::InvalidInit);
}

TEST_CASE("burr fixed scale profile fallback start") {
 std::vector<double> x = {.5,2,3,4,5};

 auto r = cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1.0,1e-10);
 REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
 CHECK(std::isfinite(r.logLik));
 double theta[2] = {r.shape1,r.shape2}, score[2];
 cppunivariate::BurrCost cost{mltest::as_span(x)};
 cost.gradient(theta,score);
 CHECK(std::hypot(score[0],score[1]) < 1e-8);
}

TEST_CASE("burr: held-scale endpoint spike and nonconstant escape differ") {
  const std::vector<double> spike={1,1,1,1}, mixed={1,1.5,2,3};
  const auto a=cppunivariate::burr_mle_fixed_scale(mltest::as_span(spike),1);
  CHECK(a.status==cppunivariate::BurrStatus::Unbounded);
  CHECK(a.logLik==INFINITY);
  CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(mixed),1).status==cppunivariate::BurrStatus::AllGeOne);
}

#include "cppunivariate/distributions/burr/dpqr.hpp"

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("burr: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("burr",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dburr(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pburr(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qburr(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rburr(rng, p[0], p[1], p[2]); });
}

TEST_CASE("P2 burr golden holds") {
 auto g=mltest::load_distribution_golden("burr");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<8;++mask){
 double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan,s=mask&4?g.estimates.at("scale"):nan;
 auto r=cppunivariate::burr_mle_holds(mltest::as_span(g.data),a,b,s);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 if(mask&4)CHECK(r.scale==s);
 CHECK(r.logLik>=g.logLik-1e-5);
 }
}


// mpmath reproduction: import mpmath as mp; mp.mp.dps=1200
// F=lambda x: -mp.expm1(-2*mp.log1p(x**4)); Q=lambda l: mp.expm1(-mp.log1p(-mp.exp(l))/2)**mp.mpf(".25")
// Evaluate log tails at the stated decimal x; log densities follow mp.log(mp.diff(F,x))
// (use -mp.diff(S,x) for survival definitions); Q uses the indicated log tail.
TEST_CASE("burr: deep tails and log inversion regressions") {
  CHECK(cppunivariate::distributions::pburr(1e-100,2,4,1,true,true) == doctest::Approx(std::log(2.0)-400*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::dburr(1e100,2,4,1,true) == doctest::Approx(std::log(8.0)-900*std::log(10.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qburr(-1000,2,4,1,true,true) == doctest::Approx(std::exp((-1000-std::log(2.0))/4)).scale(0.0));
  CHECK(cppunivariate::distributions::qburr(-INFINITY,2,4,1,true,true) == 0);
  CHECK(std::isnan(cppunivariate::distributions::qburr(.2,2,4,INFINITY,true,false)));
  // mp.exp((-mp.log(mp.exp(-1))/mp.mpf('1e-310'))/mp.mpf('1e308'))
  CHECK(cppunivariate::distributions::qburr(-1,1e-310,1e308,1,false,true) == doctest::Approx(std::exp(100.0)).scale(0.0).epsilon(1e-10));

  // mp.log((1+mp.mpf('1e100')**mp.mpf('1e308'))**(-mp.mpf('1e-308')))
  CHECK(cppunivariate::distributions::pburr(1e100,1e-308,1e308,1,false,true) == doctest::Approx(-100*std::log(10.0)).scale(0.0).epsilon(1e-10));

}

TEST_CASE("burr: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("burr");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::burr_mle(x,1e-8,1).status==cppunivariate::BurrStatus::IterationFailure);
}

TEST_CASE("multistart: rank only converged or total-score-accepted solves") {
  cppunivariate::BfgsResult best{};best.f=INFINITY;
  cppunivariate::BfgsResult r{{0,0},1,{2e-6,0},2e-6,16,30,cppunivariate::BfgsStatus::LineSearchFailure};
  CHECK(cppunivariate::consider_bfgs_candidate(best,r,200));
  CHECK(best.status==cppunivariate::BfgsStatus::Ok);
  r.f=.5;r.grad={2e-5,0};
  CHECK_FALSE(cppunivariate::consider_bfgs_candidate(best,r,200));
  CHECK(best.f==1);
  r.status=cppunivariate::BfgsStatus::Ok;
  CHECK(cppunivariate::consider_bfgs_candidate(best,r,200));
  CHECK(best.f==.5);
  r.f=.25;r.grad[0]=NAN;
  CHECK_FALSE(cppunivariate::consider_bfgs_candidate(best,r));
  r.grad={0,0};r.status=cppunivariate::BfgsStatus::BadInit;
  CHECK_FALSE(cppunivariate::consider_bfgs_candidate(best,r));
  best.f=INFINITY;r.status=cppunivariate::BfgsStatus::IterationFailure;r.grad={1,0};
  CHECK_FALSE(cppunivariate::consider_bfgs_candidate(best,r));
  CHECK(std::isinf(best.f));
}

TEST_CASE("burr: constant data off a held scale collapse only with both shapes free") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (double scale : {0.3,1.0,3.0}) {
    std::vector<double> x(3,.5*scale);
    const auto data=mltest::as_span(x);
    const auto r=cppunivariate::burr_mle_fixed_scale(data,scale);
    CHECK(r.status==cppunivariate::BurrStatus::Unbounded);
    CHECK(r.logLik==INFINITY);
    CHECK(r.iterations==0);
    CHECK(r.scale==scale);
    CHECK(cppunivariate::burr_mle_holds(data,nan,nan,scale).status==cppunivariate::BurrStatus::Unbounded);
    // Holding k removes the off-scale concentrating path; holding c
    // also leaves an ordinary finite fit (including the all-held case).
    for (int mask : {1,2,3}) {
      const auto h=cppunivariate::burr_mle_holds(data,mask&1?2.0:nan,mask&2?3.0:nan,scale);
      CHECK(h.status==cppunivariate::BurrStatus::Ok);
      CHECK(std::isfinite(h.logLik));
    }
    x.back()=std::nextafter(x.back(),INFINITY);
    CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),scale).status!=cppunivariate::BurrStatus::Unbounded);
  }
  std::vector<double> invalid{.5,.5,INFINITY};
  CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(invalid),1).status==cppunivariate::BurrStatus::InvalidInit);
}

namespace {
#include "certificate-fixtures.inc"
void check_burr_refutation_witness(const cppunivariate::BurrMLE& stopped,
    const cppunivariate::BurrMLE& proposal) {
  REQUIRE(stopped.status==cppunivariate::BurrStatus::NumericalFailure);
  CHECK_FALSE(stopped.certified);CHECK_FALSE(stopped.stop_detail.budget_exhausted);
  CHECK_FALSE(stopped.stop_detail.comparison_tie);
  CHECK(std::isnan(stopped.shape1));CHECK(std::isnan(stopped.logLik));
  REQUIRE(proposal.status==cppunivariate::BurrStatus::Ok);
  CHECK((std::isfinite(proposal.shape1)&&proposal.shape1>0));
  CHECK((std::isfinite(proposal.shape2)&&proposal.shape2>0));
  CHECK((std::isfinite(proposal.scale)&&proposal.scale>0));
  double nomination_lower=NAN;
  for(const auto& c:stopped.stop_detail.candidates) {
    if(c.model==cppunivariate::StopModel::None&&c.coef==std::vector<double>{proposal.shape1,proposal.shape2,proposal.scale}) {
      CHECK(std::isfinite(c.loglik_lower));CHECK(std::isfinite(c.loglik_upper));
      CHECK(c.loglik_lower<=c.loglik_upper);nomination_lower=c.loglik_lower;
    }
  }
  REQUIRE(std::isfinite(nomination_lower));
  for(const auto& c:stopped.stop_detail.candidates)if(c.model==cppunivariate::StopModel::Weibull) {
    REQUIRE(c.coef.size()==2);CHECK(c.coef[0]>0);CHECK(c.coef[1]>0);
    CHECK((std::isfinite(c.coef[0])&&std::isfinite(c.coef[1])));
    CHECK(std::isfinite(c.loglik_lower));CHECK(std::isfinite(c.loglik_upper));
    CHECK(c.loglik_lower<=c.loglik_upper);
    // The independently measured closure improvement is smaller than these
    // floating enclosures: preserve both witnesses without claiming separation.
    CHECK(c.loglik_upper>=nomination_lower);
    CHECK(stopped.stop_detail.loglik_lower>=c.loglik_lower);
  }
}
}
TEST_CASE("burr: certified retained workloads and guarded stops") {
  for (const auto& fixture : burr_certificate_fixtures) {
    CAPTURE(fixture.name);
    const auto fit=cppunivariate::burr_mle(mltest::as_span(fixture.x),1e-8,1000);
    if(fixture.name=="nearly-constant") {
      const auto proposal=cppunivariate::burr_mle_legacy(mltest::as_span(fixture.x),1e-8,1000,NAN,NAN,NAN,1e-12,true);
      check_burr_refutation_witness(fit,proposal);
    } else CHECK(fit.status==fixture.status);
    if(fit.status==cppunivariate::BurrStatus::Ok){
      CHECK(fit.shape1>0);CHECK(fit.shape2>0);CHECK(fit.scale>0);
      CHECK(std::isfinite(fit.logLik));
      double ll=0;
      for(double x:fixture.x)ll+=cppunivariate::distributions::dburr(x,fit.shape1,fit.shape2,fit.scale,true);
      CHECK(ll==doctest::Approx(fit.logLik).epsilon(1e-11));
    }
    if(fit.status==cppunivariate::BurrStatus::CertificateBudget||fit.status==cppunivariate::BurrStatus::CertificateTie){
      CHECK(std::isnan(fit.shape1));CHECK(std::isnan(fit.logLik));
    }
  }
}
TEST_CASE("burr: certificate arithmetic and failure gates") {
  namespace cert=cppunivariate::burr_cert_detail;
  CHECK_FALSE(cert::log(cert::Interval(-1,1)).valid());
  CHECK_FALSE((cert::Interval(1)/cert::Interval(-1,1)).valid());
  CHECK_FALSE(cert::exp(cert::Interval(10000)).valid());
  CHECK_FALSE(cert::entropy(cert::Interval(-.1,.5)).valid());
  CHECK_FALSE(cert::padded(NAN,1).valid());
  cert::Matrix singular,inverse;
  CHECK_FALSE(cert::inverse(singular,inverse,3));
  const std::vector<double> x={.2,.5,1,2,4};
  auto a=cppunivariate::burr_mle_certified(mltest::as_span(x),1e-8,1000,NAN,NAN,NAN,1e-12,0);
  const auto proposal=cppunivariate::burr_mle_legacy(mltest::as_span(x),1e-8,1000,NAN,NAN,NAN,1e-12,true);
  check_burr_refutation_witness(a,proposal);
  CHECK(proposal.scale/(1-1e-8)<x.front());
  for(const std::vector<double>& bad:std::vector<std::vector<double>>{{},{1},{0,1},{1,INFINITY},{1,NAN}})
    CHECK(cppunivariate::burr_mle(mltest::as_span(bad),1e-8,1000).status==cppunivariate::BurrStatus::InvalidInit);
  for(double tol:std::array<double,4>{{0.,-1.,INFINITY,NAN}})
    CHECK(cppunivariate::burr_mle(mltest::as_span(x),tol,1000).status==cppunivariate::BurrStatus::InvalidInit);
  const std::vector<double> constant={2,2,2};
  CHECK(cppunivariate::burr_mle(mltest::as_span(constant),1e-8,1000).status==cppunivariate::BurrStatus::Unbounded);
  const auto failed=cppunivariate::burr_mle(mltest::as_span(x),1e-8,2);
  CHECK(failed.status==cppunivariate::BurrStatus::NumericalFailure);
  CHECK(std::isnan(failed.shape1));
  // A sign-uncertain slope never supplies a signed Weibull tail.
  cert::Data d;REQUIRE(cert::data(mltest::as_span(x),d));
  cert::Weibull w;REQUIRE(cert::weibull(d,w));
  for(cert::Interval slope:{cert::Interval(-1e-20,1e-20),cert::Interval(0),cert::Interval(.1),cert::Interval(-1,1)}){
    w.A=slope;CHECK(cert::signed_tail(d,w)==0);
  }
}

TEST_CASE("burr: certificate coordinates survive extreme changes of units") {
  auto g=mltest::load_distribution_golden("burr");
  const auto base=cppunivariate::burr_mle(mltest::as_span(g.data),1e-8,1000);
  REQUIRE(base.status==cppunivariate::BurrStatus::Ok);
  for(double scale:{1e-100,1e100}){
    auto x=g.data;for(double& v:x)v*=scale;
    std::reverse(x.begin(),x.end());
    const auto fit=cppunivariate::burr_mle(mltest::as_span(x),1e-8,1000);
    REQUIRE(fit.status==cppunivariate::BurrStatus::Ok);
    CHECK(fit.shape1==doctest::Approx(base.shape1).epsilon(1e-8));
    CHECK(fit.shape2==doctest::Approx(base.shape2).epsilon(1e-8));
    CHECK(fit.scale/scale==doctest::Approx(base.scale).epsilon(1e-8));
    CHECK(fit.logLik==doctest::Approx(base.logLik-g.data.size()*std::log(scale)).epsilon(1e-10));
  }
}

TEST_CASE("burr: inverse legacy held dispatch retains invalid control guards") {
  const std::vector<double> x={.2,.5,1,2,4};
  for(double tol:std::array<double,4>{{0.,-1.,INFINITY,NAN}})
    CHECK(cppunivariate::invburr_mle_holds(mltest::as_span(x),NAN,NAN,NAN,tol,1000).status==cppunivariate::InvBurrStatus::InvalidInit);
  CHECK(cppunivariate::invburr_mle_holds(mltest::as_span(x),NAN,NAN,NAN,1e-8,0).status==cppunivariate::InvBurrStatus::InvalidInit);
}

TEST_CASE("burr: structured limits and budget witnesses preserve units") {
  const std::vector<double> x{1,1,1,2,4,8};
  const auto face=cppunivariate::burr_mle(mltest::as_span(x),1e-8,1000);
  REQUIRE(face.status==cppunivariate::BurrStatus::ParetoBoundary);
  REQUIRE(face.stop_detail.limit_coef.size()==2);
  CHECK(face.stop_detail.limit_coef[0]==doctest::Approx(x.size()/(std::log(2.)+std::log(4.)+std::log(8.))));
  CHECK(face.stop_detail.limit_coef[1]==1);
  CHECK(face.stop_detail.limit_loglik==face.logLik);
  const auto budget=cppunivariate::burr_mle_certified(mltest::as_span(x),1e-8,1000,NAN,NAN,NAN,1e-12,2);
  REQUIRE(budget.status==cppunivariate::BurrStatus::CertificateBudget);
  REQUIRE(budget.stop_detail.candidates.size()==3);
  const auto& c=budget.stop_detail.candidates.back();
  double ll=0;for(double v:x)ll+=cppunivariate::distributions::dburr(v,c.coef[0],c.coef[1],c.coef[2],true);
  CHECK(c.loglik_lower<=ll+1e-10);
  CHECK(c.loglik_upper>=ll-1e-10);
  CHECK(budget.stop_detail.loglik_lower>=c.loglik_lower);
  CHECK(budget.stop_detail.loglik_upper==INFINITY);
}

TEST_CASE("burr: held-power Weibull stop transports the exponential fit") {
  const std::vector<double> x{3,3,3};
  const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2,NAN);
  REQUIRE(r.status==cppunivariate::BurrStatus::WeibullBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(3));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(3*(std::log(2.)-std::log(3.)-1)));
}

TEST_CASE("held Burr power transports certified Lomax face bounds") {
 const std::vector<double> x{1,2,3,4};
 const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2,NAN,1e-10,1000);
 REQUIRE(r.status==cppunivariate::BurrStatus::WeibullBoundary);
 REQUIRE(r.stop_detail.limit_coef.size()==2);
 double ll=0;
 for(double v:x){const double scale=r.stop_detail.limit_coef[1];ll+=std::log(2.)-2*std::log(scale)+std::log(v)-(v/scale)*(v/scale);}
 CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
}

TEST_CASE("Burr held-scale strict-concavity certificate covers both sides of scale") {
 for(const std::vector<double>& x:std::vector<std::vector<double>>{
   {.1,.2,.4,.8},{.5,2,3,4,5},{.01,.02,.1,1,2,100},{.9,.95,1,1.01},
   {1e-100,1e-50,1.,1e50,1e100}}) {
   const auto r=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1,1e-10);
   REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
   CHECK(r.stop_detail.loglik_lower<=r.logLik);
   CHECK(r.stop_detail.loglik_upper>=r.logLik);
   CHECK(r.stop_detail.candidates.size()==1);
   double ll=0;for(double v:x)ll+=cppunivariate::distributions::dburr(v,r.shape1,r.shape2,1,true);
   CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
   // Concavity certifies globality. These scores independently check the center.
   double theta[2]={r.shape1,r.shape2},g[2];
   cppunivariate::BurrCost{mltest::as_span(x)}.gradient(theta,g);
   CHECK(std::hypot(g[0]*r.shape1,g[1]*r.shape2)<1e-8);
 }
 const std::vector<double> x{.5,2,3,4,5};
 const auto budget=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1,1e-10,1);
 CHECK(budget.status==cppunivariate::BurrStatus::Ok);
 CHECK(std::isfinite(budget.shape1));CHECK(std::isfinite(budget.shape2));
 CHECK(std::isfinite(budget.stop_detail.loglik_lower));
 CHECK(budget.stop_detail.candidates.size()==1);
 for(double tol:std::vector<double>{0.,-1.,INFINITY,NAN})
   CHECK(cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1,tol).status==cppunivariate::BurrStatus::InvalidInit);
}

TEST_CASE("Burr held-scale Pareto supremum retains tied-endpoint penalty") {
 const std::vector<double> x{1,1,2,4};
 const auto r=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1);
 REQUIRE(r.status==cppunivariate::BurrStatus::AllGeOne);
 const double s=std::log(8.),expected=4*std::log(4/s)-4-s-2*std::log(2.);
 CHECK(r.stop_detail.limit_loglik==doctest::Approx(expected).epsilon(1e-12));
 CHECK(r.stop_detail.loglik_lower<=expected);CHECK(r.stop_detail.loglik_upper>=expected);
 CHECK(r.stop_detail.limit_coef[1]==1);
}

TEST_CASE("Burr held-scale log arithmetic avoids overflowing ratios") {
 const std::vector<double> x{1e-300,1e-200,1e-100,1.,1e100};
 const auto r=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1e-200);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
 CHECK(std::isfinite(r.logLik));
 const std::vector<double> close{.5,std::nextafter(.5,1.)};
 const auto stop=cppunivariate::burr_mle_fixed_scale(mltest::as_span(close),1);
 CHECK(stop.status!=cppunivariate::BurrStatus::Ok);
 CHECK(std::isnan(stop.shape1));CHECK(std::isnan(stop.shape2));
}

TEST_CASE("Burr held-scale sign uncertainty retains its converged search") {
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};
 const auto r=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
 CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.shape1));CHECK(std::isfinite(r.shape2));CHECK(std::isfinite(r.logLik));
}

TEST_CASE("Burr uncertified fallback rejects a refuted or failed search") {
 const std::vector<double> x{.2,.5,1,2,4};
 const auto data=mltest::as_span(x);
 auto candidate=cppunivariate::burr_mle_legacy(data,1e-8,1000,NAN,NAN,NAN,1e-12,true);
 REQUIRE(candidate.status==cppunivariate::BurrStatus::Ok);
 cppunivariate::BurrMLE certificate{NAN,NAN,NAN,NAN,0,cppunivariate::BurrStatus::CertificateBudget};
 auto fallback=cppunivariate::burr_uncertified(certificate,candidate,data);
 check_burr_refutation_witness(fallback,candidate);
 CHECK(candidate.scale/(1-1e-10)<x.front());
 certificate.stop_detail.loglik_lower=candidate.logLik+1;
 CHECK(cppunivariate::burr_uncertified(certificate,candidate,data).status==certificate.status);
 candidate.status=cppunivariate::BurrStatus::IterationFailure;
 CHECK(cppunivariate::burr_uncertified(certificate,candidate,data).status==certificate.status);
}

TEST_CASE("burr retains a converged joint search after short certificate work") {
 const auto g=mltest::load_distribution_golden("burr");
 const auto r=cppunivariate::burr_mle(mltest::as_span(g.data),1e-8,16);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
}

TEST_CASE("Burr held-shape logarithm coalescence is not a constant-data proof") {
 const double a=1e300;
 const std::vector<double> x{a,std::nextafter(a,INFINITY)};
 REQUIRE(x[0]!=x[1]);REQUIRE(std::log(x[0])==std::log(x[1]));
 CHECK(cppunivariate::burr_mle_holds(mltest::as_span(x),2,NAN,NAN).status==cppunivariate::BurrStatus::NumericalFailure);
}

TEST_CASE("Burr held-power tolerance-face preference retains the finite search") {
 const std::vector<double> x{1,1,1,1,6.0001};
 const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,1,NAN,1e-14,1000);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
 CHECK(cppunivariate::fallback_lomax_face_proved(mltest::as_span(std::vector<double>{1,2,3,4}),1000));
}

TEST_CASE("held-power fallback score gate rejects an unchecked finite Lomax point") {
 const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
 const cppunivariate::LomaxResult bad{1,100,0,1,cppunivariate::LomaxStatus::IterationFailure};
 CHECK_FALSE(cppunivariate::fallback_lomax_search_accepted(mltest::as_span(x),bad));
 const std::vector<double> near{1,1,1,1,6.0001};
 const auto r=cppunivariate::lomax_newton_candidate(mltest::as_span(near),1e-14,1000);
 CHECK(cppunivariate::fallback_lomax_search_accepted(mltest::as_span(near),r));
}

TEST_CASE("Burr held-power fallback bounds coefficients near the exponential face") {
  // Independent 90-digit log-density stationary point, exact binary64 input.
  const std::vector<double> x{1,1,1,1,6.0001};
  const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,1,NAN,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
  CHECK_FALSE(r.certified);
  CHECK(r.shape1==doctest::Approx(66667.7187444793946102).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(133334.770828333628047).scale(0.).epsilon(1e-10));
}

TEST_CASE("Burr unit-power information survives double-only shape cancellation") {
  const std::vector<double> x{1,1,1,1,6.00000001};
  double shape=7e8,scale=1.4e9;
  REQUIRE(cppunivariate::burr_power_one_detail::fit(mltest::as_span(x),shape,scale,1e-14,1000,false));
  CHECK(shape==doctest::Approx(666666671.770397338004).epsilon(1e-14));
  CHECK(scale==doctest::Approx(1333333342.87412800995).epsilon(1e-14));
}

TEST_CASE("Burr uncertified held-scale root meets an explicit accuracy target") {
  const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};
  const auto r=cppunivariate::burr_mle_fixed_scale(mltest::as_span(x),1,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
  // Independent 101-digit likelihood score with scale held exactly.
  CHECK(r.shape1==doctest::Approx(48258140354.1866752246650177026654).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==doctest::Approx(23.993572806535416190638122614245).scale(0.).epsilon(1e-10));
  CHECK(r.scale==1);
}

TEST_CASE("Burr large-shape coefficient box has a separated local refutation") {
  for(const auto& fixture:burr_certificate_fixtures)if(fixture.name=="nearly-constant") {
    const auto x=mltest::as_span(fixture.x);
    const auto proposed=cppunivariate::burr_mle_legacy(x,1e-8,1000,NAN,NAN,NAN,1e-12,true);
    REQUIRE(proposed.status==cppunivariate::BurrStatus::Ok);
    CHECK(cppunivariate::burr_shape_box_refuted(x,proposed.shape1,proposed.shape2,1e-8));
  }
  const std::vector<double> x{.2,.5,1,2,4};
  const auto proposed=cppunivariate::burr_mle_legacy(mltest::as_span(x),1e-8,1000,NAN,NAN,NAN,1e-12,true);
  REQUIRE(proposed.status==cppunivariate::BurrStatus::Ok);
  // The fixed-scale all-above theorem excludes every stationary point in
  // this entire requested scale box, independently of the other coefficients.
  CHECK(proposed.scale/(1-1e-8)<x.front());
}

TEST_CASE("Burr nonunit held-power fallback meets native coefficient accuracy") {
  const std::vector<double> x{1,1,1,1,2.4495101551126504};
  const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2,NAN,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
  // Independent high-precision stationary point of the original density.
  CHECK(r.shape1==doctest::Approx(66667.71874405320055825545018026647679833294733177931493178952973).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(365.15034003473314572567979882839772936854697211830511199867490983).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==2);
}

TEST_CASE("Burr local accuracy retains its target under a legacy search tolerance") {
  const std::vector<double> x{1,1,1,1,6.0001};
  double shape=60000,power=1,scale=120000;
  REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),shape,power,scale,{{true,false,true}},1.220703e-4,1000));
  // Independent 90-digit root; the loose control belongs to the search.
  CHECK(shape==doctest::Approx(66667.71874447939461023856773531186459536977558075677731627344142672063202245712091942).scale(0.).epsilon(1e-10));
  CHECK(scale==doctest::Approx(133334.7708283336280471370552934774438962200637402300276785163578466626638319176309549).scale(0.).epsilon(1e-10));
}

TEST_CASE("burr: Weak exponential-limit held powers retain native coefficient accuracy") {
  {
    const std::vector<double> x{1.0,1.0,1.0,1.0,2.4494897448244197};
    const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,2.0,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.shape1==doctest::Approx(666666610.92630697381779917077968275295298369167313287087387395132677813352875519665091381025665).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(36514.835631369714294825408120922276675386090363445046754314358011288694714220507288532125474644).scale(0.).epsilon(1e-10));
    CHECK(r.shape2==2.0);
  }
  {
    const std::vector<double> x{1.0,1.0,1.0,1.0,36.00000012};
    const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,0.5,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.shape1==doctest::Approx(666666652.58865502197420083662392759859342796035022394941588076282966536099986062466890461021225).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(1777777700917272060.6654163176664120044564896171320992198583887770446808939526040220326876030463).scale(0.).epsilon(1e-10));
    CHECK(r.shape2==0.5);
  }
  {
    const std::vector<double> x{1.0,1.0,1.0,1.0,1.7688590836615325};
    const auto r=cppunivariate::burr_mle_holds(mltest::as_span(x),NAN,3.141592653589793,NAN,1e-10,1000);
    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.shape1==doctest::Approx(666666667.89315058989582895932224763845393917523352091454951786377841719505145358266361848939366).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(802.70914516744934322181353673911291509246224796875392553960405621104121272397593883894593180991).scale(0.).epsilon(1e-10));
    CHECK(r.shape2==3.141592653589793);
  }
}

TEST_CASE("Coupled Weibull-limit burr fallback retains coefficient accuracy") {
  { const std::vector<double> x{0.022944204006919947,0.07796154146971186,0.13353139262452263,0.19237189264745608,0.25489224962879004,0.3215836241274623,0.39304258810960724,0.4700036292457356,0.5533852381847866,0.6443570163905133,0.7444404749474959,0.8556661100577202,0.9808292530117262,1.1239300966523995,1.2909841813155658,1.491654876777717,1.742969305058623,2.0794415416798357,2.5902671654458267,4.249486463117075};
    double k=1011514.4448902489594276495020804652*(1+1e-5),c=1.0000003493627439985941339660820916*(1+1e-8),s=1022318.8474620781567707270823607141*(1+1e-5);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,s,{{true,true,true}},1e-10,1000,false,0));
    CHECK(k==doctest::Approx(1011514.4448902489594276495020804652).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.0000003493627439985941339660820916).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(1022318.8474620781567707270823607141).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{0.022944204006919947,0.07796154146971186,0.13353139262452263,0.19237189264745608,0.25489224962879004,0.3215836241274623,0.39304258810960724,0.4700036292457356,0.5533852381847866,0.6443570163905133,0.7444404749474959,0.8556661100577202,0.9808292530117262,1.1239300966523995,1.2909841813155658,1.491654876777717,1.742969305058623,2.0794415416798357,2.5902671654458267,4.249482256129683};
    double k=101151327.20202118000696486694120576*(1+1e-5),c=1.0000000034936317628298799937559361*(1+1e-8),s=102232317.10398401209558647161117913*(1+1e-5);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,s,{{true,true,true}},1e-10,1000,false,0));
    CHECK(k==doctest::Approx(101151327.20202118000696486694120576).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.0000000034936317628298799937559361).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(102232317.10398401209558647161117913).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("Burr weak-coordinate holds preserve native coefficient accuracy") {
  { const std::vector<double> x{0.5,0.5,1.0,1.0,2.0};
    double k=1.00000000000000005250476025520442025e+300;
    double c=1.9735125038192624698070750534821045*(1+1e-6);
    double scale=1.17165737357432157840259370205389208e+152*(1+1e-6);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,scale,{{false,true,true}},1e-10,1000,false));
    CHECK(k==doctest::Approx(1.00000000000000005250476025520442025e+300).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.9735125038192624698070750534821045).scale(0.).epsilon(1e-10));
    CHECK(scale==doctest::Approx(1.17165737357432157840259370205389208e+152).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{0.5,0.5,1.0,1.0,2.0};
    double k=1.74409805514458174346163609070365999e+197*(1+1e-6);
    double c=1.9735125038192624698070750534821045*(1+1e-6);
    double scale=1.00000000000000001590289110975991805e+100;
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,scale,{{true,true,false}},1e-10,1000,false));
    CHECK(k==doctest::Approx(1.74409805514458174346163609070365999e+197).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.9735125038192624698070750534821045).scale(0.).epsilon(1e-10));
    CHECK(scale==doctest::Approx(1.00000000000000001590289110975991805e+100).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{2.0,2.0,1.0,1.0,0.5};
    double k=1.00000000000000005250476025520442025e+300;
    double c=1.9735125038192624698070750534821045*(1+1e-6);
    double scale=8.53491833495099102690809415626554753e-153*(1+1e-6);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,scale,{{false,true,true}},1e-10,1000,true));
    CHECK(k==doctest::Approx(1.00000000000000005250476025520442025e+300).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.9735125038192624698070750534821045).scale(0.).epsilon(1e-10));
    CHECK(scale==doctest::Approx(8.53491833495099102690809415626554753e-153).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("burr: exact-input extreme arithmetic regression") {
  CHECK(cppunivariate::distributions::qburr(-0.6931471805599453,1.0,1e-08,1.0,true,true) == doctest::Approx(1.0000000046380937).epsilon(1e-10).scale(0));
}
