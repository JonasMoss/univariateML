#include "test_support.hpp"
#include "cppunivariate/distributions/pareto2/dpqr.hpp"
#include "cppunivariate/distributions/pareto2/mle.hpp"
TEST_CASE("pareto2: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::pareto2_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::lomax_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("pareto2: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dpareto2(1,2,1,0,false) == doctest::Approx(.25));
}

#include "cppunivariate/distributions/pareto2/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("pareto2: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qpareto2(0,2,1,0,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qpareto2(0,2,1,0,false,false))==true);
  CHECK(cppunivariate::distributions::ppareto2(-1,2,1,0,true,false)==0);
  CHECK(cppunivariate::distributions::ppareto2(std::numeric_limits<double>::infinity(),2,1,0,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qpareto2(.1,2,1,0,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rpareto2(rng,2,1,0));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::ppareto2(point,2,1,0,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float((1+mp.mpf(100)**1)**(-2))
TEST_CASE("pareto2: independent extreme value") {
 CHECK(cppunivariate::distributions::ppareto2(100.0,2,1,0,false,false)==doctest::Approx(9.8029604940692082e-05).epsilon(1e-8));
}

TEST_CASE("P2 pareto2 golden holds") {
 auto g=mltest::load_distribution_golden("pareto2");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<4;++mask){double a=mask&1?g.estimates.at("shape"):nan,s=mask&2?g.estimates.at("scale"):nan;auto r=cppunivariate::pareto2_mle(mltest::as_span(g.data),g.estimates.at("min"),1e-9,1000,a,s);
 if(mask&1)CHECK(r.shape==a);
 if(mask&2)CHECK(r.scale==s);
 CHECK(r.logLik>=g.logLik-1e-5);}
}


TEST_CASE("pareto2: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pareto2",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto2(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto2(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto2(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto2(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: mp.mp.dps=300; s=mp.mpf(3); m=mp.mpf(4)
// pareto2: m+s*mp.expm1(500/2); pareto3: m+s*mp.exp(500/4)
// pareto4: m+s*mp.expm1(500/2)**mp.mpf('.25') (upper log input -500).
TEST_CASE("pareto2: shifted deep-tail delegation") {
  CHECK(cppunivariate::distributions::qpareto2(-500, 2, 3, 4, false, true) == doctest::Approx(4+3*std::expm1(250.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto2(-INFINITY, 2, 3, 4, true, true) == 4);
  CHECK(cppunivariate::distributions::ppareto2(INFINITY, 2, 3, 4, false, true) == -INFINITY);
}

TEST_CASE("Pareto II held coefficients use exact Lomax coordinates") {
 const std::vector<double> x{2,3,4,7,12},y{1,2,3,6,11};
 for(double shape:{double(NAN),2.})for(double scale:{double(NAN),3.}) {
  const auto r=cppunivariate::pareto2_mle(mltest::as_span(x),1,1e-10,1000,shape,scale);
  const auto p=cppunivariate::lomax_mle(mltest::as_span(y),1e-10,1000,std::isnan(scale)?scale:1/scale,shape);
  CHECK(static_cast<int>(r.status)==static_cast<int>(p.status));
  if(r.status==cppunivariate::Pareto2Status::Ok){CHECK(r.logLik==doctest::Approx(p.logLik));CHECK(r.shape==doctest::Approx(p.kappa));CHECK(r.scale==doctest::Approx(1/p.lambda));}
 }
}

TEST_CASE("Pareto II keeps the always-certified Lomax success and scale map") {
 const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};const auto d=mltest::as_span(x);
 const auto p=cppunivariate::lomax_mle(d,1e-8,1000);const auto r=cppunivariate::pareto2_mle(d,0,1e-8,1000);
 REQUIRE(p.status==cppunivariate::LomaxStatus::Ok);REQUIRE(static_cast<int>(r.status)==static_cast<int>(p.status));
 CHECK(r.shape==p.kappa);CHECK(r.scale==1/p.lambda);CHECK(r.min==0);CHECK(r.logLik==p.logLik);
}

TEST_CASE("pareto2: exact-input extreme arithmetic regression") {
  CHECK(cppunivariate::distributions::qpareto2(-0.6931471805599453,1.0,1.0,-1.0,true,true) == doctest::Approx(4.638093627692599e-17).epsilon(1e-10).scale(0));
}

TEST_CASE("pareto2 reciprocal rate overflow remains numerical at every tolerance") {
  const std::vector<double> x{1e308,1.1e308,1.2e308};
  for(double tol:{1e-10,1e-20}) {
    const auto r=cppunivariate::pareto2_mle({x.data(),x.size()},0,tol,200,100,NAN);
    CHECK(r.status!=cppunivariate::Pareto2Status::Ok);
    CHECK(r.certificate_failure==cppunivariate::LomaxCertificateFailure::Arithmetic);
    CHECK(r.stop_detail.accuracy_stop==cppunivariate::AccuracyStop::None);
    for(const auto& c:r.stop_detail.candidates)for(double v:c.coef)CHECK(std::isfinite(v));
  }
}

TEST_CASE("pareto2: the closed lower endpoint retains its likelihood conclusions") {
  const std::vector<double> zeros{0,0,0};
  const auto no=cppunivariate::pareto2_mle(mltest::as_span(zeros),0,1e-10,1000);
  CHECK(cppunivariate::outcome(no).reason==cppunivariate::OutcomeReason::Unbounded);
  const auto held=cppunivariate::pareto2_mle(mltest::as_span(zeros),0,1e-10,1000,2,3);
  REQUIRE(held.status==cppunivariate::Pareto2Status::Ok);
  CHECK(held.logLik==doctest::Approx(3*std::log(2./3)));
  const std::vector<double> x{0,1};
  const auto finite=cppunivariate::pareto2_mle(mltest::as_span(x),0,1e-10,1000,2,NAN);
  REQUIRE(finite.status==cppunivariate::Pareto2Status::Ok);
  CHECK(finite.scale==doctest::Approx(.5).epsilon(1e-10));
  const auto critical=cppunivariate::pareto2_mle(mltest::as_span(x),0,1e-10,1000,1,NAN);
  CHECK(cppunivariate::outcome(critical).reason==cppunivariate::OutcomeReason::Unattained);
  const auto shape=cppunivariate::pareto2_mle(mltest::as_span(x),0,1e-10,1000,NAN,1);
  REQUIRE(shape.status==cppunivariate::Pareto2Status::Ok);
  CHECK(shape.shape==doctest::Approx(2/std::log(2.)).epsilon(1e-10));
}

// TASK-214.27: lomax decides its exponential stop by a margin of 1e-8 per
// observation, so the stop is never a proof. For (1, r) the sup of the profile
// h exceeds zero (the MLE exists, gain 5e-9 per observation) at this r, below
// the margin; pareto2 used to certify the limit.
TEST_CASE("pareto2: a margin-decided exponential stop is not certified") {
  const std::vector<double> x{1,28.809206572777494};
  const auto r=cppunivariate::pareto2_mle(mltest::as_span(x),0,1e-10,1000);
  REQUIRE(r.status==cppunivariate::Pareto2Status::ExponentialLimit);
  const auto o=cppunivariate::outcome(r);
  CHECK(o.kind==cppunivariate::OutcomeKind::Limit);
  CHECK_FALSE(o.certified);
}

#include "cppunivariate/distributions/exp/dpqr.hpp"
TEST_CASE("pareto2: exponential limit payload evaluates original observations") {
  const std::vector<double> x{3,4,5,6,7,8,9,10,11,12};
  const auto fit=cppunivariate::pareto2_mle(mltest::as_span(x),2,1e-10,1000);
  REQUIRE(fit.status==cppunivariate::Pareto2Status::ExponentialLimit);
  REQUIRE(fit.stop_detail.limit_model==cppunivariate::StopModel::ShiftedExp);
  const auto& p=fit.stop_detail.limit_coef;
  REQUIRE(p.size()==2);
  CHECK(p[1]==2);
  double ll=0;
  for(double v:x)ll+=cppunivariate::distributions::dexp(v-p[1],p[0],true);
  CHECK(ll==doctest::Approx(fit.stop_detail.limit_loglik).epsilon(1e-12));
}

TEST_CASE("pareto2: budget face candidates carry a negative held threshold") {
  const std::vector<double> x{-2,-2,37,56};
  const auto fit=cppunivariate::pareto2_mle(mltest::as_span(x),-3,1e-8,1);
  REQUIRE(fit.stop_detail.budget_exhausted);
  const auto& candidates=fit.stop_detail.candidates;
  const auto face=std::find_if(candidates.begin(),candidates.end(),[](const cppunivariate::FitCandidate& c) {
    return c.model==cppunivariate::StopModel::ShiftedExp;
  });
  REQUIRE(face!=candidates.end());
  REQUIRE(face->coef.size()==2);
  CHECK(face->coef[1]==-3);
  double ll=0;
  for(double v:x)ll+=cppunivariate::distributions::dexp(v-face->coef[1],face->coef[0],true);
  CHECK(ll==doctest::Approx(face->loglik_lower).epsilon(1e-12));
  CHECK(ll==doctest::Approx(face->loglik_upper).epsilon(1e-12));
}

TEST_CASE("pareto2: free threshold uses the closed endpoint for held shape") {
  const std::vector<double> x{0, 1};
  const auto fit=cppunivariate::pareto2_mle(mltest::as_span(x),NAN,1e-10,100,2);
  REQUIRE(fit.status==cppunivariate::Pareto2Status::Ok);
  CHECK(fit.min==0);
  CHECK(fit.scale==doctest::Approx(.5).epsilon(1e-10));
  const auto critical=cppunivariate::pareto2_mle(mltest::as_span(x),NAN,1e-10,100,1);
  CHECK(cppunivariate::outcome(critical).reason==cppunivariate::OutcomeReason::Unattained);
}
