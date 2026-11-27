#include "test_support.hpp"
#include "cppunivariate/distributions/pareto3/dpqr.hpp"
#include "cppunivariate/distributions/pareto3/mle.hpp"
TEST_CASE("pareto3: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::pareto3_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::loglogistic_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("pareto3: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dpareto3(1,2,1,0,false) == doctest::Approx(.5));
}

#include "cppunivariate/distributions/pareto3/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("pareto3: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qpareto3(0,2,1,0,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qpareto3(0,2,1,0,false,false))==true);
  CHECK(cppunivariate::distributions::ppareto3(-1,2,1,0,true,false)==0);
  CHECK(cppunivariate::distributions::ppareto3(std::numeric_limits<double>::infinity(),2,1,0,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qpareto3(.1,2,1,0,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rpareto3(rng,2,1,0));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::ppareto3(point,2,1,0,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float((1+mp.mpf(100)**2)**(-1))
TEST_CASE("pareto3: independent extreme value") {
 CHECK(cppunivariate::distributions::ppareto3(100.0,2,1,0,false,false)==doctest::Approx(9.9990000999900015e-05).epsilon(1e-8));
}

TEST_CASE("P2 pareto3 golden holds") {
 auto g=mltest::load_distribution_golden("pareto3");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<4;++mask){double a=mask&1?g.estimates.at("shape"):nan,s=mask&2?g.estimates.at("scale"):nan;auto r=cppunivariate::pareto3_mle(mltest::as_span(g.data),g.estimates.at("min"),1e-9,1000,a,s);
 if(mask&1)CHECK(r.shape==a);
 if(mask&2)CHECK(r.scale==s);
 CHECK(r.logLik>=g.logLik-1e-5);}
}


TEST_CASE("pareto3: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pareto3",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto3(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto3(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto3(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto3(rng, p[0], p[1], p[2]); });
}

// mpmath reproduction: mp.mp.dps=300; s=mp.mpf(3); m=mp.mpf(4)
// pareto2: m+s*mp.expm1(500/2); pareto3: m+s*mp.exp(500/4)
// pareto4: m+s*mp.expm1(500/2)**mp.mpf('.25') (upper log input -500).
TEST_CASE("pareto3: shifted deep-tail delegation") {
  CHECK(cppunivariate::distributions::qpareto3(-500, 4, 3, 4, false, true) == doctest::Approx(4+3*std::exp(125.0)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto3(-INFINITY, 4, 3, 4, true, true) == 4);
  CHECK(cppunivariate::distributions::ppareto3(INFINITY, 4, 3, 4, false, true) == -INFINITY);
}

TEST_CASE("pareto3: P3 logistic delegate status is preserved") {
  const std::vector<double> values{.3,.8,1.7,4.2};
  CHECK(cppunivariate::pareto3_mle(mltest::as_span(values),0,1e-10,1).status==cppunivariate::Pareto3Status::IterationFailure);
}

TEST_CASE("Pareto III held subsets use log-logistic excess fits") {
 const std::vector<double> x{2,3,4,7,12};
 const std::vector<double> y{1,2,3,6,11};
 for(double shape:{double(NAN),2.0}) for(double scale:{double(NAN),3.0}) {
  const auto r=cppunivariate::pareto3_mle({x.data(),x.size()},1,1e-10,1000,shape,scale);
  const auto b=cppunivariate::loglogistic_mle({y.data(),y.size()},1e-10,1000,shape,scale);
  CHECK(r.logLik==doctest::Approx(b.logLik));
  CHECK(r.shape==doctest::Approx(b.shape));CHECK(r.scale==doctest::Approx(b.scale));
 }
}

TEST_CASE("Pareto III constant sample is finite with shape held") {
 const std::vector<double> x{3,3,3};
 const auto r=cppunivariate::pareto3_mle(mltest::as_span(x),1,1e-10,1000,2,NAN);
 REQUIRE(r.status==cppunivariate::Pareto3Status::Ok);
 CHECK(r.scale==doctest::Approx(2).epsilon(mltest::kFitReferenceTol).scale(0));
 CHECK(r.logLik==doctest::Approx(3*std::log(.25)));
}

TEST_CASE("pareto3: preserves the log-logistic uncertified restoration flag") {
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> data{1,1+e,1+2*e,1+4*e,1+7*e};
  const auto fit=cppunivariate::pareto3_mle(mltest::as_span(data),0,1e-10,100);
  REQUIRE(fit.status==cppunivariate::Pareto3Status::Ok);
  CHECK_FALSE(fit.certified);
  CHECK(std::isfinite(fit.logLik));
}

TEST_CASE("Pareto III retains the represented uncertified log-logistic parent") {
 for(double center:{1.,1000.,1e100})for(int n:{2,4}) {
  std::vector<double> x;double v=center;for(int j=0;j<n;++j){x.push_back(v);v=std::nextafter(v,INFINITY);}const auto d=mltest::as_span(x);
  const auto p=cppunivariate::loglogistic_mle(d,1e-10,1000);const auto r=cppunivariate::pareto3_mle(d,0,1e-10,1000);
  REQUIRE(p.status==cppunivariate::LogLogisticStatus::Ok);REQUIRE(r.status==cppunivariate::Pareto3Status::Ok);
  CHECK_FALSE(p.certified);CHECK(r.certified==p.certified);CHECK(r.logLik>=p.logLik-1e-10);
  double ll=0;for(double v:x){const double t=r.shape*cppunivariate::transformed_detail::log_ratio(v,r.scale);ll+=std::log(r.shape)-std::log(v)-cppunivariate::transformed_detail::softplus(t)-cppunivariate::transformed_detail::softplus(-t);}
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
 }
}

// Independent central log-probability expansion; exact binary64 input.
TEST_CASE("pareto3: central inverse retains a translated residual") {
  CHECK(cppunivariate::distributions::qpareto3(-0.6931471805599453,1e8,1e8,-1e8,false,true) ==
        doctest::Approx(-4.638093627692599e-17).epsilon(1e-10).scale(0));
}

TEST_CASE("pareto3: the inverse displacement survives an underflowed power coordinate") {
  CHECK(cppunivariate::distributions::qpareto3(-0.6931471805599453,1e308,1e308,-1e308,false,true)==
        doctest::Approx(-4.638093627692599e-17).epsilon(1e-10).scale(0));
}

TEST_CASE("pareto3 core distinguishes collapse from invalid observations") {
  const std::vector<double> constant{2,2,2}, invalid{-1,2};
  const auto r=cppunivariate::pareto3_mle(mltest::as_span(constant),0,1e-10,1000);
  CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Nonexistence);
  CHECK(cppunivariate::outcome(r).reason==cppunivariate::OutcomeReason::Unbounded);
  CHECK(cppunivariate::outcome(cppunivariate::pareto3_mle(mltest::as_span(invalid),0,1e-10,1000)).reason==cppunivariate::OutcomeReason::InvalidInput);
}

TEST_CASE("pareto3 rounded threshold subtraction retains the finite maximizer") {
  const std::vector<double> distinct{.1,.2},one{.1};
  for(const auto& x : {distinct,one}) {
    const auto r=cppunivariate::pareto3_mle(mltest::as_span(x),-1e16,1e-10,1000,NAN,x.size()==1?1e16:NAN);
    REQUIRE(r.status==cppunivariate::Pareto3Status::Ok);
    CHECK(cppunivariate::outcome(r).kind==cppunivariate::OutcomeKind::Estimate);
    // On the centered logistic scale, u*tanh(u/2)=1 at the maximum.
    constexpr double root=1.5434046384182084;
    const double expected=(x.size()==1?root:2*root)/std::log1p(.1/1e16);
    CHECK(r.shape==doctest::Approx(expected).epsilon(1e-10));
    CHECK(r.scale==1e16);
    CHECK(std::isfinite(r.logLik));
  }
}

TEST_CASE("pareto3: centered refinement retains the spread of archive 005591") {
  const std::vector<double> x{0.6875776168638714, 0.687577616864559,
      0.6875776168652465, 0.6875776168659341, 0.6875776168673092};
  const auto f = cppunivariate::pareto3_mle(mltest::as_span(x), 0, 1e-10, 1000);
  REQUIRE(f.status == cppunivariate::Pareto3Status::Ok);
  // 80-digit stationary solve treating each observation as exact binary64.
  CHECK(std::fabs(f.shape / 977351135434.3642976093662 - 1) < 1e-10);
  CHECK(std::fabs(f.scale / 0.6875776168653109690504518 - 1) < 1e-10);
}

TEST_CASE("pareto3: held shape and scale give the threshold at the sample mode offset") {
  const std::vector<double> x{2, 2};
  const auto fit=cppunivariate::pareto3_mle(mltest::as_span(x),NAN,1e-10,100,2,1);
  REQUIRE(fit.status==cppunivariate::Pareto3Status::Ok);
  CHECK(fit.min==doctest::Approx(2-std::sqrt(1./3)).epsilon(1e-10));
  const auto unbounded=cppunivariate::pareto3_mle(mltest::as_span(x),NAN,1e-10,100,NAN,1);
  CHECK(cppunivariate::outcome(unbounded).reason==cppunivariate::OutcomeReason::Unbounded);
}

TEST_CASE("pareto3: interior free threshold and scale satisfy both profile scores") {
  const std::vector<double> x{0,1,1,1};
  const auto fit=cppunivariate::pareto3_mle(mltest::as_span(x),NAN,1e-10,100,2);
  REQUIRE(fit.status==cppunivariate::Pareto3Status::Ok);
  CHECK(cppunivariate::outcome(fit).certified);
  CHECK(fit.min<0);
  double threshold_score=0,scale_score=0;
  for(double v:x) {
    const double y=v-fit.min, r=y/fit.scale, u=r*r/(1+r*r);
    threshold_score+=(1-4*u)/y;
    scale_score+=2*u-1;
  }
  CHECK(std::abs(threshold_score)<1e-8);
  CHECK(std::abs(scale_score)<1e-8);
}

TEST_CASE("pareto3: signed scale and threshold brackets close on the golden sample") {
  const auto golden=mltest::load_distribution_golden("pareto3");
  const auto fit=cppunivariate::pareto3_mle(mltest::as_span(golden.data),
      NAN,1e-10,1000,golden.estimates.at("shape"));
  REQUIRE(fit.status==cppunivariate::Pareto3Status::Ok);
  CHECK(fit.certified);
  CHECK(fit.logLik>=golden.logLik-1e-8);
}
