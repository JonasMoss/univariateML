#include "test_support.hpp"
#include "cppunivariate/distributions/pareto4/dpqr.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
TEST_CASE("pareto4: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::pareto4_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::burr_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("pareto4: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dpareto4(1,2,3,1,0,false) == doctest::Approx(.75));
}

#include "cppunivariate/distributions/pareto4/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("pareto4: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qpareto4(0,2,2,1,0,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qpareto4(0,2,2,1,0,false,false))==true);
  CHECK(cppunivariate::distributions::ppareto4(-1,2,2,1,0,true,false)==0);
  CHECK(cppunivariate::distributions::ppareto4(std::numeric_limits<double>::infinity(),2,2,1,0,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qpareto4(.1,2,2,1,0,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rpareto4(rng,2,2,1,0));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::ppareto4(point,2,2,1,0,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float((1+mp.mpf(100)**2)**(-2))
TEST_CASE("pareto4: independent extreme value") {
 CHECK(cppunivariate::distributions::ppareto4(100.0,2,2,1,0,false,false)==doctest::Approx(9.9980002999600055e-09).epsilon(1e-8));
}

TEST_CASE("P2 pareto4 golden holds") {
 auto g=mltest::load_distribution_golden("pareto4");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=1;mask<8;++mask){double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan,s=mask&4?g.estimates.at("scale"):nan;auto r=cppunivariate::pareto4_mle(mltest::as_span(g.data),g.estimates.at("min"),1e-9,1000,s,a,b);REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 if(mask&4)CHECK(r.scale==s);
 CHECK(r.logLik>=g.logLik-1e-5);}
}


TEST_CASE("pareto4: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pareto4",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dpareto4(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ppareto4(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qpareto4(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rpareto4(rng, p[0], p[1], p[2], p[3]); });
}

// mpmath reproduction: mp.mp.dps=300; s=mp.mpf(3); m=mp.mpf(4)
// pareto2: m+s*mp.expm1(500/2); pareto3: m+s*mp.exp(500/4)
// pareto4: m+s*mp.expm1(500/2)**mp.mpf('.25') (upper log input -500).
TEST_CASE("pareto4: shifted deep-tail delegation") {
  CHECK(cppunivariate::distributions::qpareto4(-500, 2, 4, 3, 4, false, true) == doctest::Approx(4+3*std::pow(std::expm1(250.0),.25)).scale(0.0));
  CHECK(cppunivariate::distributions::qpareto4(-INFINITY, 2, 4, 3, 4, true, true) == 4);
  CHECK(cppunivariate::distributions::ppareto4(INFINITY, 2, 4, 3, 4, false, true) == -INFINITY);
}

TEST_CASE("pareto4: default golden coefficients and likelihood") {
  const auto g = mltest::load_distribution_golden("pareto4");
  const auto r = cppunivariate::pareto4_mle(mltest::as_span(g.data), g.estimates.at("min"), 1e-10, 1000);
  REQUIRE(r.status == cppunivariate::BurrStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_pareto4).scale(0));
  CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_pareto4).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_pareto4).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_pareto4).scale(0));
}

TEST_CASE("pareto4: TASK-132 precision comparison retains independent likelihood and score") {
  const auto g=mltest::load_distribution_golden("pareto4");
  const auto r=cppunivariate::pareto4_mle(mltest::as_span(g.data),g.estimates.at("min"),1e-10,1000);
  REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
  // Analytic total likelihood scores in log(shape1), log(shape2), log(scale).
  // These are independent of the optimizer's finite-difference objective.
  long double ll=0, gk=g.data.size(), gc=g.data.size();
  long double gs=-static_cast<long double>(g.data.size())*r.shape2;
  for(double v:g.data) {
    const long double t=std::log((static_cast<long double>(v)-r.min)/r.scale);
    const long double z=r.shape2*t;
    const long double sp=std::max(z,0.0L)+std::log1p(std::exp(-std::abs(z)));
    const long double p=z>0?1/(1+std::exp(-z)):std::exp(z)/(1+std::exp(z));
    ll+=std::log(static_cast<long double>(r.shape1))+std::log(static_cast<long double>(r.shape2))-
      std::log(static_cast<long double>(r.scale))+(r.shape2-1)*t-(r.shape1+1)*sp;
    gk-=r.shape1*sp;
    gc+=z-(r.shape1+1)*z*p;
    gs+=(r.shape1+1)*r.shape2*p;
  }
  CHECK(std::abs(ll-g.logLik)<1e-8L);
  CHECK(std::abs(ll-r.logLik)<1e-8L);
  CHECK(std::sqrt(gk*gk+gc*gc+gs*gs)<1e-3L);
}

namespace {
#include "../burr/certificate-fixtures.inc"
}
TEST_CASE("pareto4: transformed certificate workloads and guarded outcomes") {
  for(const auto& f:burr_certificate_fixtures) {
    CAPTURE(f.name);
    std::vector<double> x=f.x;
    for(double& v:x)v+=2;
    auto y=x;for(double& v:y)v-=2;
    const auto r=cppunivariate::pareto4_mle_certified(mltest::as_span(x),2,1e-8,1000);
    const auto b=cppunivariate::burr_mle(mltest::as_span(y),1e-8,1000);
    CHECK(r.status==b.status);
    CHECK(r.min==2);
    if(b.status==cppunivariate::BurrStatus::Ok || b.status==cppunivariate::BurrStatus::WeibullBoundary || b.status==cppunivariate::BurrStatus::ParetoBoundary) {
      CHECK(r.shape1==b.shape1);
      CHECK(r.shape2==b.shape2);
      CHECK(r.scale==b.scale);
      CHECK(r.logLik==doctest::Approx(b.logLik+0).epsilon(1e-11));
      if(b.status==cppunivariate::BurrStatus::Ok) {
        double ll=0;
        for(double v:x) ll+=cppunivariate::distributions::dpareto4(v,r.shape1,r.shape2,r.scale,r.min,true);
        CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
      }
    } else {
      CHECK(std::isnan(r.shape1));CHECK(std::isnan(r.shape2));
      CHECK(std::isnan(r.scale));CHECK(std::isnan(r.logLik));
    }
  }
}

TEST_CASE("pareto4: certified candidate rejects invalid thresholds and excesses") {
  const std::vector<double> x={.2,.5,1,2,4};
  for(double min:std::vector<double>{INFINITY,NAN,4.}) {
    auto bad=x;
    if(min==-std::numeric_limits<double>::max())bad.back()=std::numeric_limits<double>::max();
    CHECK(cppunivariate::pareto4_mle_certified(mltest::as_span(bad),min,1e-8,1000).status==cppunivariate::BurrStatus::InvalidInit);
  }
  for(double tol:std::vector<double>{0.,-1.,INFINITY,NAN})
    CHECK(cppunivariate::pareto4_mle_certified(mltest::as_span(x),0,tol,1000).status==cppunivariate::BurrStatus::InvalidInit);
  const std::vector<double> constant={2,2,2};
  CHECK(cppunivariate::pareto4_mle_certified(mltest::as_span(constant),1,1e-8,1000).status==cppunivariate::BurrStatus::Unbounded);
}

TEST_CASE("pareto4: certified excess arithmetic cannot create ties or overflow") {
  const std::vector<double> close={1,std::nextafter(1.,INFINITY),2};
  const auto r=cppunivariate::pareto4_mle_certified(mltest::as_span(close),-1e100,1e-8,1000);
  CHECK(r.status==cppunivariate::BurrStatus::NumericalFailure);
  CHECK(std::isnan(r.logLik));CHECK(std::isnan(r.shape1));
  const std::vector<double> large={1,std::numeric_limits<double>::max()};
  CHECK(cppunivariate::pareto4_mle_certified(mltest::as_span(large),-std::numeric_limits<double>::max(),1e-8,1000).status==cppunivariate::BurrStatus::NumericalFailure);
}

TEST_CASE("pareto4: public joint path preserves every certificate outcome") {
  for(const auto& f:burr_certificate_fixtures) {
    const auto a=cppunivariate::pareto4_mle(mltest::as_span(f.x),0,1e-8,1000);
    const auto b=cppunivariate::pareto4_mle_certified(mltest::as_span(f.x),0,1e-8,1000);
    CHECK(a.status==b.status);
    if(std::isfinite(b.logLik)) CHECK(a.logLik==b.logLik);
    else CHECK(std::isnan(a.logLik));
    if(b.status==cppunivariate::BurrStatus::Ok) {
      CHECK(a.shape1==b.shape1); CHECK(a.shape2==b.shape2); CHECK(a.scale==b.scale);
    }
  }
}

TEST_CASE("Pareto IV transports parent finite and face witnesses") {
 const std::vector<double> x{2,3,4,5},y{1,2,3,4};
 const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),1,1e-10,1000,NAN,NAN,2);
 const auto p=cppunivariate::burr_mle_holds(mltest::as_span(y),NAN,2,NAN,1e-10,1000);
 CHECK(r.status==p.status);
 CHECK(r.stop_detail.limit_coef==p.stop_detail.limit_coef);
 CHECK(r.stop_detail.limit_loglik==doctest::Approx(p.stop_detail.limit_loglik));
}

TEST_CASE("Pareto IV held-scale inherits finite and budget certificates") {
 const std::vector<double> x{2.5,3,4,6},y{.5,1,2,4};
 const auto a=cppunivariate::pareto4_mle(mltest::as_span(x),2,1e-10,1000,1);
 const auto b=cppunivariate::burr_mle_fixed_scale(mltest::as_span(y),1,1e-10);
 REQUIRE(a.status==cppunivariate::BurrStatus::Ok);
 CHECK(a.shape1==b.shape1);CHECK(a.shape2==b.shape2);CHECK(a.logLik==b.logLik);
 REQUIRE(a.stop_detail.candidates.size()==1);
 CHECK(a.stop_detail.candidates[0].coef.size()==4);
 const auto budget=cppunivariate::pareto4_mle(mltest::as_span(x),2,1e-10,1,1);
 CHECK(budget.status==cppunivariate::BurrStatus::Ok);
 CHECK(std::isfinite(budget.shape1));CHECK(std::isfinite(budget.stop_detail.loglik_lower));
 const std::vector<double> close{1,std::nextafter(1.,INFINITY),2};
 CHECK(cppunivariate::pareto4_mle(mltest::as_span(close),-1e100,1e-10,1000,1e100).status==cppunivariate::BurrStatus::NumericalFailure);
}

TEST_CASE("Pareto IV refuses inexact threshold subtraction onto held scale") {
 const std::vector<double> x{1.1,2,3};
 CHECK(cppunivariate::pareto4_mle(mltest::as_span(x),.1,1e-9,1000,1).status==cppunivariate::BurrStatus::NumericalFailure);
}

TEST_CASE("Pareto IV centered held-shape roots reject an exhausted search") {
  const std::vector<double> x{57.95719829085132,57.95719829090928,57.95719829096724,57.9571982910252,57.95719829114111};
  const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),0,1e-10,1,NAN,.3157266252310832,NAN);
  CHECK(r.status==cppunivariate::BurrStatus::IterationFailure);
  CHECK(std::isnan(r.logLik));
}

// Raw score roots checked independently at 350 and 700 digits.
TEST_CASE("pareto4: TASK-101.7.3 raw likelihood regressions") {
  struct Fixture { int index; std::vector<double> x; double a,b,s,lo,ll; };
  const std::vector<Fixture> fixtures{
    {5666,{57.95719829085132,57.95719829090928,57.95719829096724,57.9571982910252,57.95719829114111},0.3157266252310832,1813936513691.6501,57.95719829088866,0.0,108.0395367269637},
    {5669,{57.95719829085132,57.95719829090928,57.95719829096724,57.9571982910252,57.95719829114111},0.3157266252310832,1813936513691.6501,57.95719829088866,0.0,108.0395367269637},
  };
  for(const auto& f:fixtures) {
    CAPTURE(f.index);
    const auto r=cppunivariate::pareto4_mle(mltest::as_span(f.x),f.lo,1e-10,1000,NAN,f.a,NAN);
    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);
    CHECK(r.shape1==doctest::Approx(f.a).epsilon(1e-10));
    CHECK(r.shape2==doctest::Approx(f.b).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(f.s).epsilon(1e-10));
    CHECK(r.min==doctest::Approx(f.lo).epsilon(1e-10));
    CHECK(r.logLik==doctest::Approx(f.ll).epsilon(1e-10));
  }
}

TEST_CASE("Pareto IV centered profile rejects a separated scale restoration loss") {
  const std::vector<double> x{1,1.5,2};
  // The ideal scale is 1e100 plus an order-one displacement, which cannot
  // survive restoration to binary64. The profile likelihood separates it
  // from the likelihood at the rounded scale.
  const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),-1e100,1e-10,1000,NAN,.3157266252310832,NAN);
  CHECK(r.status==cppunivariate::BurrStatus::NumericalFailure);
  CHECK(std::isnan(r.logLik));
}

TEST_CASE("pareto4 transports an uncertified held-scale search") {
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};
 const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),0,1.220703e-4,1000,1);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
}

TEST_CASE("pareto4 retains a converged joint search after short certificate work") {
 const auto g=mltest::load_distribution_golden("pareto4");
 const auto r=cppunivariate::pareto4_mle(mltest::as_span(g.data),0,1e-8,16);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
}

TEST_CASE("pareto4 transports an unclosed held-power face") {
 const std::vector<double> x{1,1,1,1,6.0001};
 const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),0,1e-14,1000,NAN,NAN,1);
 REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
 CHECK(std::isfinite(r.logLik));
}

TEST_CASE("pareto4: exact-input extreme arithmetic regression") {
  CHECK(cppunivariate::distributions::qpareto4(-0.6931471805599453,1.0,1e-08,1.0,0.0,true,true) == doctest::Approx(1.0000000046380937).epsilon(1e-10).scale(0));
}

TEST_CASE("pareto4: Weak exponential-limit held powers retain native coefficient accuracy") {
  {
    const std::vector<double> x{2.0,2.0,2.0,2.0,3.4494897448244197};
    const auto r=cppunivariate::pareto4_mle(mltest::as_span(x),1,1e-10,1000,NAN,NAN,2);
    REQUIRE(r.status==cppunivariate::BurrStatus::Ok);CHECK_FALSE(r.certified);
    CHECK(r.shape1==doctest::Approx(666666610.92630697381779917077968275295298369167313287087387395132677813352875519665091381025665).scale(0.).epsilon(1e-10));
    CHECK(r.scale==doctest::Approx(36514.835631369714294825408120922276675386090363445046754314358011288694714220507288532125474644).scale(0.).epsilon(1e-10));
    CHECK(r.shape2==2.0);
  }
}

TEST_CASE("Coupled Weibull-limit pareto4 fallback retains coefficient accuracy") {
  { const std::vector<double> x{1.0229442040069199,1.077961541469712,1.1335313926245227,1.192371892647456,1.25489224962879,1.3215836241274623,1.3930425881096071,1.4700036292457357,1.5533852381847866,1.6443570163905132,1.744440474947496,1.85566611005772,1.9808292530117262,2.1239300966523995,2.290984181315566,2.4916548767777167,2.7429693050586232,3.0794415416798357,3.5902671654458267,5.249486463117075};
    double k=1011514.4451644723195882086701604903*(1+1e-5),c=1.0000003493627437731359920084370786*(1+1e-8),s=1022318.8477392338820785229577984847*(1+1e-5);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,s,{{true,true,true}},1e-10,1000,false,1));
    CHECK(k==doctest::Approx(1011514.4451644723195882086701604903).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.0000003493627437731359920084370786).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(1022318.8477392338820785229577984847).scale(0.).epsilon(1e-10));
  }
  { const std::vector<double> x{1.0229442040069199,1.077961541469712,1.1335313926245227,1.192371892647456,1.25489224962879,1.3215836241274623,1.3930425881096071,1.4700036292457357,1.5533852381847866,1.6443570163905132,1.744440474947496,1.85566611005772,1.9808292530117262,2.1239300966523995,2.290984181315566,2.4916548767777167,2.7429693050586232,3.0794415416798357,3.5902671654458267,5.249482256129683};
    double k=101151329.94424823635129160417085871*(1+1e-5),c=1.0000000034936315373718039433251065*(1+1e-8),s=102232319.87551729384381398004294937*(1+1e-5);
    REQUIRE(cppunivariate::burr_local_accuracy(mltest::as_span(x),k,c,s,{{true,true,true}},1e-10,1000,false,1));
    CHECK(k==doctest::Approx(101151329.94424823635129160417085871).scale(0.).epsilon(1e-10));
    CHECK(c==doctest::Approx(1.0000000034936315373718039433251065).scale(0.).epsilon(1e-10));
    CHECK(s==doctest::Approx(102232319.87551729384381398004294937).scale(0.).epsilon(1e-10));
  }
}

TEST_CASE("pareto4: concentrated density retains the scale displacement") {
  CHECK(cppunivariate::distributions::dpareto4(999.9976974175579,1,1e8,1000,0,true)==
        doctest::Approx(-218.7455815369823).epsilon(1e-12));
}

TEST_CASE("pareto4: archived density seam 5665") {
  const std::vector<double> data{57.95719713182328,57.95719771139526,57.95719829096724,57.95719887053922,57.95720002968319};
  const std::vector<double> expected{12.27889959374499,13.045079346465263,12.919212765394823,12.436220911877216,11.308505069224625};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dpareto4(data[i],0.3157266252310832,181397157.91800258,57.95719750520992,0.0,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}

TEST_CASE("pareto4: archived density seam 5666") {
  const std::vector<double> data{57.95719829085132,57.95719829090928,57.95719829096724,57.9571982910252,57.95719829114111};
  const std::vector<double> expected{21.489228490519867,22.2554047373488,22.12952272425259,21.646520330786835,20.51886044379439};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dpareto4(data[i],0.3157266252310832,1813936513691.6497,57.95719829088866,0.0,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}

TEST_CASE("pareto4: archived density seam 5668") {
  const std::vector<double> data{57.95719713182328,57.95719771139526,57.95719829096724,57.95719887053922,57.95720002968319};
  const std::vector<double> expected{12.27889959374499,13.045079346465263,12.919212765394823,12.436220911877216,11.308505069224625};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dpareto4(data[i],0.3157266252310832,181397157.91800258,57.95719750520992,0.0,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}

TEST_CASE("pareto4: archived density seam 5669") {
  const std::vector<double> data{57.95719829085132,57.95719829090928,57.95719829096724,57.9571982910252,57.95719829114111};
  const std::vector<double> expected{21.489228490519867,22.2554047373488,22.12952272425259,21.646520330786835,20.51886044379439};
  for(std::size_t i=0;i<data.size();++i) {
    const double actual=cppunivariate::distributions::dpareto4(data[i],0.3157266252310832,1813936513691.6497,57.95719829088866,0.0,true);
    CHECK(std::abs(actual-expected[i])<=1e-10*std::max(1.0,std::abs(expected[i])));
  }
}
