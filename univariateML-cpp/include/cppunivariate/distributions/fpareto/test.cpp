#include "test_support.hpp"
#include "cppunivariate/distributions/fpareto/dpqr.hpp"
#include "cppunivariate/distributions/fpareto/mle.hpp"
TEST_CASE("fpareto: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::fpareto_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::gb2_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("fpareto: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dfpareto(1,2,3,1,1,0,false) == doctest::Approx(.75));
}

#include "cppunivariate/distributions/fpareto/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("fpareto: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qfpareto(0,20,2,20,1,0,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qfpareto(0,20,2,20,1,0,false,false))==true);
  CHECK(cppunivariate::distributions::pfpareto(-1,20,2,20,1,0,true,false)==0);
  CHECK(cppunivariate::distributions::pfpareto(std::numeric_limits<double>::infinity(),20,2,20,1,0,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qfpareto(.1,20,2,20,1,0,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rfpareto(rng,20,2,20,1,0));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pfpareto(point,20,2,20,1,0,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; x=mp.mpf('1e-100'); z=x*x/(1+x*x)
// float(mp.log(mp.betainc(20,20,0,z,regularized=True)))
TEST_CASE("fpareto: log beta tails beyond probability underflow") {
  CHECK(cppunivariate::distributions::pfpareto(1e-100,20,2,20,1,0,true,true)==doctest::Approx(-9185.384112363492).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qfpareto(lp,20,2,20,1,0,lower,true);
    REQUIRE(q>0);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pfpareto(q,20,2,20,1,0,lower,true)==doctest::Approx(lp).epsilon(2e-11));
  }
}

TEST_CASE("P2 fpareto constant conditional shape fits") {
 std::vector<double> x(5,2.0);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(int mask=0;mask<3;++mask) {
   double q=mask==0?nan:2,a=mask==2?nan:3,p=mask==1?nan:2;
   auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,300,q,a,p,1);
   REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
   CHECK(std::isfinite(r.logLik));
   CHECK(r.scale==1);
   if(mask!=0) CHECK(r.shape1==q);
   if(mask!=1) CHECK(r.shape3==p);
   if(mask!=2) CHECK(r.shape2==a);
 }
}


TEST_CASE("fpareto: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("fpareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfpareto(x, p[0], p[1], p[2], p[3], p[4], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfpareto(x, p[0], p[1], p[2], p[3], p[4], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfpareto(x, p[0], p[1], p[2], p[3], p[4], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfpareto(rng, p[0], p[1], p[2], p[3], p[4]); });
}

TEST_CASE("Feller Pareto held unit beta shape and scale inherit scalar certificate") {
 const std::vector<double> x{2.1,2.2,2.4,2.8};
 const auto a=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-9,1000,NAN,NAN,1,1);
 REQUIRE(a.status==cppunivariate::Gb2Status::Ok);
 REQUIRE(a.stop_detail.candidates.size()==1);
 CHECK(a.stop_detail.candidates[0].coef.size()==5);
 const auto stop=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-9,1,NAN,NAN,1,1);
 CHECK(stop.status==cppunivariate::Gb2Status::Ok);
 CHECK(std::isfinite(stop.stop_detail.loglik_lower));
 const std::vector<double> close{1,std::nextafter(1.,INFINITY),2};
 CHECK(cppunivariate::fpareto_mle(mltest::as_span(close),-1e100,1e-9,1000,NAN,NAN,1,1e100).status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("Feller Pareto refuses inexact threshold subtraction onto held scale") {
 const std::vector<double> x{1.1,2,3};
 CHECK(cppunivariate::fpareto_mle(mltest::as_span(x),.1,1e-9,1000,NAN,NAN,1,1).status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("Feller Pareto transports held-power certificates and excess face coordinates") {
 const std::vector<double>x{2.03,2.1,2.3,2.7,3,4,7,22};
 std::vector<double> y;for(double v:x)y.push_back(v-2);
 const auto a=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-7,1000,NAN,.7,2,NAN);
 const auto b=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,1000,NAN,.7,2,NAN);
 CHECK(a.status==b.status);CHECK(a.logLik==b.logLik);
 REQUIRE(a.stop_detail.candidates.size()==b.stop_detail.candidates.size());
 for(std::size_t i=0;i<a.stop_detail.candidates.size();++i) {
  auto expected=b.stop_detail.candidates[i].coef;
  if(b.stop_detail.candidates[i].model==cppunivariate::StopModel::None)expected.push_back(2);
  CHECK(a.stop_detail.candidates[i].coef==expected);
  CHECK(a.stop_detail.candidates[i].model==b.stop_detail.candidates[i].model);
 }
 const std::vector<double>constant(5,4);
 const auto face=cppunivariate::fpareto_mle(mltest::as_span(constant),2,1e-7,300,NAN,2,2,NAN);
 CHECK(face.status==cppunivariate::Gb2Status::TrGammaBoundary);
 CHECK(face.stop_detail.limit_model==cppunivariate::StopModel::Trgamma);
 REQUIRE_FALSE(face.stop_detail.candidates.empty());
 CHECK(face.stop_detail.candidates[0].model==cppunivariate::StopModel::Trgamma);
 CHECK(face.stop_detail.candidates[0].coef.size()==3);
}

TEST_CASE("Feller Pareto held-power subtraction cannot invent a gamma face") {
 const std::vector<double>x{1,std::nextafter(1.,2.)};
 const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),-1e100,1e-7,300,NAN,2,2,NAN);
 CHECK(r.status==cppunivariate::Gb2Status::NumericalFailure);
}

TEST_CASE("Feller Pareto generic budget faces also stay on the excess scale") {
 const auto g=mltest::load_distribution_golden("trgamma");
 auto x=g.data;for(double& v:x)v+=2;
 const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-8,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::CertificateBudget);
 bool found=false;
 for(const auto& c:r.stop_detail.candidates)if(c.model==cppunivariate::StopModel::Trgamma) {
  found=true;CHECK(c.coef.size()==3);
 }
 CHECK(found);
}

TEST_CASE("Feller Pareto transports held-shape concavity certificate and numerical stop") {
 const std::vector<double>x{2.03,2.1,2.3,2.7,3,4,7,22};
 auto y=x;for(double&v:y)v-=2;
 const auto a=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-7,300,2,NAN,3,NAN);
 const auto b=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,300,2,NAN,3,NAN);
 REQUIRE(a.status==cppunivariate::Gb2Status::Ok);
 CHECK(a.shape2==b.shape2);CHECK(a.scale==b.scale);CHECK(a.logLik==b.logLik);
 REQUIRE(a.stop_detail.candidates.size()==1);
 CHECK(a.stop_detail.candidates[0].coef.size()==5);
 CHECK(a.stop_detail.candidates[0].coef.back()==2);
 const auto low=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-7,1,2,NAN,3,NAN);
 CHECK(low.status==cppunivariate::Gb2Status::NumericalFailure);
 CHECK_FALSE(low.stop_detail.budget_exhausted);
}

TEST_CASE("Feller Pareto subunit held beta shape selects the excess endpoint face") {
 const std::vector<double>x{2.1,2.2,2.4,2.8};
 const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-7,1,.4,NAN,NAN,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::PowerBoundary);
 REQUIRE(r.stop_detail.limit_coef.size()==2);
 CHECK(r.stop_detail.limit_coef[0]==1);
 CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::Power);
 REQUIRE(r.stop_detail.candidates.size()==1);
 CHECK(r.stop_detail.candidates[0].coef.size()==2);
}

TEST_CASE("Feller Pareto transports global held-scale mixed-power certificates") {
 const std::vector<double>x{2.03,2.1,2.3,2.7,3,4,7,22};
 auto y=x;for(double&v:y)v-=2;
 const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),2,1e-7,300,2,NAN,NAN,1);
 const auto s=cppunivariate::gb2_mle(mltest::as_span(y),1e-7,300,2,NAN,NAN,1);
 REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
 CHECK(r.shape2==s.shape2);CHECK(r.shape3==s.shape3);CHECK(r.logLik==s.logLik);
 REQUIRE(r.stop_detail.candidates.size()==1);
 REQUIRE(r.stop_detail.candidates[0].coef.size()==5);
 CHECK(r.stop_detail.candidates[0].coef.back()==2);
}

TEST_CASE("fpareto: preserves the GB2 uncertified restoration flag") {
  const double e=std::numeric_limits<double>::epsilon();
  const std::vector<double> data{1,1+e,1+2*e,1+3*e,1+4*e};
  const auto fit=cppunivariate::fpareto_mle(mltest::as_span(data),0,1e-10,100,1,NAN,.855,NAN);
  REQUIRE(fit.status==cppunivariate::Gb2Status::Ok);
  CHECK_FALSE(fit.certified);
  CHECK(std::isfinite(fit.logLik));
}

TEST_CASE("Feller Pareto keeps an uncertified GB2 delegate") {
 const std::vector<double> x{std::exp(-1.1),std::exp(-1.)};const auto d=mltest::as_span(x);
 const auto p=cppunivariate::gb2_mle(d,1e-8,1000,NAN,NAN,1,1);
 const auto r=cppunivariate::fpareto_mle(d,0,1e-8,1000,NAN,NAN,1,1);
 REQUIRE(p.status==cppunivariate::Gb2Status::Ok);REQUIRE(r.status==p.status);
 CHECK_FALSE(p.certified);CHECK(r.certified==p.certified);CHECK(r.shape1==p.shape1);CHECK(r.shape2==p.shape2);CHECK(r.shape3==p.shape3);CHECK(r.scale==p.scale);CHECK(r.logLik==p.logLik);
}

// Independent central log-probability expansion; exact binary64 input.
TEST_CASE("fpareto: central inverse retains a translated residual") {
  CHECK(cppunivariate::distributions::qfpareto(-0.6931471805599453,2,1e8,2,1e8,-1e8,true,true) ==
        doctest::Approx(3.092062418461733e-17).epsilon(1e-10).scale(0));
}

// The exact binary64 product 1e8*1e-8 exceeds one, although it rounds to one.
TEST_CASE("fpareto: exact power side at the support endpoint") {
  CHECK(cppunivariate::distributions::dfpareto(0,1,1e8,1e-8,1,0,true) == -std::numeric_limits<double>::infinity());
}

// Independent 1600-digit exact-input reference; beta-smoke.py.
TEST_CASE("fpareto: centered large-shape density") {
  CHECK(cppunivariate::distributions::dfpareto(1.0,100000000.0,1,100000000.0,1,0,true)==
        doctest::Approx(7.944828247241538).epsilon(1e-10).scale(0));
}

TEST_CASE("fpareto: portable small-shape complementary tail") {
 CHECK(cppunivariate::distributions::pfpareto(5e-324,2.0,1.0,2e-08,1.0,0.0,false,true)==doctest::Approx(-11.116252837780058).epsilon(1e-10));
}

TEST_CASE("fpareto: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dfpareto(1.1350862928982455,3.0861599286892316e+16,1.2356446103149665e-08,3.0861599294133976e+16,1.0087842921157315,0.0,true)==doctest::Approx(-0.6287034440723621).epsilon(1e-10));
}

TEST_CASE("Feller Pareto shifted small-power delegate preserves coefficient accuracy") {
  const std::vector<double> x{-0.9214649596795436,0.8052817030312258,-0.8794500440544186,-0.94038306419953,-0.8578562107426755,-0.5330042262055368,-0.6058183156150942,-0.8552011020269872,-0.8650718678225906,-0.9131779105776012,1.6691108495205547,-0.7495125418805053,-0.9342347883565515,-0.9070418110427749,-0.8137499120449846,-0.8832017297669015,-0.6031416207431743,-0.8357494184006476,-0.8345401934239036,-0.7768843416522309,-0.9440393414775035,-0.9301468388918922,-0.16713815926646924,-0.8933883275895507,-0.7810962145460771,-0.973491063922652,-0.8564847306035056,-0.3018277157710706,-0.8895306220601406,-0.8953419504699215,-0.7368659214847622,-0.6109156382164084,-0.9134973845491653,-0.7197476622532691,-0.1115184175143471,0.5240183611033451,-0.915357339803346,-0.3594178394401816,-0.6675255921229256,-0.5521462960848786,-0.9581894843135835,-0.7595560959239014,-0.9741704759253846,-0.6178582794004865,-0.9485966746158716,-0.9837048144865806,-0.8262981273151637,-0.8352059470163766,-0.2875627028269807,-0.9579134964998978,-0.7556943151040054,-0.9314072290592983,-0.9309586129077811,-0.18416779494913804,-0.47172818978401465,-0.9181842727552808,1.0944026714447435,0.05024372935570964,-0.9242009517124405,-0.9456501645041582,0.9909394766284128,-0.8093136559727943,-0.4150131260714647,-0.9695296083850372,-0.9359507059271471,-0.9161908548566309,0.6930664596676608,-0.6790304596326251,-0.9341157211021279,-0.8972875710975925,-0.9440017696678997,-0.6401608398490215,-0.9282654288365675,-0.74669606234718,-0.5273730915206909,-0.8966737340210387,-0.9798047166128145,-0.32920983114635627,-0.831410821562459,-0.670473686290163,-0.9553451426837769,-0.8906545821269859,-0.8561099101226859,-0.4974309967093806,3.529005885777762,-0.9546126866303658,1.7601163143875178,-0.950376000426306,-0.9161101243863532,43.601555292678334,-0.9076113680700507,-0.9188084619165454,-0.8349667333008088,-0.9511211729568833,-0.7421022186468236,-0.9668541991293433,-0.9285269374835531,-0.18777542381117185,-0.8952247869528144,-0.4707853765488056};
  const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),-1,1e-10,64,NAN,.5,0.6611055385779123,NAN);
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);CHECK_FALSE(r.certified);
  // Independent high-precision stationary point of the original density.
  CHECK(r.shape1==doctest::Approx(903.42459848503018709681433141762496194656947889977050180189860907).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(625653.11035127752986411643771357591891618246514876143687797578969).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==0.5);
  CHECK(r.shape3==0.6611055385779123);
  CHECK(r.min==-1);
}

TEST_CASE("Comparable beta shapes fpareto transport propagates small-power error") {
  const std::vector<double> x{1.3733876795318397e-88,2.937594362709469e-65,2.701378714528384e-52,1.2699376077132869e-42,1.3673899352578955e-34,1.5970627146986006e-27,4.555442832854519e-21,5.19854194459634e-15,3.3274256113905883e-09,0.0015450781141231093,647.2164681254823,300532638.97973114,192361629598590.03,2.195176268680371e+20,6.261494873039616e+26,7.313202870783376e+33,7.8744025999969e+41,3.701813428166952e+51,3.40414596614879e+64,7.281265260368941e+87};
  cppunivariate::Gb2Result r{20000.458802500446887116828672063727*(1+1e-5),1e-4,20000.45880248738,1.0000000065331610723249177026237890*(1+1e-5),0,0,cppunivariate::Gb2Status::Ok};r.certified=false;
  REQUIRE(cppunivariate::fpareto_transport_accuracy(mltest::as_span(x),-1e-100,r,{{true,false,false,true}},1e-10,1000));
  CHECK(r.shape1==doctest::Approx(20000.458802500446887116828672063727).scale(0.).epsilon(1e-10));
  CHECK(r.scale==doctest::Approx(1.0000000065331610723249177026237890).scale(0.).epsilon(1e-10));
  CHECK(r.shape2==1e-4);CHECK(r.shape3==20000.45880248738);
}

// TASK-205: archived default fits; work bounds, never wall-clock assertions.
TEST_CASE("Feller Pareto extreme all-free profiles have bounded nested work") {
  SUBCASE("archive 1599") {
    const std::vector<double> x{9.341081677852826e-151,4.595475734747349e-150,5.998711847579505e-151,1.056419269543656e-149,7.000746031939794e-151,1.112552433763363e-150,1.3300121914598085e-148,3.057685931487713e-151,9.471857714817827e-151,3.8097695246334955e-150,1.8225442484990885e-150,1.1865419401405472e-151};
    const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,1000);
    CHECK(r.status!=cppunivariate::Gb2Status::Ok);
    CHECK(r.profile_evaluations>0);
    CHECK(r.profile_evaluations<=cppunivariate::certificate_budget(1000));
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    const auto& c=r.stop_detail.candidates.front();
    REQUIRE(c.coef.size()==5);
    CHECK(c.coef.back()==0);
    CHECK(std::isfinite(c.loglik_lower));
    CHECK(c.loglik_lower<=c.loglik_upper);
    CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  }
  SUBCASE("archive 1600") {
    const std::vector<double> x{9.341081677852825e+149,4.5954757347473486e+150,5.998711847579505e+149,1.056419269543656e+151,7.000746031939793e+149,1.1125524337633629e+150,1.3300121914598085e+152,3.057685931487713e+149,9.471857714817827e+149,3.809769524633495e+150,1.8225442484990886e+150,1.1865419401405473e+149};
    const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,1000);
    CHECK(r.status!=cppunivariate::Gb2Status::Ok);
    CHECK(r.profile_evaluations>0);
    CHECK(r.profile_evaluations<=cppunivariate::certificate_budget(1000));
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    const auto& c=r.stop_detail.candidates.front();
    REQUIRE(c.coef.size()==5);
    CHECK(c.coef.back()==0);
    CHECK(std::isfinite(c.loglik_lower));
    CHECK(c.loglik_lower<=c.loglik_upper);
    CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  }
  SUBCASE("archive 10717") {
    const std::vector<double> x{1e-200,2e-200,3e-200,5e-200,9e-200};
    const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,1000);
    CHECK(r.status!=cppunivariate::Gb2Status::Ok);
    CHECK(r.profile_evaluations>0);
    CHECK(r.profile_evaluations<=cppunivariate::certificate_budget(1000));
    REQUIRE_FALSE(r.stop_detail.candidates.empty());
    const auto& c=r.stop_detail.candidates.front();
    REQUIRE(c.coef.size()==5);
    CHECK(c.coef.back()==0);
    CHECK(std::isfinite(c.loglik_lower));
    CHECK(c.loglik_lower<=c.loglik_upper);
    CHECK(r.stop_detail.loglik_lower<=r.stop_detail.loglik_upper);
  }
}

TEST_CASE("Feller Pareto completed grid profiles preserve Pareto limit decisions") {
  SUBCASE("archive 1601") {
    const std::vector<double> x{0.9341081677852825,4.595475734747349,0.5998711847579505,10.56419269543656,1112685.434982509};
    const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,1000);
    REQUIRE(r.status==cppunivariate::Gb2Status::ParetoBoundary);
    CHECK(r.logLik==cppunivariate::pareto_mle(mltest::as_span(x)).logLik);
    CHECK(r.logLik==doctest::Approx(-29.102002422134035).epsilon(2e-14));
    CHECK(r.profile_evaluations>0);
    CHECK(r.profile_evaluations<=cppunivariate::certificate_budget(1000));
    CHECK_FALSE(r.stop_detail.budget_exhausted);
  }
  SUBCASE("archive 10072") {
    const std::vector<double> x{0.9992918544349726,0.9992918544349726,0.9992918544349726,0.9992918544349726,0.9992918544349726,0.9992918544349726,0.9992918544349726,1.0007086473905342,1.0007086473905342,1.0007086473905342,1.0007086473905342,1.0007086473905342};
    const auto r=cppunivariate::fpareto_mle(mltest::as_span(x),0,1e-7,1000);
    REQUIRE(r.status==cppunivariate::Gb2Status::ParetoBoundary);
    CHECK(r.logLik==cppunivariate::pareto_mle(mltest::as_span(x)).logLik);
    CHECK(r.logLik==doctest::Approx(77.219355967752222).epsilon(2e-14));
    CHECK(r.profile_evaluations>0);
    CHECK(r.profile_evaluations<=cppunivariate::certificate_budget(1000));
    CHECK_FALSE(r.stop_detail.budget_exhausted);
  }
}
