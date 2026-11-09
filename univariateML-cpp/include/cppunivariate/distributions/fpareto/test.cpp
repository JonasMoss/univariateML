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
