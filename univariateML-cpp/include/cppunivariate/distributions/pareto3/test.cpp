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
 CHECK(r.scale==doctest::Approx(2));
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
