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
  const auto p=cppunivariate::lomax_mle(mltest::as_span(y),1e-10,1000,NAN,std::isnan(scale)?scale:1/scale,shape);
  CHECK(r.status==p.status);
  if(r.status==cppunivariate::LomaxStatus::Ok){CHECK(r.logLik==doctest::Approx(p.logLik));CHECK(r.shape==doctest::Approx(p.kappa));CHECK(r.scale==doctest::Approx(1/p.lambda));}
 }
}
