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
