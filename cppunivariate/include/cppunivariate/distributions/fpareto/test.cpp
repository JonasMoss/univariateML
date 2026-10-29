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

TEST_CASE("fpareto: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("fpareto",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dfpareto(x, p[0], p[1], p[2], p[3], p[4], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pfpareto(x, p[0], p[1], p[2], p[3], p[4], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qfpareto(x, p[0], p[1], p[2], p[3], p[4], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rfpareto(rng, p[0], p[1], p[2], p[3], p[4]); });
}
