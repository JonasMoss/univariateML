#include "test_support.hpp"
#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
TEST_CASE("nsbeta: exact affine likelihood") {
 const std::vector<double> y{.05,.1,.2,.4,.6,.7,.85};
 std::vector<double> x(y); for(double &v:x) v=2+3*v;
 auto a=cppunivariate::nsbeta_mle(cppunivariate::span{x.data(),x.size()},2,5,1e-10,1000);
 auto b=cppunivariate::beta_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik-y.size()*std::log(3)));
 CHECK(a.shape1 == doctest::Approx(b.alpha));
}

TEST_CASE("nsbeta: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dnsbeta(.5,2,3,0,1,false) == doctest::Approx(1.5));
}

#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("nsbeta: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qnsbeta(0,20,20,0,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qnsbeta(0,20,20,0,1,false,false))==false);
  CHECK(cppunivariate::distributions::pnsbeta(-1,20,20,0,1,true,false)==0);
  CHECK(cppunivariate::distributions::pnsbeta(std::numeric_limits<double>::infinity(),20,20,0,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qnsbeta(.1,20,20,0,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rnsbeta(rng,20,20,0,1));
  for(double point:{.4,.5,.6}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pnsbeta(point,20,20,0,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float(mp.betainc(20,20,0,mp.mpf(1e-6),regularized=True))
TEST_CASE("nsbeta: independent extreme value") {
 CHECK(cppunivariate::distributions::pnsbeta(1e-06,20,20,0,1,true,false)==doctest::Approx(6.8922017237834511e-110).epsilon(2e-13));
}

TEST_CASE("nsbeta: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("nsbeta",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnsbeta(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnsbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnsbeta(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnsbeta(rng, p[0], p[1], p[2], p[3]); });
}

// import mpmath as mp, math; mp.mp.dps=100
// x=mp.mpf(math.nextafter(5.,2.)); y=(5-x)/3
// print(3*mp.log(y)+mp.log(4-3*y))  # Beta(2,3) upper tail
// print(mp.log(12)+mp.log(1-y)+2*mp.log(y)-mp.log(3))
TEST_CASE("nsbeta: targeted tail affine endpoint distances") {
  const double x=std::nextafter(5.0,2.0);
  CHECK(cppunivariate::distributions::pnsbeta(x,2,3,2,5,false,true)
        ==doctest::Approx(-105.88161958887623).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dnsbeta(x,2,3,2,5,true)
        ==doctest::Approx(-70.12564827221085).epsilon(1e-10));
  CHECK(cppunivariate::distributions::dnsbeta(2,1,3,2,5,true)==0);
  CHECK(cppunivariate::distributions::qnsbeta(0,2,3,2,5,false,false)==5);
}
