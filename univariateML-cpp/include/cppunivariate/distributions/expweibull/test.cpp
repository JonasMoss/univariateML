#include "test_support.hpp"
#include "cppunivariate/distributions/expweibull/dpqr.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>

TEST_CASE("expweibull: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("expweibull",
    [](double x,const std::vector<double>& p,bool l){return cppunivariate::distributions::dexpweibull(x,p[0],p[1],p[2],l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::pexpweibull(x,p[0],p[1],p[2],t,l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::qexpweibull(x,p[0],p[1],p[2],t,l);},
    // At b=1000,c=1e200, a few ulps of h amplify to 2.7e-10
    // relative error in a natural probability of 1e-300.
    [](cppunivariate::distributions::StdRng<>& rng,const std::vector<double>& p){return cppunivariate::distributions::rexpweibull(rng,p[0],p[1],p[2]);},5e-10);
}
TEST_CASE("expweibull: sampler uses the analytic inverse and has uniform PIT") {
  cppunivariate::distributions::StdRng<> rng(179), control(179);
  double sum=0, squares=0;
  for(int i=0;i<10000;++i) {
    const double u=control.runif(0,1);
    const double x=cppunivariate::distributions::rexpweibull(rng,2,1.5,2.5);
    CHECK(x==cppunivariate::distributions::qexpweibull(u,2,1.5,2.5,true,false));
    const double f=cppunivariate::distributions::pexpweibull(x,2,1.5,2.5,true,false);
    sum+=f; squares+=f*f;
  }
  CHECK(std::abs(sum/10000-.5)<.012);
  CHECK(std::abs(squares/10000-1.0/3)<.012);
}
TEST_CASE("expweibull: endpoints invalid inputs and Weibull reduction") {
  const double inf=std::numeric_limits<double>::infinity();
  for(bool lower:{false,true}) for(bool log:{false,true}) {
    CHECK(cppunivariate::distributions::qexpweibull(log ? -inf : 0,2,3,4,lower,log)==(lower ? 0 : inf));
    CHECK(cppunivariate::distributions::qexpweibull(log ? 0 : 1,2,3,4,lower,log)==(lower ? inf : 0));
    CHECK(std::isnan(cppunivariate::distributions::qexpweibull(log ? .1 : 1.1,2,3,4,lower,log)));
    for(double x:{.01,1.,10.,100.})
      CHECK(cppunivariate::distributions::pexpweibull(x,2,3,1,lower,log)==doctest::Approx(cppunivariate::distributions::pweibull(x,3,2,lower,log)).epsilon(1e-12));
  }
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,.5,false)==.5);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,.1,false)==inf);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,2,1,false)==0);
  CHECK(cppunivariate::distributions::dexpweibull(0,2,1000,.001,false)==0);
  CHECK(std::isnan(cppunivariate::distributions::pexpweibull(1,0,2,3,true,false)));
  CHECK(cppunivariate::distributions::dexpweibull(-1,2,3,4,true)==-inf);
}
TEST_CASE("expweibull: underflowed powers and extreme shapes retain log tails") {
  CHECK(cppunivariate::distributions::dexpweibull(1e-200,1,2,.5,true)==doctest::Approx(0).epsilon(1e-12));
  CHECK(cppunivariate::distributions::pexpweibull(1000,1,1,1e300,false,true)==doctest::Approx(std::log(1e300)-1000).epsilon(1e-13));
  for(double b:{.01,1.,100.,1e200}) for(double c:{1e-200,.5,2.,1e200})
    for(bool lower:{false,true}) for(double lp:{-1e-300,-.01,-1.,-100.,-1000.,-10000.}) {
      const double x=cppunivariate::distributions::qexpweibull(lp,1,b,c,lower,true);
      CHECK_FALSE(std::isnan(x));
      if(x>0 && std::isfinite(x) && b<1e10) {
        const double back=cppunivariate::distributions::pexpweibull(x,1,b,c,lower,true);
        INFO("relative="<<(back/lp-1)<<" shape="<<b<<" exponent="<<c<<" lower="<<lower<<" lp="<<lp<<" x="<<x);
        const double left=cppunivariate::distributions::pexpweibull(std::nextafter(x,0.0),1,b,c,lower,true);
        const double right=cppunivariate::distributions::pexpweibull(std::nextafter(x,INFINITY),1,b,c,lower,true);
        CHECK((std::abs(back-lp)<=5e-10*std::abs(lp) ||
              (lp>=std::min(left,right) && lp<=std::max(left,right))));
      }
    }
}

TEST_CASE("expweibull: extreme reciprocal shapes rescue intermediate range") {
  // b*z overflows, but c*b*z and the limiting density remain finite.
  CHECK(cppunivariate::distributions::pexpweibull(1e-100,1,1e308,1e-308,true,true)==doctest::Approx(-100*std::log(10.0)).epsilon(1e-14));
  CHECK(std::abs(cppunivariate::distributions::dexpweibull(1e-100,1,1e308,1e-308,true))<1e-12);
  CHECK(cppunivariate::distributions::qexpweibull(-.5,1,1e308,1e-308,true,true)==doctest::Approx(std::exp(-.5)).epsilon(2e-13));
  // Close scale ratios must retain a one-ulp displacement before multiplying b.
  const double a=1e200, x=std::nextafter(a,INFINITY);
  const double lh=1e16*std::log1p((x-a)/a);
  CHECK(cppunivariate::distributions::pexpweibull(x,a,1e16,1,false,true)==doctest::Approx(-std::exp(lh)).epsilon(1e-14));
}

// mpmath, 100 digits, exact binary64 inputs:
// h=mp.mpf(1e-200)**2; lp=mp.mpf(c)*mp.log(-mp.expm1(-h));
// mp.log(-mp.expm1(lp)). Full values are retained in the job evidence.
TEST_CASE("expweibull: subnormal exponents preserve log survival") {
  CHECK(cppunivariate::distributions::pexpweibull(1e-200,1,2,
      std::numeric_limits<double>::denorm_min(),false,true)==
      doctest::Approx(-737.6145749290254).epsilon(2e-15));
  CHECK(cppunivariate::distributions::pexpweibull(1e-200,1,2,1e-320,false,true)==
      doctest::Approx(-730.001743898618).epsilon(2e-15));
}
