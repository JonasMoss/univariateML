#include "test_support.hpp"
#include "cppunivariate/distributions/pgw/dpqr.hpp"
#include "cppunivariate/distributions/weibull/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>

TEST_CASE("pgw: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("pgw",
    [](double x,const std::vector<double>& p,bool l){return cppunivariate::distributions::dpgw(x,p[0],p[1],p[2],l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::ppgw(x,p[0],p[1],p[2],t,l);},
    [](double x,const std::vector<double>& p,bool t,bool l){return cppunivariate::distributions::qpgw(x,p[0],p[1],p[2],t,l);},
    [](cppunivariate::distributions::StdRng<>& rng,const std::vector<double>& p){return cppunivariate::distributions::rpgw(rng,p[0],p[1],p[2]);},1e-10);
}
TEST_CASE("pgw: sampler uses the analytic inverse and has uniform PIT") {
  cppunivariate::distributions::StdRng<> rng(179), control(179);
  double sum=0, squares=0;
  for(int i=0;i<10000;++i) {
    const double u=control.runif(0,1);
    const double x=cppunivariate::distributions::rpgw(rng,2,1.5,2.5);
    CHECK(x==cppunivariate::distributions::qpgw(u,2,1.5,2.5,true,false));
    const double f=cppunivariate::distributions::ppgw(x,2,1.5,2.5,true,false);
    sum+=f; squares+=f*f;
  }
  CHECK(std::abs(sum/10000-.5)<.012);
  CHECK(std::abs(squares/10000-1.0/3)<.012);
}

TEST_CASE("pgw: endpoints, invalid input and Weibull identities") {
  const double inf=std::numeric_limits<double>::infinity();
  for(bool lower:{false,true}) for(bool log:{false,true}) {
    CHECK(cppunivariate::distributions::qpgw(log ? -inf : 0,2,3,4,lower,log)==(lower ? 0 : inf));
    CHECK(cppunivariate::distributions::qpgw(log ? 0 : 1,2,3,4,lower,log)==(lower ? inf : 0));
    CHECK(std::isnan(cppunivariate::distributions::qpgw(log ? .1 : 1.1,2,3,4,lower,log)));
    CHECK(cppunivariate::distributions::ppgw(-1,2,3,4,lower,log)==(log ? (lower ? -inf : 0) : (lower ? 0 : 1)));
    CHECK(cppunivariate::distributions::ppgw(inf,2,3,4,lower,log)==(log ? (lower ? 0 : -inf) : (lower ? 1 : 0)));
    for(double x:{.01,1.,10.,100.}) {
      CHECK(cppunivariate::distributions::ppgw(x,2,3,1,lower,log)==doctest::Approx(cppunivariate::distributions::pweibull(x,3,2,lower,log)).epsilon(1e-12));
      CHECK(cppunivariate::distributions::qpgw(log ? -x : std::exp(-x),2,3,1,lower,log)==doctest::Approx(cppunivariate::distributions::qweibull(log ? -x : std::exp(-x),3,2,lower,log)).epsilon(1e-12));
    }
  }
  CHECK(cppunivariate::distributions::dpgw(0,2,.5,3,false)==inf);
  CHECK(cppunivariate::distributions::dpgw(0,2,2,3,false)==0);
  CHECK(cppunivariate::distributions::dpgw(0,2,1,3,false)==1.5);
  CHECK(cppunivariate::distributions::dpgw(-1,2,3,4,true)==-inf);
  CHECK(cppunivariate::distributions::dpgw(inf,2,3,4,true)==-inf);
  for(double bad:{0.,-1.,inf,std::numeric_limits<double>::quiet_NaN()}) {
    CHECK(std::isnan(cppunivariate::distributions::dpgw(1,bad,2,3,true)));
    CHECK(std::isnan(cppunivariate::distributions::ppgw(1,2,bad,3,true,true)));
    CHECK(std::isnan(cppunivariate::distributions::qpgw(.5,2,3,bad,true,false)));
  }
}

TEST_CASE("pgw: overflowed powers and subnormal factors retain log tails") {
  const double e=std::exp(1.0);
  CHECK(cppunivariate::distributions::ppgw(e,1,1e308,1e-308,false,true)==doctest::Approx(-std::expm1(1.0)).epsilon(2e-13));
  CHECK(cppunivariate::distributions::qpgw(-std::expm1(1.0),1,1e308,1e-308,false,true)==doctest::Approx(e).epsilon(2e-13));
  CHECK(cppunivariate::distributions::dpgw(e,1,1e308,1e-308,true)==doctest::Approx(-std::expm1(1.0)).epsilon(2e-13));
  const double c=std::numeric_limits<double>::denorm_min();
  CHECK(cppunivariate::distributions::ppgw(1e-200,1,2,c,true,true)==doctest::Approx(std::log(c)-400*std::log(10.0)).epsilon(2e-15));
  CHECK(cppunivariate::distributions::ppgw(1e-308,1,1,1e308,false,true)==doctest::Approx(-std::expm1(1.0)).epsilon(2e-13));
  const double a=1e200, x=std::nextafter(a,INFINITY);
  const double v=2*cppunivariate::special::softplus(1e16*std::log1p((x-a)/a));
  CHECK(cppunivariate::distributions::ppgw(x,a,1e16,2,false,true)==doctest::Approx(-std::expm1(v)).epsilon(2e-14));
}

TEST_CASE("pgw: both proper boundary paths converge to their stated laws") {
  for(double x:{.5,1.,2.}) {
    const double u=x*x;
    CHECK(cppunivariate::distributions::ppgw(x,1e4,2,1e8,false,true)==doctest::Approx(-std::expm1(u)).epsilon(1e-6));
  }
  for(double x:{2.1,3.,4.}) {
    CHECK(cppunivariate::distributions::ppgw(x,2,1e4,.0002,false,true)==doctest::Approx(1-(x/2)*(x/2)).epsilon(1e-12));
  }
  CHECK(cppunivariate::distributions::ppgw(1.9,2,1e4,.0002,true,true)<-500);
  CHECK(cppunivariate::distributions::dpgw(2,2,1e4,.0002,false)==doctest::Approx(.5).epsilon(1e-6));
}

TEST_CASE("pgw: unresolved inverse cancellation is explicit") {
  // The exact algebraic center is scale, including vanishing shapes.
  CHECK(cppunivariate::distributions::qpgw(-3,1,1e-300,2,false,true)==1);
  // A nearby binary64 hazard gives a finite, noncentral quantile. The
  // division by b amplifies inverse rounding beyond the contract.
  CHECK(std::isnan(cppunivariate::distributions::qpgw(-3.000000004,1e-308,1e-12,2,false,true)));
}

TEST_CASE("pgw: subnormal inverse hazard and overflowed log power are rescued") {
  const double c=std::numeric_limits<double>::denorm_min();
  const double lp=std::log(c)+std::log(1000.0);
  // log(H)=lp and H/c approximately 1000; retaining log(H/c) avoids
  // quantizing a subnormal H before division. Residual log rounding is
  // amplified by the final exponential, hence the quantile contract margin.
  CHECK(cppunivariate::distributions::qpgw(lp,1,2,c,true,true)==doctest::Approx(std::exp(500.0)).epsilon(1e-9));
  const double x=std::exp(10.0);
  CHECK(cppunivariate::distributions::ppgw(x,1,1e308,1e-308,false,true)==doctest::Approx(-std::expm1(10.0)).epsilon(2e-12));
  CHECK(cppunivariate::distributions::dpgw(x,1,1e308,1e-308,true)==doctest::Approx(-std::expm1(10.0)).epsilon(2e-12));
  CHECK(cppunivariate::distributions::qpgw(-std::expm1(10.0),1,1e308,1e-308,false,true)==doctest::Approx(x).epsilon(2e-12));
}

TEST_CASE("pgw: rescaling subnormal powers preserves natural-tail accuracy") {
  // Independent limiting forms: F ~ c*x^2 (relative remainder O(1e-24))
  // and S=exp(1-x^(b*c)) when b*log(x) is enormous. The latter's omitted
  // softplus correction is smaller than exp(-1e308).
  CHECK(std::abs(cppunivariate::distributions::ppgw(1e-160,1,2,1e308,true,false)/
      ((1e308*1e-160)*1e-160)-1)<1e-10);
  const double k=1e308*1e-308;
  CHECK(std::abs(cppunivariate::distributions::ppgw(700,1,1e308,1e-308,false,false)/
      std::exp(1-std::pow(700.0,k))-1)<1e-10);
}

TEST_CASE("pgw: binary scaling resolves subnormal inverse ratios near scale") {
  // 1000-digit exact-binary64 references retained in edge-references.json.
  const double c=std::numeric_limits<double>::denorm_min();
  CHECK(cppunivariate::distributions::qpgw(-744.8065848419628,1,1e-4,c,true,true)==
      doctest::Approx(1.0000000013261663).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qpgw(-737.5323166423991,1,2,c,true,true)==
      doctest::Approx(1.4035922178637614e217).epsilon(1e-11));
}

TEST_CASE("pgw: tiny natural upper tails survive extreme rescaling") {
  // Independent 1000-digit references: check-ranges.py and check-ranges.json
  // in dev/research/evaluations/2026-11-12-pgw-dpqr-data/.
  struct Point { double x, scale, shape, exponent, upper; };
  const std::vector<Point> points{
    {3e-300, 1e-300, 1.0, 4.7256055559161645, 2.6801379583380074e-304},
    {6.551080335043532e-308, 1.0, 1.0, 9.999999999999807e+307, 2.680137958338261e-304},
    {9.999999999999998e+199, 1e+200, 3e+18, 1.8188645488006158e+222, 2.6801379583375254e-304},
    {3e+300, 1e+300, 5.963050297740191, 1.0, 9.859676543765704e-305}
  };
  for (const auto& point:points) {
    INFO("scale="<<point.scale<<" shape="<<point.shape<<" exponent="<<point.exponent);
    const double actual=cppunivariate::distributions::ppgw(point.x,point.scale,point.shape,point.exponent,false,false);
    CHECK(std::abs(actual/point.upper-1)<=1e-10);
  }
}

TEST_CASE("pgw: Weibull inverses retain the cancellation guard") {
  CHECK(cppunivariate::distributions::qpgw(-1,2,1e-308,1,false,true)==2);
  // The binary64 value exp(-1) is not the exact e^-1. At this small
  // shape the inverse cannot silently replace that displacement by zero.
  CHECK(std::isnan(cppunivariate::distributions::qpgw(std::exp(-1.),1,1e-12,1,false,false)));
}
