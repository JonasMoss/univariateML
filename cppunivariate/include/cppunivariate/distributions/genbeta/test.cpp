#include "test_support.hpp"
#include "cppunivariate/distributions/genbeta/mle.hpp"
#include "cppunivariate/distributions/genbeta/dpqr.hpp"

TEST_CASE("genbeta: golden and held-scale likelihood") {
  const auto g=mltest::load_distribution_golden("genbeta");
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(g.data),1);
  REQUIRE(r.status==cppunivariate::GenbetaStatus::Ok);
  CHECK(r.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(1e-5));
  CHECK(r.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(1e-5));
  CHECK(r.shape3==doctest::Approx(g.estimates.at("shape3")).epsilon(1e-5));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-8));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dgenbeta(x,r.shape1,r.shape2,r.shape3,1,true);
  CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
  for(int j=0;j<3;++j) {
    std::array<double,3> v{{r.shape1,r.shape2,r.shape3}};
    auto value=[&](double h){auto w=v;w[j]*=std::exp(h);double ans=0;for(double x:g.data)ans+=cppunivariate::distributions::dgenbeta(x,w[0],w[1],w[2],1,true);return ans;};
    CHECK(std::abs((value(1e-5)-value(-1e-5))/2e-5)<1e-3);
  }
}
TEST_CASE("genbeta: invalid observations, controls, and attained power ridge") {
  const std::vector<double> x{.1,.2,.3,.4};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto f=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,1);
  CHECK(f.status==cppunivariate::GenbetaStatus::Ok);
  CHECK(f.shape3==1);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,1e308,1,1e308).status==cppunivariate::GenbetaStatus::SearchUnresolved);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,1e308,1e308,1).status==cppunivariate::GenbetaStatus::SearchUnresolved);
  for(double s:{0.,-.1,.4})CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),s).status!=cppunivariate::GenbetaStatus::Ok);
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(x),1,0).status==cppunivariate::GenbetaStatus::InvalidData);
  CHECK(cppunivariate::distributions::dgenbeta(1,2,1,3,1,false)==doctest::Approx(6));
  CHECK(cppunivariate::distributions::dgenbeta(0,.5,2,2,1,false)==doctest::Approx(2/std::exp(cppunivariate::special::lbeta(.5,2))));
}
TEST_CASE("genbeta: exact gamma and effective-endpoint power face comparisons") {
  std::vector<double> gamma,power;
  for(int i=0;i<300;++i) {
    const double u=(i+.5)/300;
    gamma.push_back(std::exp(-cppunivariate::special::qgamma(u,2,1.0/3,true,false)));
    power.push_back(.7*u*u);
  }
  CHECK(cppunivariate::genbeta_mle(mltest::as_span(gamma),1).status==cppunivariate::GenbetaStatus::UnitGammaBoundary);
  const auto r=cppunivariate::genbeta_mle(mltest::as_span(power),1);
  CHECK(r.status==cppunivariate::GenbetaStatus::PowerBoundary);
  const auto p=cppunivariate::power_mle(mltest::as_span(power),0);
  CHECK(r.logLik==doctest::Approx(p.logLik).epsilon(1e-10));
}

#include "cppunivariate/distributions/genbeta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("genbeta: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qgenbeta(0,20,1000,2,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qgenbeta(0,20,1000,2,1,false,false))==false);
  CHECK(cppunivariate::distributions::pgenbeta(-1,20,1000,2,1,true,false)==0);
  CHECK(cppunivariate::distributions::pgenbeta(std::numeric_limits<double>::infinity(),20,1000,2,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qgenbeta(.1,20,1000,2,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rgenbeta(rng,20,1000,2,1));
  for(double point:{.05,.1,.2}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pgenbeta(point,20,1000,2,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; x=mp.mpf('1e-100'); z=x*x
// float(mp.log(mp.betainc(20,1000,0,z,regularized=True)))
TEST_CASE("genbeta: log beta tails beyond probability underflow") {
  CHECK(cppunivariate::distributions::pgenbeta(1e-100,20,1000,2,1,true,true)==doctest::Approx(-9114.3321059628252).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qgenbeta(lp,20,1000,2,1,lower,true);
    REQUIRE(q>0);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pgenbeta(q,20,1000,2,1,lower,true)==doctest::Approx(lp).epsilon(2e-11));
  }
}

TEST_CASE("P2 genbeta constant conditional beta fits") {
 std::vector<double> x(5,.4);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 auto a=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,3,1);
 REQUIRE(a.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(a.shape2==3);
 CHECK(a.shape3==1);
 auto b=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,2,nan,1);
 REQUIRE(b.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(b.shape1==2);
 CHECK(b.shape3==1);
 auto ridge=cppunivariate::genbeta_mle(mltest::as_span(x),1,1e-7,300,nan,1,nan);
 REQUIRE(ridge.status==cppunivariate::GenbetaStatus::Ok);
 CHECK(ridge.shape1*ridge.shape3==doctest::Approx(-1/std::log(.4)));
}

TEST_CASE("genbeta: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("genbeta");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::genbeta_mle(x,1,1e-8,1).status==cppunivariate::GenbetaStatus::SearchUnresolved);
}
