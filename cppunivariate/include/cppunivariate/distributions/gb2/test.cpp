#include "test_support.hpp"
#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include <vector>
TEST_CASE("gb2: golden and full score gate") {
  const auto g=mltest::load_distribution_golden("gb2");
  const auto r=cppunivariate::gb2_mle(mltest::as_span(g.data));
  REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
  std::vector<double> budget_data;
  for(int i=0;i<200;++i) {
    const double u=cppunivariate::special::beta_quantile((i+.5)/200,.4,.3,true);
    budget_data.push_back(std::sqrt(u/(1-u)));
  }
  CHECK(cppunivariate::gb2_mle(mltest::as_span(budget_data),1e-7,1).status==cppunivariate::Gb2Status::SearchUnresolved);
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-9));
  CHECK(r.shape1==doctest::Approx(g.estimates.at("shape1")).epsilon(1e-5));
  CHECK(r.shape2==doctest::Approx(g.estimates.at("shape2")).epsilon(1e-5));
  CHECK(r.shape3==doctest::Approx(g.estimates.at("shape3")).epsilon(1e-5));
  CHECK(r.scale==doctest::Approx(g.estimates.at("scale")).epsilon(1e-5));
  std::vector<double> v{r.shape1,r.shape2,r.shape3,r.scale};
  for(int j=0;j<4;++j) {
    auto p=v,m=v;p[j]*=std::exp(1e-5);m[j]*=std::exp(-1e-5);
    double d=0;
    for(double x:g.data)d+=cppunivariate::distributions::dgb2(x,p[0],p[1],p[2],p[3],true)-
      cppunivariate::distributions::dgb2(x,m[0],m[1],m[2],m[3],true);
    CHECK(std::abs(d/2e-5)<1e-3);
  }
}
TEST_CASE("gb2: stable density and exact double Pareto kink profile") {
  CHECK(cppunivariate::distributions::dgb2(1e-300,2,5,3,1,true)==doctest::Approx(-9666.763).epsilon(1e-6));
  CHECK(cppunivariate::distributions::dgb2(1e300,2,5,3,1,true)==doctest::Approx(-7594.436).epsilon(1e-6));
  std::vector<double> logs{-2,-.8,-.2,0,.1,.7,1.5};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(logs),d));
  const double ll=cppunivariate::gb2_detail::double_pareto(d,1);
  CHECK(ll==doctest::Approx(-9.173878).epsilon(1e-6));
}
TEST_CASE("gb2: analytic envelope scores and Hessians for inner constraints") {
  const std::vector<double> y{-2,-.8,-.2,0,.1,.7,1.5};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(y),d));
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,1.7}) for(double b:{nan,1.2}) {
    const auto f=cppunivariate::gb2_detail::profile(d,.7,1.3,a,b);
    REQUIRE(f.ok);
    for(int j=0;j<2;++j) {
      const double h=1e-4;
      const auto plus=cppunivariate::gb2_detail::profile(d,.7*std::exp(j==0?h:0),1.3*std::exp(j==1?h:0),a,b);
      const auto minus=cppunivariate::gb2_detail::profile(d,.7*std::exp(j==0?-h:0),1.3*std::exp(j==1?-h:0),a,b);
      CHECK((plus.ll-minus.ll)/(2*h*d.n)==doctest::Approx(f.g[j]).epsilon(1e-5));
      for(int i=0;i<2;++i)
        CHECK((plus.g[i]-minus.g[i])/(2*h)==doctest::Approx(f.h[2*i+j]).epsilon(1e-5));
    }
  }
}

#include "cppunivariate/distributions/gb2/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("gb2: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qgb2(0,20,2,20,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qgb2(0,20,2,20,1,false,false))==true);
  CHECK(cppunivariate::distributions::pgb2(-1,20,2,20,1,true,false)==0);
  CHECK(cppunivariate::distributions::pgb2(std::numeric_limits<double>::infinity(),20,2,20,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qgb2(.1,20,2,20,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rgb2(rng,20,2,20,1));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pgb2(point,20,2,20,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; x=mp.mpf('1e-100'); z=x*x/(1+x*x)
// float(mp.log(mp.betainc(20,20,0,z,regularized=True)))
TEST_CASE("gb2: log beta tails beyond probability underflow") {
  // from mpmath import mp; mp.dps=100
  // -mp.mpf('1e20')*mp.log1p(1/mp.mpf('1e20')) == -1 to double precision
  CHECK(cppunivariate::distributions::pgb2(1e20,1,1,1e20,1,true,true)==doctest::Approx(-1).epsilon(2e-13));
  CHECK(cppunivariate::distributions::pgb2(1e-100,20,2,20,1,true,true)==doctest::Approx(-9185.384112363492).epsilon(2e-14));
  for(double lp:{-27.631021115928547,-69.07755278982137,-230.25850929940458,-690.7755278982137,-800.,-10000.}) for(bool lower:{true,false}) {
    const double q=cppunivariate::distributions::qgb2(lp,20,2,20,1,lower,true);
    REQUIRE(q>0);
    REQUIRE(std::isfinite(q));
    CHECK(cppunivariate::distributions::pgb2(q,20,2,20,1,lower,true)==doctest::Approx(lp).epsilon(2e-11));
  }
}

TEST_CASE("P2 gb2 constant conditional shape fits") {
 std::vector<double> x(5,2.0);
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(int mask=0;mask<3;++mask) {
   double q=mask==0?nan:2,a=mask==2?nan:3,p=mask==1?nan:2;
   auto r=cppunivariate::gb2_mle(mltest::as_span(x),1e-7,300,q,a,p,1);
   REQUIRE(r.status==cppunivariate::Gb2Status::Ok);
   CHECK(std::isfinite(r.logLik));
   CHECK(r.scale==1);
   if(mask!=0) CHECK(r.shape1==q);
   if(mask!=1) CHECK(r.shape3==p);
   if(mask!=2) CHECK(r.shape2==a);
 }
}

TEST_CASE("gb2: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("gb2");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::gb2_mle(x,1e-8,1).status==cppunivariate::Gb2Status::SearchUnresolved);
}
