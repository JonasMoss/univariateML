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
    for(double x:g.data)d+=cppunivariate::distributions::dgb2(x,p[0],p[1],p[2],true,p[3])-
      cppunivariate::distributions::dgb2(x,m[0],m[1],m[2],true,m[3]);
    CHECK(std::abs(d/2e-5)<1e-3);
  }
}
TEST_CASE("gb2: stable density and exact double Pareto kink profile") {
  CHECK(cppunivariate::distributions::dgb2(1e-300,2,5,3,true)==doctest::Approx(-9666.763).epsilon(1e-6));
  CHECK(cppunivariate::distributions::dgb2(1e300,2,5,3,true)==doctest::Approx(-7594.436).epsilon(1e-6));
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
