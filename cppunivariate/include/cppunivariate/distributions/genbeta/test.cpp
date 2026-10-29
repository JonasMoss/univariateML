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
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dgenbeta(x,r.shape1,r.shape2,r.shape3,true,1);
  CHECK(ll==doctest::Approx(r.logLik).epsilon(1e-10));
  for(int j=0;j<3;++j) {
    std::array<double,3> v{{r.shape1,r.shape2,r.shape3}};
    auto value=[&](double h){auto w=v;w[j]*=std::exp(h);double ans=0;for(double x:g.data)ans+=cppunivariate::distributions::dgenbeta(x,w[0],w[1],w[2],true,1);return ans;};
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
  CHECK(cppunivariate::distributions::dgenbeta(1,2,1,3)==doctest::Approx(6));
  CHECK(cppunivariate::distributions::dgenbeta(0,.5,2,2)==doctest::Approx(2/std::exp(cppunivariate::special::lbeta(.5,2))));
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
