#include "test_support.hpp"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include "cppunivariate/distributions/hcauchy/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("hcauchy: golden and exact pair") {
  const auto g=mltest::load_distribution_golden("hcauchy");
  const auto r=cppunivariate::hcauchy_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::HcauchyStatus::Ok);
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  for(double factor:{1e-200,1.0,1e200}) {
    const std::vector<double> x={factor,4*factor};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).sigma/factor==doctest::Approx(2).epsilon(1e-11));
  }
}
TEST_CASE("hcauchy: zero multiplicities and invalid data") {
  const std::vector<double> a={0,1,2}, b={0,1}, c={0,0,1}, d={0}, e={-1,1}, f={};
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(a)).status==cppunivariate::HcauchyStatus::Ok);
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(b)).status==cppunivariate::HcauchyStatus::ScaleBoundary);
  for(const auto &x:{c,d}) CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::Unbounded);
  for(const auto &x:{e,f}) CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::InvalidData);
}
TEST_CASE("hcauchy: reference identities, tails and sampling") {
  CHECK(cppunivariate::distributions::dhcauchy(0,2,false)==doctest::Approx(1/std::acos(-1.0)));
  CHECK(std::isfinite(cppunivariate::distributions::dhcauchy(1e200,1,true)));
  CHECK(cppunivariate::distributions::phcauchy(-1,2,true,false)==0);
  for(bool lower:{false,true}) for(bool logs:{false,true}) for(double p:{0.,.01,.5,.99,1.}) {
    const double q=cppunivariate::distributions::qhcauchy(logs?std::log(p):p,2,lower,logs);
    CHECK(cppunivariate::distributions::phcauchy(q,2,lower,false)==doctest::Approx(p).epsilon(1e-11));
  }
  cppunivariate::distributions::StdRng<> rng(23);
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rhcauchy(rng,2)>=0);
}
