#include "test_support.hpp"
#include "cppunivariate/distributions/tbinom/mle.hpp"
#include <vector>
TEST_CASE("tbinom: unique mean solve and support errors") {
  const std::vector<int> x={1,2,3,4};
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
  CHECK(std::isfinite(f.logLik));
  CHECK(cppunivariate::tbinom_mle({x.data(),x.size()},-INFINITY,INFINITY,1e308).status==cppunivariate::truncated_detail::Status::NumericalFailure);
  auto z=cppunivariate::tbinom_mle({x.data(),x.size()},2,10,12);
  CHECK(z.status==cppunivariate::truncated_detail::Status::InvalidInput);
  const std::vector<int> constant={1,1};
  CHECK(cppunivariate::tbinom_mle({constant.data(),constant.size()},0,10,12).status==cppunivariate::truncated_detail::Status::LowerPointMass);
}

#include "cppunivariate/distributions/tbinom/dpqr.hpp"
TEST_CASE("tbinom: density normalization and log-tail quantiles") {
  double total=0;
  for(int x=0;x<=200;++x)total+=cppunivariate::distributions::dtbinom(x,12,.4,0,10);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qtbinom(lp,12,.4,0,10,false,true);
    CHECK(cppunivariate::distributions::ptbinom(q,12,.4,0,10,false,true)<=lp);
    CHECK(cppunivariate::distributions::ptbinom(q-1,12,.4,0,10,false,true)>lp);
  }
}

TEST_CASE("tbinom: golden regression") {
  auto g=mltest::load_distribution_golden("tbinom");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::tbinom_mle({x.data(),x.size()},0,10,12);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(f.parameter==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
}

TEST_CASE("tbinom: free bounds and fixed-probability integer tail") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4},constant{3,3};
 const auto f=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,8);
 CHECK(f.a==0);CHECK(f.b==4);CHECK(f.size==8);
 CHECK(f.status==cppunivariate::truncated_detail::Status::Ok);
 const auto h=cppunivariate::tbinom_design_mle({x.data(),x.size()},nan,nan,nan,.4);
 REQUIRE(h.status==cppunivariate::truncated_detail::Status::Ok);
 for(int k=4;k<100;++k)CHECK(h.logLik>=cppunivariate::tbinom_mle({x.data(),x.size()},0,4,k,.4).logLik-1e-9);
 const auto c=cppunivariate::tbinom_design_mle({constant.data(),constant.size()},nan,nan,8);
 CHECK(c.prob==.5);CHECK(c.logLik==0);
 CHECK(cppunivariate::tbinom_design_mle({constant.data(),constant.size()},-1,3,nan,.4).status==cppunivariate::truncated_detail::Status::UpperPointMass);
}
