#include "test_support.hpp"
#include "cppunivariate/distributions/dlaplace/mle.hpp"
#include <vector>
TEST_CASE("dlaplace: closed form and fixed subsets") {
  std::vector<int>x={-2,0,1,4};
  auto f=cppunivariate::dlaplace_mle({x.data(),x.size()});
  CHECK(f.status==cppunivariate::DlaplaceStatus::Ok);
  CHECK(f.location==0);CHECK(f.not_unique);
  CHECK(f.scale==doctest::Approx(1.75/(std::hypot(1.,1.75)+1)));
  CHECK(cppunivariate::dlaplace_mle({x.data(),x.size()},.5).status==cppunivariate::DlaplaceStatus::InvalidInput);
  std::vector<int>z={2,2};
  CHECK(cppunivariate::dlaplace_mle({z.data(),z.size()}).status==cppunivariate::DlaplaceStatus::PointMass);
  CHECK(cppunivariate::dlaplace_mle({z.data(),z.size()},0).status==cppunivariate::DlaplaceStatus::Ok);
}

#include "cppunivariate/distributions/dlaplace/dpqr.hpp"
TEST_CASE("dlaplace: density normalization and log-tail quantiles") {
  CHECK(std::isnan(cppunivariate::distributions::ddlaplace(std::numeric_limits<double>::quiet_NaN(),0,.4)));
  double total=0;
  for(int x=-200;x<=200;++x)total+=cppunivariate::distributions::ddlaplace(x,0,.4);
  CHECK(total==doctest::Approx(1).epsilon(1e-12));
  for(double lp : {-1.,-10.,-100.}) {
    const double q=cppunivariate::distributions::qdlaplace(lp,0,.4,false,true);
    CHECK(cppunivariate::distributions::pdlaplace(q,0,.4,false,true)<=lp);
    CHECK(cppunivariate::distributions::pdlaplace(q-1,0,.4,false,true)>lp);
  }
}

TEST_CASE("dlaplace: golden regression") {
  auto g=mltest::load_distribution_golden("dlaplace");std::vector<int>x;
  for(double v:g.data)x.push_back(static_cast<int>(v));
  auto f=cppunivariate::dlaplace_mle({x.data(),x.size()});
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(f.scale==doctest::Approx(g.estimates.at("scale")).epsilon(1e-10));
}
