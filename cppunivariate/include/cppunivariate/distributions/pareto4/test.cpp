#include "test_support.hpp"
#include "cppunivariate/distributions/pareto4/dpqr.hpp"
#include "cppunivariate/distributions/pareto4/mle.hpp"
TEST_CASE("pareto4: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::pareto4_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::burr_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("pareto4: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dpareto4(1,0,2,3,1) == doctest::Approx(.75));
}
