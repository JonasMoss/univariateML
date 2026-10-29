#include "test_support.hpp"
#include "cppunivariate/distributions/pareto2/dpqr.hpp"
#include "cppunivariate/distributions/pareto2/mle.hpp"
TEST_CASE("pareto2: exact delegated likelihood") {
 const std::vector<double> y{.01,.03,.1,.3,1,4,20,100,10000};
 std::vector<double> x(y);
 for(double &v:x) v+=2;
 auto a=cppunivariate::pareto2_mle(cppunivariate::span{x.data(),x.size()},2,1e-10,1000);
 auto b=cppunivariate::lomax_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik));
}

TEST_CASE("pareto2: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dpareto2(1,0,2,1) == doctest::Approx(.25));
}
