#include "test_support.hpp"
#include "cppunivariate/distributions/nsbeta/dpqr.hpp"
#include "cppunivariate/distributions/nsbeta/mle.hpp"
TEST_CASE("nsbeta: exact affine likelihood") {
 const std::vector<double> y{.05,.1,.2,.4,.6,.7,.85};
 std::vector<double> x(y); for(double &v:x) v=2+3*v;
 auto a=cppunivariate::nsbeta_mle(cppunivariate::span{x.data(),x.size()},2,5,1e-10,1000);
 auto b=cppunivariate::beta_mle(cppunivariate::span{y.data(),y.size()},1e-10,1000);
 CHECK(a.logLik == doctest::Approx(b.logLik-y.size()*std::log(3)));
 CHECK(a.shape1 == doctest::Approx(b.alpha));
}

TEST_CASE("nsbeta: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dnsbeta(.5,2,3,0,1) == doctest::Approx(1.5));
}
