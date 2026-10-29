#include "test_support.hpp"
#include "cppunivariate/distributions/frechet/dpqr.hpp"
#include "cppunivariate/distributions/frechet/mle.hpp"
TEST_CASE("frechet: held location and free location restrictions") {
 std::vector<double> x{.2,.4,.7,1,2,4,10};
 auto r=cppunivariate::frechet_mle(cppunivariate::span{x.data(),x.size()},1e-10,100,0);
 auto b=cppunivariate::invweibull_mle(cppunivariate::span{x.data(),x.size()},1e-10,100);
 CHECK(r.lambda == doctest::Approx(b.shape));
 CHECK(r.sigma == doctest::Approx(1/b.rate));
 CHECK(r.logLik == doctest::Approx(b.logLik));
 auto f=cppunivariate::frechet_mle(cppunivariate::span{x.data(),x.size()});
 CHECK(f.status == cppunivariate::FrechetStatus::Ok);
 CHECK(f.lambda>=1);
 std::vector<double> ties{1,1,1,2};
 CHECK(cppunivariate::frechet_mle(cppunivariate::span{ties.data(),ties.size()}).status == cppunivariate::FrechetStatus::InvalidData);
}

TEST_CASE("frechet: scalar density parameter mapping") {
 CHECK(cppunivariate::distributions::dfrechet(1,2,0,1) == doctest::Approx(2/std::exp(1.)));
}

TEST_CASE("frechet: ties above the minimum are admissible") {
  const std::vector<double> x{0,1,2,2,2};
  const auto r=cppunivariate::frechet_gev_mle(mltest::as_span(x),1e-10,100,.3);
  CHECK(r.status==cppunivariate::GevStatus::Ok);
}
