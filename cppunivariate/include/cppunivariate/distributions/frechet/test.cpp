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
 CHECK(cppunivariate::distributions::dfrechet(1,2,0,1,false) == doctest::Approx(2/std::exp(1.)));
}

#include "cppunivariate/distributions/frechet/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
TEST_CASE("frechet: probability endpoints and sampler CDF") {
  CHECK(cppunivariate::distributions::qfrechet(0,2,0,1,true,false)==0);
  CHECK(std::isinf(cppunivariate::distributions::qfrechet(0,2,0,1,false,false))==true);
  CHECK(cppunivariate::distributions::pfrechet(-1,2,0,1,true,false)==0);
  CHECK(cppunivariate::distributions::pfrechet(std::numeric_limits<double>::infinity(),2,0,1,true,false)==1);
  CHECK(std::isnan(cppunivariate::distributions::qfrechet(.1,2,0,1,true,true)));
  cppunivariate::distributions::StdRng<> rng(7813);
  std::vector<double> samples;
  for(int i=0;i<20000;++i) samples.push_back(cppunivariate::distributions::rfrechet(rng,2,0,1));
  for(double point:{.5,1.,2.}) {
    int count=0;for(double x:samples) if(x<=point) ++count;
    const double prob=cppunivariate::distributions::pfrechet(point,2,0,1,true,false);
    CHECK(std::abs(count/20000.-prob)<=5*std::sqrt(prob*(1-prob)/20000.));
  }
}
// from mpmath import mp; mp.dps=100; float(mp.log(2)-3*mp.log(mp.mpf(".01"))-10000)
TEST_CASE("frechet: independent extreme value") {
 CHECK(cppunivariate::distributions::dfrechet(.01,2,0,1,true)==doctest::Approx(-9985.4913422614754).epsilon(2e-13));
}

TEST_CASE("frechet: ties above the minimum are admissible") {
  const std::vector<double> x{0,1,2,2,2};
  const auto r=cppunivariate::frechet_gev_mle(mltest::as_span(x),1e-10,100,.3);
  CHECK(r.status==cppunivariate::GevStatus::Ok);
}
