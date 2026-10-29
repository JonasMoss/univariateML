#include "test_support.hpp"
#include "cppunivariate/distributions/bhatt/mle.hpp"
#include <vector>
TEST_CASE("bhatt: golden and likelihood") {
  const auto g=mltest::load_distribution_golden("bhatt");
  const auto f=cppunivariate::bhatt_mle(mltest::as_span(g.data));
  REQUIRE(f.status==cppunivariate::BhattStatus::Ok);
  CHECK(f.logLik==doctest::Approx(g.logLik).epsilon(1e-7));
  double ll=0;for(double x:g.data)ll+=cppunivariate::distributions::dbhatt(x,f.mu,f.sigma,f.a,true);
  CHECK(ll==doctest::Approx(f.logLik).epsilon(1e-12));
  std::vector<double> theta={f.mu,f.sigma,f.a};
  double norm2=0;
  for(std::size_t j=0;j<theta.size();++j) {
    const double h=1e-5*(1+std::abs(theta[j]));auto plus=theta,minus=theta;
    plus[j]+=h;minus[j]-=h;double score=0;
    for(double x:g.data)score+=(cppunivariate::distributions::dbhatt(x,plus[0],plus[1],plus[2],true)-cppunivariate::distributions::dbhatt(x,minus[0],minus[1],minus[2],true))/(2*h);
    norm2+=score*score;
  }
  CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("bhatt: direct log tails and quantiles") {
  for(bool lower:{false,true})for(double lp:{-1e-6,-.6931471805599453,-10.,-100.,-600.}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,2,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,2,lower,true)==doctest::Approx(lp).epsilon(1e-10));
  }
  for(double a:{0.,2.})for(bool lower:{false,true})for(double lp:{-1e-20,-1e-100}) {
    const double q=cppunivariate::distributions::qbhatt(lp,0,1,a,lower,true);
    CHECK(cppunivariate::distributions::pbhatt(q,0,1,a,lower,true)/lp==doctest::Approx(1).epsilon(1e-9));
  }
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(0,0,1,2)));
  CHECK(std::isinf(cppunivariate::distributions::qbhatt(1,0,1,2)));
  CHECK(std::isnan(cppunivariate::distributions::dbhatt(0,0,-1,1)));
  CHECK(cppunivariate::distributions::dbhatt(0,0,1,0)==doctest::Approx(.39894228040143267794));
  CHECK(cppunivariate::distributions::pbhatt(0,0,1,0)==doctest::Approx(.5));
  CHECK(cppunivariate::distributions::qbhatt(.5,7,3,0)==doctest::Approx(7));
}
TEST_CASE("bhatt: faces, held subsets and invalid inputs") {
  const std::vector<double> uniform={-1,-.5,0,.5,1},constant={2,2,2};
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform)).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(constant)).status==cppunivariate::BhattStatus::Unbounded);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,1,2).status==cppunivariate::BhattStatus::Ok);
  CHECK(cppunivariate::bhatt_mle(mltest::as_span(uniform),1e-8,500,cppunivariate::special::kNaN,2).status==cppunivariate::BhattStatus::UniformLimit);
  CHECK(cppunivariate::bhatt_mle({}).status==cppunivariate::BhattStatus::InvalidInput);
}
