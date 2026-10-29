#include "test_support.hpp"
#include "cppunivariate/distributions/dnorm/mle.hpp"
#include "cppunivariate/distributions/dnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
TEST_CASE("dnorm: golden and independent score") {
  const auto g=mltest::load_distribution_golden("dnorm");
  const auto r=cppunivariate::dnorm_mle({g.data.data(),g.data.size()});
  REQUIRE(r.status==cppunivariate::DnormStatus::Ok);
  CHECK(r.mean==doctest::Approx(g.estimates.at("mean")).epsilon(2e-6));
  CHECK(r.sd==doctest::Approx(g.estimates.at("sd")).epsilon(2e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-9));
  const double h=1e-5;
  double ga=0,gb=0;
  for (double x:g.data) {
    ga+=(cppunivariate::distributions::ddnorm(x,r.mean+h,r.sd,true)-cppunivariate::distributions::ddnorm(x,r.mean-h,r.sd,true))/(2*h);
    gb+=(cppunivariate::distributions::ddnorm(x,r.mean,r.sd+h,true)-cppunivariate::distributions::ddnorm(x,r.mean,r.sd-h,true))/(2*h);
  }
  CHECK(std::hypot(ga,gb)<1e-3);
}
TEST_CASE("dnorm: statuses, held subsets and tails") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> constant{3,3,3}, adjacent{3,3,4}, ordinary{0,1,2,4,8}, invalid{0,1.5};
  CHECK(cppunivariate::dnorm_mle({constant.data(),constant.size()}).status==cppunivariate::DnormStatus::PointMass);
  CHECK(cppunivariate::dnorm_mle({adjacent.data(),adjacent.size()}).status==cppunivariate::DnormStatus::TwoPointLimit);
  CHECK(cppunivariate::dnorm_mle({invalid.data(),invalid.size()}).status==cppunivariate::DnormStatus::InvalidData);
  CHECK(cppunivariate::dnorm_mle({ordinary.data(),ordinary.size()},-1).status==cppunivariate::DnormStatus::InvalidControl);
  CHECK(cppunivariate::dnorm_mle({ordinary.data(),ordinary.size()},1e-8,400,1,nan).status==cppunivariate::DnormStatus::Ok);
  CHECK(cppunivariate::dnorm_mle({ordinary.data(),ordinary.size()},1e-8,400,nan,1).status==cppunivariate::DnormStatus::Ok);
  const auto fixed=cppunivariate::dnorm_mle({constant.data(),constant.size()},1e-8,400,2,1);
  CHECK(fixed.status==cppunivariate::DnormStatus::Ok);
  CHECK(fixed.iterations==0);
  CHECK(std::isfinite(cppunivariate::distributions::ddnorm(1000,2,1,true)));
  CHECK(cppunivariate::distributions::pdnorm(1.5,2,1)==cppunivariate::distributions::pdnorm(1,2,1));
  cppunivariate::distributions::StdRng<> rng(19);
  const double z=cppunivariate::distributions::rdnorm(rng,2,1);
  CHECK(z==std::floor(z));
}
TEST_CASE("dnorm: held mean collapse and quantile inequalities") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{3,4},constant{3,3};
  CHECK(cppunivariate::dnorm_mle({x.data(),x.size()},1e-9,200,4,nan).status==cppunivariate::DnormStatus::ScaleLimit);
  CHECK(cppunivariate::dnorm_mle({constant.data(),constant.size()},1e-9,200,2,nan).status==cppunivariate::DnormStatus::Ok);
  CHECK(cppunivariate::dnorm_mle({constant.data(),constant.size()},1e-9,200,nan,1).mean==doctest::Approx(3.5));
  CHECK(cppunivariate::dnorm_mle({x.data(),x.size()},1e-9,1).status==cppunivariate::DnormStatus::TwoPointLimit);
  CHECK(std::isfinite(cppunivariate::distributions::ddnorm(0,0,1e300,true)));
  CHECK(cppunivariate::distributions::ddnorm(1e300,0,1e100,true)==-std::numeric_limits<double>::infinity());
  for (double p:std::vector<double>{1e-12,0.1,0.5,0.9}) {
    const double q=cppunivariate::distributions::qdnorm(p,-10,2);
    CHECK(cppunivariate::distributions::pdnorm(q,-10,2)>=p-1e-15);
    CHECK(cppunivariate::distributions::pdnorm(q-1,-10,2)<p);
  }
}
TEST_CASE("dnorm: broad cells use known interval width") {
  const std::vector<double> x{-1e12,-1e12,0,1e12,2e12};
  const auto r=cppunivariate::dnorm_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::DnormStatus::Ok);
  CHECK(r.mean==doctest::Approx(2e11+0.5).epsilon(1e-12));
  CHECK(r.sd==doctest::Approx(std::sqrt(1.36e24)).epsilon(1e-12));
  double ll=0;
  for (double z:x) ll+=cppunivariate::special::dnorm(z+0.5,r.mean,r.sd,true);
  CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  const std::vector<double> small{0,1,3};
  const auto held=cppunivariate::dnorm_mle({small.data(),small.size()},1e-9,200,1e8);
  CHECK(held.status==cppunivariate::DnormStatus::Ok);
  CHECK(std::isfinite(held.logLik));
}
