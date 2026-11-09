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
  CHECK(r.mean==doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kGolden_dnorm).scale(0));
  CHECK(r.sd==doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kGolden_dnorm).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_dnorm).scale(0));
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

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("dnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdnorm(rng, p[0], p[1]); });
}

TEST_CASE("dnorm: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // m=mpf(0.0); s=mpf(1.0)
  // d=lambda k:(erfc((k-m)/(s*sqrt(2)))-erfc((k+1-m)/(s*sqrt(2))))/2; S=lambda k:erfc((k+1-m)/(s*sqrt(2)))/2
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(54.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::ddnorm(1e+100, 0.0, 1.0, true) == doctest::Approx(-5e+199).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pdnorm(54.0, 0.0, 1.0, false, true) == doctest::Approx(-1517.4266020241887).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qdnorm(-10000, 0.0, 1.0, false, true) == 141.0);
  CHECK(cppunivariate::distributions::ddnorm(0.5, 0.0, 1.0, false) == 0);
}

TEST_CASE("dnorm: exact and rounded half probabilities") {
  // mp.dps=100; print(erfc(0)/2); print(exp(mpf(-0.6931471805599453))-mpf('.5'))
  CHECK(cppunivariate::distributions::qdnorm(0.5,0,1,true,false)==-1);
  CHECK(cppunivariate::distributions::qdnorm(-0.6931471805599453,0,1,true,true)==0);
}

TEST_CASE("dnorm: exact half and rounded log half straddle the CDF jump") {
  // At mean=0, sd=1, P(X<=-1) is exactly 1/2. The binary64
  // logarithm -0x1.62e42fefa39efp-1 exceeds exact log(1/2).
  const double lh=-0x1.62e42fefa39efp-1;
  CHECK(cppunivariate::distributions::pdnorm(-1,0,1)==.5);
  CHECK(cppunivariate::distributions::qdnorm(.5,0,1)==-1);
  CHECK(cppunivariate::distributions::qdnorm(lh,0,1,true,true)==0);
  CHECK(cppunivariate::distributions::qdnorm(std::nextafter(lh,-INFINITY),0,1,true,true)==-1);
  CHECK(cppunivariate::distributions::qdnorm(lh,0,1,false,true)==-1);
  CHECK(cppunivariate::distributions::qdnorm(std::nextafter(lh,-INFINITY),0,1,false,true)==0);
}

TEST_CASE("dnorm: structured stop payloads") {
  const std::vector<double> x{1,1};
  const auto r=cppunivariate::dnorm_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

TEST_CASE("dnorm: translated adjacent limit retains original support") {
  const std::vector<double> x{2,2,3};
  const auto r=cppunivariate::dnorm_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::DnormStatus::TwoPointLimit);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_coef[1]==3);
  CHECK(r.stop_detail.limit_coef[2]==doctest::Approx(1./3));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(2*std::log(2./3)+std::log(1./3)));
}

TEST_CASE("dnorm: held mean zero-scale face uses discretization cells") {
  const std::vector<double> x{2,2};
  const auto interior=cppunivariate::dnorm_mle({x.data(),x.size()},1e-9,200,2.5);
  REQUIRE(interior.status==cppunivariate::DnormStatus::ScaleLimit);
  REQUIRE(interior.stop_detail.limit_coef.size()==1);
  CHECK(interior.stop_detail.limit_coef[0]==2);
  CHECK(interior.stop_detail.limit_loglik==0);
  const auto seam=cppunivariate::dnorm_mle({x.data(),x.size()},1e-9,200,2);
  REQUIRE(seam.stop_detail.limit_coef.size()==3);
  CHECK(seam.stop_detail.limit_coef[0]==1);
  CHECK(seam.stop_detail.limit_coef[1]==2);
  CHECK(seam.stop_detail.limit_coef[2]==.5);
  CHECK(seam.stop_detail.limit_loglik==doctest::Approx(-2*std::log(2.)));
}

#include "cppunivariate/distributions/dnorm/dpqr.hpp"
TEST_CASE("dnorm: bounded seed corrections require a representable jump") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qdnorm(.5,1e308,1),.5,true,
      [](double x) {return cppunivariate::distributions::pdnorm(x,1e308,1,true,false);}));
  CHECK(cppunivariate::distributions::qdnorm(.5,0,1)==-1);
}

TEST_CASE("dnorm quantiles above 2^53 are minimal representable doubles") {
  for (bool lower : {false,true}) for (bool log : {false,true}) {
    const double p=log ? std::log(.75) : .75;
    const double h=cppunivariate::distributions::qdnorm(p,0,1e20,lower,log);
    REQUIRE(std::isfinite(h));
    REQUIRE(std::abs(h)>9007199254740992.0);
    const double at=cppunivariate::distributions::pdnorm(h,0,1e20,lower,log);
    const double before=cppunivariate::distributions::pdnorm(std::nextafter(h,-INFINITY),0,1e20,lower,log);
    CHECK((lower ? at>=p && before<p : at<=p && before>p));
  }
}

TEST_CASE("dnorm: rounded seeds cannot bypass inversion at 2^53") {
  for (double k : {9007199254740990.0,9007199254740992.0,9007199254740994.0}) {
    for (bool lower : {false,true}) for (bool log : {false,true}) {
      const double at=cppunivariate::distributions::pdnorm(k,0,1e20,lower,log);
      const double p=std::nextafter(at,lower ? INFINITY : -INFINITY);
      const double edge=cppunivariate::distributions::pdnorm(9007199254740992.0,0,1e20,lower,log);
      if (lower ? edge>=p : edge<=p) continue;
      const double h=cppunivariate::distributions::qdnorm(p,0,1e20,lower,log);
      REQUIRE(std::isfinite(h));
      CHECK(mltest::truth_lattice_quantile(h,p,lower,[&](double x) {
        return cppunivariate::distributions::pdnorm(x,0,1e20,lower,log);
      }));
    }
  }
}

TEST_CASE("dnorm: overflowing translated lattice differences retain finite standardized points") {
  // Independent 100-digit references: 2026-11-08-pdnorm-fix-data/check.py.
  for (double sign : {-1.0,1.0}) {
    const double x=sign*1e308, mean=-x, sd=1e308;
    for (bool lower : {false,true}) for (bool log : {false,true}) {
      const double tail=(sign<0)==lower ? 0.0227501319481792072 : 0.977249868051820793;
      CHECK(cppunivariate::distributions::pdnorm(x,mean,sd,lower,log)
            ==doctest::Approx(log ? std::log(tail) : tail).epsilon(2e-14));
    }
    CHECK(cppunivariate::distributions::ddnorm(x,mean,sd,true)
          ==doctest::Approx(-712.11514717537074343).epsilon(2e-15));
    CHECK(cppunivariate::distributions::ddnorm(x,mean,sd)
          /std::exp(-712.11514717537074343)==doctest::Approx(1).epsilon(2e-13));
    CHECK(cppunivariate::distributions::pdnorm(x,mean,1)==(sign<0 ? 0 : 1));
  }
  CHECK(cppunivariate::distributions::pdnorm(-7.976931348623157e307,1e308,1e308)
        ==doctest::Approx(0.03611282497155248489).epsilon(2e-14));
  const double largest=std::numeric_limits<double>::max();
  CHECK(cppunivariate::distributions::pdnorm(-largest,largest,largest)
        ==doctest::Approx(0.0227501319481792072).epsilon(2e-14));
  CHECK(cppunivariate::distributions::pdnorm(-INFINITY,1e308,1e308)==0);
  CHECK(cppunivariate::distributions::pdnorm(INFINITY,-1e308,1e308)==1);
  for (double mean : {-1e308,1e308}) for (bool lower : {false,true}) for (bool log : {false,true}) {
    const double p=log ? std::log(.01) : .01;
    const double h=cppunivariate::distributions::qdnorm(p,mean,1e308,lower,log);
    // Some reflected targets lie beyond the finite binary64 range.
    if ((mean>0)==lower) {
      REQUIRE(std::isfinite(h));
      CHECK(mltest::truth_lattice_quantile(h,p,lower,[&](double x) {
        return cppunivariate::distributions::pdnorm(x,mean,1e308,lower,log);
      }));
      const double z=h/1e308-mean/1e308;
      CHECK(std::erfc((lower ? -z : z)/std::sqrt(2.0))/2
            ==doctest::Approx(.01).epsilon(2e-13));
    }
  }
}
