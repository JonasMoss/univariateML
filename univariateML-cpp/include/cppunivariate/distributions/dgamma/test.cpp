#include "test_support.hpp"
#include "cppunivariate/distributions/dgamma/mle.hpp"
#include "cppunivariate/distributions/dgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
TEST_CASE("dgamma: golden and independent score") {
  const auto g=mltest::load_distribution_golden("dgamma");
  const auto r=cppunivariate::dgamma_mle({g.data.data(),g.data.size()});
  REQUIRE(r.status==cppunivariate::DgammaStatus::Ok);
  CHECK(r.shape==doctest::Approx(g.estimates.at("shape")).epsilon(2e-6));
  CHECK(r.rate==doctest::Approx(g.estimates.at("rate")).epsilon(2e-6));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-9));
  const double h=1e-5;
  double ga=0,gb=0;
  for (double x:g.data) {
    ga+=(cppunivariate::distributions::ddgamma(x,r.shape+h,r.rate,true)-cppunivariate::distributions::ddgamma(x,r.shape-h,r.rate,true))/(2*h);
    gb+=(cppunivariate::distributions::ddgamma(x,r.shape,r.rate+h,true)-cppunivariate::distributions::ddgamma(x,r.shape,r.rate-h,true))/(2*h);
  }
  CHECK(std::hypot(ga,gb)<1e-3);
}
TEST_CASE("dgamma: statuses, held subsets and tails") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> constant{3,3,3}, adjacent{3,3,4}, ordinary{0,1,2,4,8}, invalid{0,1.5};
  CHECK(cppunivariate::dgamma_mle({constant.data(),constant.size()}).status==cppunivariate::DgammaStatus::PointMass);
  CHECK(cppunivariate::dgamma_mle({adjacent.data(),adjacent.size()}).status==cppunivariate::DgammaStatus::TwoPointLimit);
  CHECK(cppunivariate::dgamma_mle({invalid.data(),invalid.size()}).status==cppunivariate::DgammaStatus::InvalidData);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},-1).status==cppunivariate::DgammaStatus::InvalidControl);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},1e-8,400,1,nan).status==cppunivariate::DgammaStatus::Ok);
  CHECK(cppunivariate::dgamma_mle({ordinary.data(),ordinary.size()},1e-8,400,nan,1).status==cppunivariate::DgammaStatus::Ok);
  const auto fixed=cppunivariate::dgamma_mle({constant.data(),constant.size()},1e-8,400,2,1);
  CHECK(fixed.status==cppunivariate::DgammaStatus::Ok);
  CHECK(fixed.iterations==0);
  CHECK(std::isfinite(cppunivariate::distributions::ddgamma(1000,2,1,true)));
  CHECK(cppunivariate::distributions::pdgamma(1.5,2,1)==cppunivariate::distributions::pdgamma(1,2,1));
  cppunivariate::distributions::StdRng<> rng(19);
  const double z=cppunivariate::distributions::rdgamma(rng,2,1);
  CHECK(z==std::floor(z));
}
TEST_CASE("dgamma: held boundaries, profile score and quantiles") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> zero{0,0},x{0,0,1,3,9};
  CHECK(cppunivariate::dgamma_mle({zero.data(),zero.size()},1e-8,400,2,nan).status==cppunivariate::DgammaStatus::PointMass);
  CHECK(cppunivariate::dgamma_mle({zero.data(),zero.size()},1e-8,400,nan,2).status==cppunivariate::DgammaStatus::PointMass);
  const auto r=cppunivariate::dgamma_mle({x.data(),x.size()},1e-8,400,2,nan);
  REQUIRE(r.status==cppunivariate::DgammaStatus::Ok);
  cppunivariate::dnorm_detail::Cells cells; REQUIRE(cells.init({x.data(),x.size()}));
  double derivative=0;
  CHECK(std::abs(cppunivariate::dgamma_detail::rate_score(cells,2,std::log(r.rate),derivative))<1e-9);
  CHECK(derivative<0);
  CHECK(std::isfinite(cppunivariate::distributions::ddgamma(0,1000,1e-300,true)));
  CHECK(cppunivariate::distributions::ddgamma(0,1000,1e-300,true)==doctest::Approx(1000*std::log(1e-300)-std::lgamma(1001.0)).epsilon(1e-12));
  for (double p:std::vector<double>{1e-12,0.1,0.5,0.9}) {
    const double q=cppunivariate::distributions::qdgamma(p,2,1);
    CHECK(cppunivariate::distributions::pdgamma(q,2,1)>=p-1e-15);
    CHECK(cppunivariate::distributions::pdgamma(q-1,2,1)<p);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("dgamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddgamma(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdgamma(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdgamma(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdgamma(rng, p[0], p[1]); });
}

TEST_CASE("dgamma: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // a=mpf(2.0); r=mpf(1.0)
  // d=lambda k:gammainc(a,r*k,r*(k+1))/gamma(a); S=lambda k:gammainc(a,r*(k+1),inf)/gamma(a)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(118.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::ddgamma(1e+100, 2.0, 1.0, true) == doctest::Approx(-1e+100).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pdgamma(118.0, 2.0, 1.0, false, true) == doctest::Approx(-114.21250825721795).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qdgamma(-10000, 2.0, 1.0, false, true) == 10009.0);
  CHECK(cppunivariate::distributions::ddgamma(0.5, 2.0, 1.0, false) == 0);
}

TEST_CASE("dgamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("dgamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::dgamma_mle(x,1e-8,1).status==cppunivariate::DgammaStatus::IterationFailure);
}

// mpmath 1.3.0, 60 decimal digits: log(gammainc(a,x,inf,regularized=True)).
TEST_CASE("dgamma: certified deep upper tail at large shape") {
  const double logq=cppunivariate::special::fs_gamma_p(1040000,1000000,false,true);
  CHECK(logq==doctest::Approx(-783.89531375263302684).epsilon(1e-10));
}

#ifdef BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS
TEST_CASE("dgamma: uncertified extreme upper tail reports failure") {
  CHECK(std::isnan(cppunivariate::special::fs_gamma_p(1e14+4e8,1e14,false,true)));
}
#endif

TEST_CASE("dgamma: shape score intervals include profiled differences") {
  const std::vector<double> x{0,0,0,1,1,2,3,5,9};
  cppunivariate::dnorm_detail::Cells cells;
  REQUIRE(cells.init({x.data(),x.size()}));
  for (double a:std::vector<double>{0.2,1.0,5.0,20.0}) {
    for (bool held:std::vector<bool>{false,true}) {
      double lo=0,hi=0;
      REQUIRE(cppunivariate::dgamma_detail::shape_score_bounds(cells,a*0.99,a*1.01,held,0.8,lo,hi));
      const cppunivariate::dgamma_detail::Cost cost{cells,held,0.8,400};
      const double t=std::log(a),h=1e-5;
      const double slope=-(cost(std::vector<double>{t+h})-cost(std::vector<double>{t-h}))/(2*h);
      CHECK(slope>=lo-1e-6);
      CHECK(slope<=hi+1e-6);
    }
  }
}

TEST_CASE("dgamma: exhausted certificate is a numerical failure") {
  const std::vector<double> x{0,0,1,2,4,8};
  CHECK(cppunivariate::dgamma_mle({x.data(),x.size()},1e-8,1).status==cppunivariate::DgammaStatus::IterationFailure);
}

TEST_CASE("dgamma: structured stop payloads") {
  const std::vector<double> x{1,1};
  const auto r=cppunivariate::dgamma_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_model==cppunivariate::StopModel::PointMass);
  CHECK(r.stop_detail.limit_coef[0]==1);
  CHECK(r.stop_detail.limit_loglik==0);
  CHECK(r.stop_detail.candidates.size()==1);
}

TEST_CASE("dgamma: translated adjacent limit retains original support") {
  const std::vector<double> x{2,2,3};
  const auto r=cppunivariate::dgamma_mle({x.data(),x.size()});
  REQUIRE(r.status==cppunivariate::DgammaStatus::TwoPointLimit);
  REQUIRE(r.stop_detail.limit_coef.size()==3);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_coef[1]==3);
  CHECK(r.stop_detail.limit_coef[2]==doctest::Approx(1./3));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(2*std::log(2./3)+std::log(1./3)));
}

#include "cppunivariate/distributions/dgamma/dpqr.hpp"
TEST_CASE("dgamma: bounded seed corrections require inverse inequalities") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qdgamma(.5,2,1e-300),.5,true,
      [](double x) {return cppunivariate::distributions::pdgamma(x,2,1e-300,true,false);}));
  CHECK(cppunivariate::distributions::qdgamma(.5,1,1)==0);
}

TEST_CASE("dgamma quantiles above 2^53 are minimal representable doubles") {
  for (bool lower : {false,true}) for (bool log : {false,true}) {
    const double p=log ? std::log(.75) : .75;
    const double h=cppunivariate::distributions::qdgamma(p,2,1e-20,lower,log);
    REQUIRE(std::isfinite(h));
    REQUIRE(std::abs(h)>9007199254740992.0);
    const double at=cppunivariate::distributions::pdgamma(h,2,1e-20,lower,log);
    const double before=cppunivariate::distributions::pdgamma(std::nextafter(h,-INFINITY),2,1e-20,lower,log);
    CHECK((lower ? at>=p && before<p : at<=p && before>p));
  }
}

TEST_CASE("dgamma: rounded seeds cannot bypass inversion at 2^53") {
  for (double k : {9007199254740990.0,9007199254740992.0,9007199254740994.0}) {
    for (bool lower : {false,true}) for (bool log : {false,true}) {
      const double at=cppunivariate::distributions::pdgamma(k,2,1e-16,lower,log);
      const double p=std::nextafter(at,lower ? INFINITY : -INFINITY);
      const double edge=cppunivariate::distributions::pdgamma(9007199254740992.0,2,1e-16,lower,log);
      if (lower ? edge>=p : edge<=p) continue;
      const double h=cppunivariate::distributions::qdgamma(p,2,1e-16,lower,log);
      REQUIRE(std::isfinite(h));
      CHECK(mltest::truth_lattice_quantile(h,p,lower,[&](double x) {
        return cppunivariate::distributions::pdgamma(x,2,1e-16,lower,log);
      }));
    }
  }
}

TEST_CASE("dgamma: extreme lattice scaling has no translated subtraction") {
  // Shape one is exponential; multiplication by a reciprocal scale stays finite.
  const double t=1e308*1e-308;
  CHECK(cppunivariate::distributions::pdgamma(1e308,1,1e-308)
        ==doctest::Approx(-std::expm1(-t)).epsilon(2e-14));
  CHECK(cppunivariate::distributions::pdgamma(1e308,1,1e-308,false,true)
        ==doctest::Approx(-t).epsilon(2e-14));
  // A genuinely overflowing positive gamma argument is an endpoint, not cancellation.
  CHECK(cppunivariate::distributions::pdgamma(1e308,1,2)==1);
  CHECK(cppunivariate::distributions::pdgamma(1e308,1,2,false)==0);
}
