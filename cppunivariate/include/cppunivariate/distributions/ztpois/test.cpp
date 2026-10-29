#include "test_support.hpp"
#include "cppunivariate/distributions/ztpois/mle.hpp"
#include "cppunivariate/distributions/ztpois/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztpois: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztpois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztpois_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtpoisStatus::Ok);
 CHECK(fit.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.lambda};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztpois(v,par[0],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("ztpois: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztpois_mle({}).status==cppunivariate::ZtpoisStatus::InvalidInput);
 CHECK(cppunivariate::ztpois_mle({negative.data(),negative.size()}).status==cppunivariate::ZtpoisStatus::InvalidInput);
 const auto one=cppunivariate::ztpois_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtpoisStatus::Ok);
 CHECK(cppunivariate::ztpois_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtpoisStatus::InvalidInput);
}
TEST_CASE("ztpois: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztpois");
 CHECK(std::isfinite(cppunivariate::distributions::dztpois(10000,g.estimates.at("lambda"),true)));
 CHECK(cppunivariate::distributions::qztpois(0,g.estimates.at("lambda"))==1);
 CHECK(cppunivariate::distributions::qztpois(1,g.estimates.at("lambda"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("ztpois: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztpois");
 const double q=cppunivariate::distributions::qztpois(.37,g.estimates.at("lambda"));
 CHECK(cppunivariate::distributions::pztpois(q,g.estimates.at("lambda"))>=.37);
 CHECK(cppunivariate::distributions::pztpois(q-1,g.estimates.at("lambda"))<.37);
 CHECK(cppunivariate::distributions::pztpois(q,g.estimates.at("lambda"),true,false)+cppunivariate::distributions::pztpois(q,g.estimates.at("lambda"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztpois(rng,g.estimates.at("lambda"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

TEST_CASE("zero families retain finite extreme log tails") {
 CHECK(std::isfinite(cppunivariate::zero_detail::cdf(0,10000,2,0,0,false,true)));
 CHECK(cppunivariate::zero_detail::cdf(1,10000,.4,0,0,false,true)==doctest::Approx(10000*std::log(.6)));
 CHECK(std::isfinite(cppunivariate::zero_detail::cdf(2,10000,2,.4,0,false,true)));
 CHECK(std::isfinite(cppunivariate::zero_detail::cdf(3,9999,10000,.4,0,false,true)));
 CHECK(std::isfinite(cppunivariate::zero_detail::cdf(4,10000,.4,0,.3,false,true)));
 CHECK(std::isfinite(cppunivariate::zero_detail::density(3,9999,10000,.4,.3,true)));
 CHECK(cppunivariate::zero_detail::quantile(2,1,0,1,0,true,false)==1);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("ztpois: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ztpois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztpois(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztpois(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztpois(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztpois(rng, p[0]); });
}

TEST_CASE("ztpois: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // l=mpf(2.0)
  // d=lambda k:exp(-l)*l**k/factorial(k)/(1-exp(-l)); S=lambda k:gammainc(k+1,0,l)/(gamma(k+1)*(1-exp(-l)))
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(111.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dztpois(1e+100, 2.0, true) == doctest::Approx(-2.2856536211884465e+102).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pztpois(111.0, 2.0, false, true) == doctest::Approx(-343.9550531475571).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qztpois(-10000, 2.0, false, true) == 1733.0);
  CHECK(cppunivariate::distributions::dztpois(0.5, 2.0, false) == 0);
}

TEST_CASE("ztpois: tiny conditional lower tail") {
  // from mpmath import *; mp.dps=100; print(log(20)-20-log(-expm1(-20)))
  CHECK(cppunivariate::distributions::pztpois(1,20.0,true,true)==doctest::Approx(-17.004267724384857).epsilon(1e-10));
}
