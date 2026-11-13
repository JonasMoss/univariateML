#include "test_support.hpp"
#include "cppunivariate/distributions/pois/ztpois_mle.hpp"
#include "cppunivariate/distributions/pois/ztpois_dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztpois: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztpois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztpois_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtpoisStatus::Ok);
 CHECK(fit.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(mltest::kGolden_ztpois).scale(0));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_ztpois).scale(0));
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

TEST_CASE("ztpois: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("ztpois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::ztpois_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("lambda"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.lambda==g.estimates.at("lambda"));
 }
}

TEST_CASE("ztpois: P2 held boundary likelihood and domain") {
 const std::vector<int> x{1,2},same{1,1};
 const auto incompatible=cppunivariate::ztpois_mle({x.data(),x.size()},0);
 CHECK(incompatible.status==cppunivariate::ZtpoisStatus::InvalidInput);
 const auto valid=cppunivariate::ztpois_mle({same.data(),same.size()},0);
 CHECK(valid.lambda==0);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::ztpois_mle({x.data(),x.size()},-1);
 CHECK(invalid.status==cppunivariate::ZtpoisStatus::InvalidInput);
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

#include "cppunivariate/distributions/pois/ztpois_dpqr.hpp"
TEST_CASE("ztpois: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::distributions::qztpois(.5,1e308)));
}

TEST_CASE("ztpois: shared binomial profile avoids factorial cancellation") {
  // Independent 80-digit log-gamma formula, with exact binary64 inputs;
  // reproducible grid: 2026-11-11-ztpois-pmf-data/check.py.
  struct Reference { int x; double size, prob, logpmf; };
  const Reference refs[] = {
    {1000, 1000., .999, -1.0005003335835343},
    {999999, 1000000., .999999, -.99999949999983329},
    {500000000, 1000000000., .5, -10.587424271367933},
    {1, 1000000000., 1e-9, -.99999999949999996},
    {999999999, 1000000000., .999999999, -.9999999995000004},
    {0, 1000000000., 1e-12, -.0010000000000005001},
    {16, 1000000000., .001, -1000309.9412668268},
    {999999984, 1000000000., .9, -105360249.91302635}
  };
  for (const auto& ref : refs) {
    const double actual=cppunivariate::zero_detail::bin_logpmf(ref.x,ref.size,ref.prob);
    CHECK(std::abs(actual-ref.logpmf)<=1e-12*std::max(1.,std::abs(ref.logpmf)));
  }
  CHECK(cppunivariate::zero_detail::bin_logpmf(0,1000,0)==0);
  CHECK(cppunivariate::zero_detail::bin_logpmf(1000,1000,1)==0);
  CHECK(cppunivariate::zero_detail::bin_logpmf(1,1000,0)==-INFINITY);
  CHECK(cppunivariate::zero_detail::bin_logpmf(999,1000,1)==-INFINITY);
  CHECK(cppunivariate::zero_detail::bin_logpmf(1001,1000,.5)==-INFINITY);
}

TEST_CASE("ztpois: real-size interpolation shares stable beta kernel") {
  // Independent 80-digit log-gamma extension, exact binary64 inputs.
  CHECK(std::abs(cppunivariate::zero_detail::bin_logpmf(1000000,1000023.25,.99998160042319029)
      -(-3.085115399049207))<1e-12);
  CHECK(std::abs(cppunivariate::zero_detail::bin_logpmf(500000000,1000000000.25,.5)
      -(-10.587424271524183))<1e-12);
}

TEST_CASE("ztpois: negligible zero mass preserves a large-rate body CDF") {
  const double got=cppunivariate::distributions::pztpois(99999999,1e8,true,true);
  CHECK(std::isfinite(got));
  CHECK(got==doctest::Approx(cppunivariate::distributions::ppois(99999999,1e8,true,true)).epsilon(1e-12));
}

#include "roundtrips.hpp"
TEST_CASE("ztpois: modifier four-mode round trips") {
 mltest::modifier_roundtrips("ztpois",
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::pztpois(x,p[0],l,g);},
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::qztpois(x,p[0],l,g);});
}
