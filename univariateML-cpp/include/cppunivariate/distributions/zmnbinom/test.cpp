#include "test_support.hpp"
#include "cppunivariate/distributions/zmnbinom/mle.hpp"
#include "cppunivariate/distributions/zmnbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zmnbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zmnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zmnbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZmnbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(3e-6));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(3e-6));
 CHECK(fit.p0==doctest::Approx(g.estimates.at("p0")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob,fit.p0};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzmnbinom(v,par[0],par[1],par[2],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[2]+=h;b[2]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,2.5);
 CHECK(fixed.status==cppunivariate::ZmnbinomStatus::Ok);
 CHECK(fixed.size==2.5);
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZmnbinomStatus::InvalidSize);
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZmnbinomStatus::InvalidSize);
}
TEST_CASE("zmnbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zmnbinom_mle({}).status==cppunivariate::ZmnbinomStatus::InvalidInput);
 CHECK(cppunivariate::zmnbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZmnbinomStatus::InvalidInput);
 const auto one=cppunivariate::zmnbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZmnbinomStatus::TwoPoint);
 const auto zero=cppunivariate::zmnbinom_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZmnbinomStatus::AllZero);
 std::vector<int> two{0,0,1,1};
 CHECK(cppunivariate::zmnbinom_mle({two.data(),two.size()}).status==cppunivariate::ZmnbinomStatus::TwoPoint);
}
TEST_CASE("zmnbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zmnbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dzmnbinom(10000,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),true)));
 CHECK(cppunivariate::distributions::qzmnbinom(0,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))==0);
 CHECK(cppunivariate::distributions::qzmnbinom(1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("zmnbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{0,0,1,2,2,3,3,3};
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()}).status==cppunivariate::ZmnbinomStatus::PoissonLimit);
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZmnbinomStatus::Ok);
}

TEST_CASE("zmnbinom: attained logarithmic face") {
 std::vector<int> x{0,0,1,1,1,1,1,2,3,10,30};
 const auto fit=cppunivariate::zmnbinom_mle({x.data(),x.size()});
 CHECK(fit.status==cppunivariate::ZmnbinomStatus::Ok);
 CHECK(fit.size==0);
 const auto fixed=cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(fixed.size==0);
 CHECK(fixed.logLik==doctest::Approx(fit.logLik));
}

TEST_CASE("zmnbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zmnbinom");
 const double q=cppunivariate::distributions::qzmnbinom(.37,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"));
 CHECK(cppunivariate::distributions::pzmnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))>=.37);
 CHECK(cppunivariate::distributions::pzmnbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))<.37);
 CHECK(cppunivariate::distributions::pzmnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),true,false)+cppunivariate::distributions::pzmnbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzmnbinom(rng,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("zmnbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zmnbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzmnbinom(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzmnbinom(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzmnbinom(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzmnbinom(rng, p[0], p[1], p[2]); });
}

TEST_CASE("zmnbinom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zmnbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<8;++mask) {
   const auto r=cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan,(mask & 4)?g.estimates.at("p0"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
   if(mask & 4)CHECK(r.p0==g.estimates.at("p0"));
 }
}

TEST_CASE("zmnbinom: P2 held zero share and incompatible positive component") {
 const std::vector<int> x{0,1,2},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,nan,0.4,0).status==cppunivariate::ZmnbinomStatus::InvalidInput);
 CHECK(cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,nan,1,.3).status==cppunivariate::ZmnbinomStatus::InvalidInput);
 const auto r=cppunivariate::zmnbinom_mle({z.data(),z.size()},1e-10,200,nan,0.4,.7);
 CHECK(r.prob==0.4);CHECK(r.p0==.7);
 CHECK(r.logLik==doctest::Approx(2*std::log(.7)));
}


TEST_CASE("zmnbinom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // s=mpf(2.0); p=mpf(0.4); p0=mpf(0.3)
  // d=lambda k:(1-p0)*gamma(k+s)/(gamma(s)*gamma(k+1))*p**s*(1-p)**k/(1-p**s); S=lambda k:(1-p0)*betainc(k+1,s,0,1-p,regularized=True)/(1-p**s)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(134.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dzmnbinom(1e+100, 2.0, 0.4, 0.3, true) == doctest::Approx(-5.108256237659907e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pzmnbinom(134.0, 2.0, 0.4, 0.3, false, true) == doctest::Approx(-65.13644757997024).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qzmnbinom(-10000, 2.0, 0.4, 0.3, false, true) == 19593.0);
  CHECK(cppunivariate::distributions::dzmnbinom(0.5, 2.0, 0.4, 0.3, false) == 0);
}

#include "cppunivariate/distributions/zmpois/mle.hpp"
TEST_CASE("zmnbinom: certificate details survive default and held zero share") {
 const std::vector<int>x{0,0,1,2,2,3,3,3};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(double held:{nan,.4}) {
   const auto fit=cppunivariate::zmnbinom_mle({x.data(),x.size()},1e-10,200,nan,nan,held);
   REQUIRE(fit.status==cppunivariate::ZmnbinomStatus::PoissonLimit);
   REQUIRE(fit.stop_detail.limit_coef.size()==2);
   CHECK(fit.stop_detail.limit_coef[1]==(std::isnan(held)?.25:held));
   const auto face=cppunivariate::zmpois_mle({x.data(),x.size()},1e-10,200,nan,held);
   CHECK(fit.stop_detail.limit_loglik==doctest::Approx(face.logLik).epsilon(1e-12));
 }
 const std::vector<int>finite{0,0,1,2,5};
 const auto fail=cppunivariate::zmnbinom_mle({finite.data(),finite.size()},1e-10,1);
 CHECK(fail.status==cppunivariate::ZmnbinomStatus::IterationFailure);
 CHECK(fail.budget_exhausted);CHECK(std::isnan(fail.size));
 CHECK(fail.stop_detail.loglik_upper<0);
}
