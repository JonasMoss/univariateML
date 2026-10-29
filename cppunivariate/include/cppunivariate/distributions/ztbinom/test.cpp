#include "test_support.hpp"
#include "cppunivariate/distributions/ztbinom/mle.hpp"
#include "cppunivariate/distributions/ztbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztbinom(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,15);
 CHECK(fixed.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(fixed.size==15);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,0.5).status==cppunivariate::ZtbinomStatus::InvalidSize);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZtbinomStatus::InvalidSize);
}
TEST_CASE("ztbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztbinom_mle({}).status==cppunivariate::ZtbinomStatus::InvalidInput);
 CHECK(cppunivariate::ztbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZtbinomStatus::InvalidInput);
 const auto one=cppunivariate::ztbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtbinomStatus::Ok);
 CHECK(cppunivariate::ztbinom_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtbinomStatus::InvalidInput);
}
TEST_CASE("ztbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dztbinom(4,g.estimates.at("size"),g.estimates.at("prob"),true)));
 CHECK(cppunivariate::distributions::qztbinom(0,g.estimates.at("size"),g.estimates.at("prob"))==1);
 CHECK(cppunivariate::distributions::qztbinom(1,g.estimates.at("size"),g.estimates.at("prob"))==g.estimates.at("size"));
}

TEST_CASE("ztbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{1,1,1,2,3,7,12};
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()}).status==cppunivariate::ZtbinomStatus::PoissonLimit);
 CHECK(cppunivariate::ztbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZtbinomStatus::Ok);
}

TEST_CASE("ztbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztbinom");
 const double q=cppunivariate::distributions::qztbinom(.37,g.estimates.at("size"),g.estimates.at("prob"));
 CHECK(cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"))>=.37);
 CHECK(cppunivariate::distributions::pztbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"))<.37);
 CHECK(cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"),true,false)+cppunivariate::distributions::pztbinom(q,g.estimates.at("size"),g.estimates.at("prob"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztbinom(rng,g.estimates.at("size"),g.estimates.at("prob"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("ztbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ztbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztbinom(rng, p[0], p[1]); });
}

TEST_CASE("ztbinom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // n=mpf(8.0); p=mpf(0.4)
  // d=lambda k:binomial(n,k)*p**k*(1-p)**(n-k)/(1-(1-p)**n); S=lambda k:fsum(d(j) for j in range(int(k)+1,int(n)+1))
  // print(log(d(mpf(4.0)))); print(log(S(mpf(4.0))))
  // lo,hi=0,8
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dztbinom(4.0, 8.0, 0.4, true) == doctest::Approx(-1.4430313653876057).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pztbinom(4.0, 8.0, 0.4, false, true) == doctest::Approx(-1.7336572138833999).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qztbinom(-10000, 8.0, 0.4, false, true) == 8.0);
  CHECK(cppunivariate::distributions::dztbinom(0.5, 8.0, 0.4, false) == 0);
}

TEST_CASE("ztbinom: tiny conditional lower tail") {
  // from mpmath import *; mp.dps=100; print(log(8)+log(mpf(0.94))+7*log(1-mpf(0.94))-log(1-(1-mpf(0.94))**8))
  CHECK(cppunivariate::distributions::pztbinom(1,8.0,0.94,true,true)==doctest::Approx(-17.67630887919054).epsilon(1e-10));
}

TEST_CASE("ztbinom: unit probability is a point mass at size") {
  // from mpmath import *; mp.dps=100
  // n=5; p=mpf(1); print(fsum(binomial(n,k)*p**k*(1-p)**(n-k) for k in range(1,5)))
  CHECK(cppunivariate::distributions::pztbinom(4,5,1,true,false)==0);
  CHECK(cppunivariate::distributions::pztbinom(4,5,1,false,true)==0);
  CHECK(cppunivariate::distributions::qztbinom(0.3,5,1,true,false)==5);
}
