#include "test_support.hpp"
#include "cppunivariate/distributions/zmbinom/mle.hpp"
#include "cppunivariate/distributions/zmbinom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zmbinom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zmbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zmbinom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZmbinomStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.p0==doctest::Approx(g.estimates.at("p0")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob,fit.p0};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzmbinom(v,par[0],par[1],par[2],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[2]+=h;b[2]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,15);
 CHECK(fixed.status==cppunivariate::ZmbinomStatus::Ok);
 CHECK(fixed.size==15);
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZmbinomStatus::InvalidSize);
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZmbinomStatus::InvalidSize);
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,0.5).status==cppunivariate::ZmbinomStatus::InvalidSize);
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZmbinomStatus::InvalidSize);
}
TEST_CASE("zmbinom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zmbinom_mle({}).status==cppunivariate::ZmbinomStatus::InvalidInput);
 CHECK(cppunivariate::zmbinom_mle({negative.data(),negative.size()}).status==cppunivariate::ZmbinomStatus::InvalidInput);
 const auto one=cppunivariate::zmbinom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZmbinomStatus::TwoPoint);
 const auto zero=cppunivariate::zmbinom_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZmbinomStatus::AllZero);
 std::vector<int> two{0,0,1,1};
 CHECK(cppunivariate::zmbinom_mle({two.data(),two.size()}).status==cppunivariate::ZmbinomStatus::TwoPoint);
}
TEST_CASE("zmbinom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zmbinom");
 CHECK(std::isfinite(cppunivariate::distributions::dzmbinom(4,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),true)));
 CHECK(cppunivariate::distributions::qzmbinom(0,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))==0);
 CHECK(cppunivariate::distributions::qzmbinom(1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))==g.estimates.at("size"));
}

TEST_CASE("zmbinom: infinite-size escape and fixed-size removal") {
 std::vector<int> x{0,0,1,1,1,2,3,7,12};
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()}).status==cppunivariate::ZmbinomStatus::PoissonLimit);
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZmbinomStatus::Ok);
}

TEST_CASE("zmbinom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zmbinom");
 const double q=cppunivariate::distributions::qzmbinom(.37,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"));
 CHECK(cppunivariate::distributions::pzmbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))>=.37);
 CHECK(cppunivariate::distributions::pzmbinom(q-1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"))<.37);
 CHECK(cppunivariate::distributions::pzmbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),true,false)+cppunivariate::distributions::pzmbinom(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzmbinom(rng,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("p0"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("zmbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("zmbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzmbinom(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzmbinom(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzmbinom(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzmbinom(rng, p[0], p[1], p[2]); });
}

TEST_CASE("zmbinom: held probability with free and held size") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 const std::vector<int> x{1,2,2,3,4};
 const auto f=cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,nan,.4,.3);
 REQUIRE(f.status==cppunivariate::ZmbinomStatus::Ok);
 CHECK(f.prob==.4);
 for(int k=4;k<100;++k) {
  const auto o=cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,k,.4,.3);
  CHECK(f.logLik>=o.logLik-1e-9);
 }
 CHECK(cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,8,-.1,.3).status==cppunivariate::ZmbinomStatus::InvalidInput);
}

TEST_CASE("zmbinom: P3 unfinished optimizer is a failure") {
  const std::vector<int> values{1,2,3,3,5};
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::zmbinom_mle(ix,1e-8,1).status==cppunivariate::ZmbinomStatus::IterationFailure);
}

TEST_CASE("zmbinom attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rzmbinom(rng,5,1,0)==5);
}

TEST_CASE("zmbinom TASK-134 nominal design endpoints with absent masses") {
  CHECK(cppunivariate::distributions::qzmbinom(0,10,.5,0)==0);
  CHECK(cppunivariate::distributions::qzmbinom(1,10,0,.5)==10);
  CHECK(cppunivariate::distributions::qzmbinom(-INFINITY,10,.5,1,false,true)==10);
  CHECK(cppunivariate::distributions::qzmbinom(.75,10,0,.5)==1);
}

#include "../zmpois/dpqr.hpp"

TEST_CASE("zmbinom: structured stop payloads") {
  const std::vector<int> x{0,0,1,1,1,2,3,7,12};
  const auto r=cppunivariate::zmbinom_mle({x.data(),x.size()});
  REQUIRE(r.stop_detail.limit_model==cppunivariate::StopModel::Zmpois);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  double ll=0;for(int v:x)ll+=cppunivariate::distributions::dzmpois(v,r.stop_detail.limit_coef[0],r.stop_detail.limit_coef[1],true);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
}

TEST_CASE("zmbinom: certificate stop witnesses include the zero-indicator likelihood") {
  const std::vector<int>x{0,0,1,1,1,2,3,7,12};
  const auto f=cppunivariate::zmbinom_mle({x.data(),x.size()},1e-10,200,NAN,NAN,.2);
  REQUIRE(f.stop_detail.comparison_tie);
  CHECK(f.stop_detail.limit_model==cppunivariate::StopModel::Zmpois);
  for(const auto& c:f.stop_detail.candidates) {
    REQUIRE(c.coef.size()==(c.model==cppunivariate::StopModel::None?3:2));
    CHECK(c.coef.back()==.2);
  }
  CHECK(f.stop_detail.loglik_upper==INFINITY);
}
