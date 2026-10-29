#include "test_support.hpp"
#include "cppunivariate/distributions/zmpois/mle.hpp"
#include "cppunivariate/distributions/zmpois/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zmpois: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zmpois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zmpois_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZmpoisStatus::Ok);
 CHECK(fit.lambda==doctest::Approx(g.estimates.at("lambda")).epsilon(1e-10));
 CHECK(fit.p0==doctest::Approx(g.estimates.at("p0")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.lambda,fit.p0};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzmpois(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("zmpois: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zmpois_mle({}).status==cppunivariate::ZmpoisStatus::InvalidInput);
 CHECK(cppunivariate::zmpois_mle({negative.data(),negative.size()}).status==cppunivariate::ZmpoisStatus::InvalidInput);
 const auto one=cppunivariate::zmpois_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZmpoisStatus::TwoPoint);
 const auto zero=cppunivariate::zmpois_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZmpoisStatus::AllZero);
 std::vector<int> two{0,0,1,1};
 CHECK(cppunivariate::zmpois_mle({two.data(),two.size()}).status==cppunivariate::ZmpoisStatus::TwoPoint);
}
TEST_CASE("zmpois: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zmpois");
 CHECK(std::isfinite(cppunivariate::distributions::dzmpois(10000,g.estimates.at("lambda"),g.estimates.at("p0"),true)));
 CHECK(cppunivariate::distributions::qzmpois(0,g.estimates.at("lambda"),g.estimates.at("p0"))==0);
 CHECK(cppunivariate::distributions::qzmpois(1,g.estimates.at("lambda"),g.estimates.at("p0"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("zmpois: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zmpois");
 const double q=cppunivariate::distributions::qzmpois(.37,g.estimates.at("lambda"),g.estimates.at("p0"));
 CHECK(cppunivariate::distributions::pzmpois(q,g.estimates.at("lambda"),g.estimates.at("p0"))>=.37);
 CHECK(cppunivariate::distributions::pzmpois(q-1,g.estimates.at("lambda"),g.estimates.at("p0"))<.37);
 CHECK(cppunivariate::distributions::pzmpois(q,g.estimates.at("lambda"),g.estimates.at("p0"),true,false)+cppunivariate::distributions::pzmpois(q,g.estimates.at("lambda"),g.estimates.at("p0"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzmpois(rng,g.estimates.at("lambda"),g.estimates.at("p0"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("zmpois: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("zmpois",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dzmpois(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pzmpois(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qzmpois(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rzmpois(rng, p[0], p[1]); });
}

TEST_CASE("zmpois: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zmpois");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::zmpois_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("lambda"):nan,(mask & 2)?g.estimates.at("p0"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.lambda==g.estimates.at("lambda"));
   if(mask & 2)CHECK(r.p0==g.estimates.at("p0"));
 }
}

TEST_CASE("zmpois: P2 held zero share and incompatible positive component") {
 const std::vector<int> x{0,1,2},z{0,0};
 CHECK(cppunivariate::zmpois_mle({x.data(),x.size()},1e-10,200,2,0).status==cppunivariate::ZmpoisStatus::InvalidInput);
 CHECK(cppunivariate::zmpois_mle({x.data(),x.size()},1e-10,200,0,.3).status==cppunivariate::ZmpoisStatus::InvalidInput);
 const auto r=cppunivariate::zmpois_mle({z.data(),z.size()},1e-10,200,2,.7);
 CHECK(r.lambda==2);CHECK(r.p0==.7);
 CHECK(r.logLik==doctest::Approx(2*std::log(.7)));
}
