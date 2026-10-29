#include "test_support.hpp"
#include "cppunivariate/distributions/zmlogarithmic/mle.hpp"
#include "cppunivariate/distributions/zmlogarithmic/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zmlogarithmic: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zmlogarithmic");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zmlogarithmic_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZmlogarithmicStatus::Ok);
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.p0==doctest::Approx(g.estimates.at("p0")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.prob,fit.p0};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzmlogarithmic(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("zmlogarithmic: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zmlogarithmic_mle({}).status==cppunivariate::ZmlogarithmicStatus::InvalidInput);
 CHECK(cppunivariate::zmlogarithmic_mle({negative.data(),negative.size()}).status==cppunivariate::ZmlogarithmicStatus::InvalidInput);
 const auto one=cppunivariate::zmlogarithmic_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZmlogarithmicStatus::TwoPoint);
 const auto zero=cppunivariate::zmlogarithmic_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZmlogarithmicStatus::AllZero);
 std::vector<int> two{0,0,1,1};
 CHECK(cppunivariate::zmlogarithmic_mle({two.data(),two.size()}).status==cppunivariate::ZmlogarithmicStatus::TwoPoint);
}
TEST_CASE("zmlogarithmic: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zmlogarithmic");
 CHECK(std::isfinite(cppunivariate::distributions::dzmlogarithmic(10000,g.estimates.at("prob"),g.estimates.at("p0"),true)));
 CHECK(cppunivariate::distributions::qzmlogarithmic(0,g.estimates.at("prob"),g.estimates.at("p0"))==0);
 CHECK(cppunivariate::distributions::qzmlogarithmic(1,g.estimates.at("prob"),g.estimates.at("p0"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("zmlogarithmic: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zmlogarithmic");
 const double q=cppunivariate::distributions::qzmlogarithmic(.37,g.estimates.at("prob"),g.estimates.at("p0"));
 CHECK(cppunivariate::distributions::pzmlogarithmic(q,g.estimates.at("prob"),g.estimates.at("p0"))>=.37);
 CHECK(cppunivariate::distributions::pzmlogarithmic(q-1,g.estimates.at("prob"),g.estimates.at("p0"))<.37);
 CHECK(cppunivariate::distributions::pzmlogarithmic(q,g.estimates.at("prob"),g.estimates.at("p0"),true,false)+cppunivariate::distributions::pzmlogarithmic(q,g.estimates.at("prob"),g.estimates.at("p0"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzmlogarithmic(rng,g.estimates.at("prob"),g.estimates.at("p0"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

TEST_CASE("logarithmic reuse brackets near the point mass") {
 std::vector<int> x{1,1,1,2};
 const auto fit=cppunivariate::zmlogarithmic_mle({x.data(),x.size()});
 CHECK(fit.status==cppunivariate::ZmlogarithmicStatus::Ok);
 CHECK(std::isfinite(fit.logLik));
 CHECK(-fit.prob/((1-fit.prob)*std::log1p(-fit.prob))==doctest::Approx(1.25).epsilon(1e-10));
 const auto nb=cppunivariate::ztnbinom_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(nb.prob==doctest::Approx(1-fit.prob).epsilon(1e-10));
 CHECK(nb.logLik==doctest::Approx(fit.logLik).epsilon(1e-10));
}

TEST_CASE("zmlogarithmic: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("zmlogarithmic");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::zmlogarithmic_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("prob"):nan,(mask & 2)?g.estimates.at("p0"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.prob==g.estimates.at("prob"));
   if(mask & 2)CHECK(r.p0==g.estimates.at("p0"));
 }
}

TEST_CASE("zmlogarithmic: P2 held zero share and incompatible positive component") {
 const std::vector<int> x{0,1,2},z{0,0};
 CHECK(cppunivariate::zmlogarithmic_mle({x.data(),x.size()},1e-10,200,0.4,0).status==cppunivariate::ZmlogarithmicStatus::InvalidInput);
 CHECK(cppunivariate::zmlogarithmic_mle({x.data(),x.size()},1e-10,200,0,.3).status==cppunivariate::ZmlogarithmicStatus::InvalidInput);
 const auto r=cppunivariate::zmlogarithmic_mle({z.data(),z.size()},1e-10,200,0.4,.7);
 CHECK(r.prob==0.4);CHECK(r.p0==.7);
 CHECK(r.logLik==doctest::Approx(2*std::log(.7)));
}
