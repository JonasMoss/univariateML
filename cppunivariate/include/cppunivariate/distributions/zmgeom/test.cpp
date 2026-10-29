#include "test_support.hpp"
#include "cppunivariate/distributions/zmgeom/mle.hpp"
#include "cppunivariate/distributions/zmgeom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zmgeom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zmgeom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zmgeom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZmgeomStatus::Ok);
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.p0==doctest::Approx(g.estimates.at("p0")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.prob,fit.p0};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzmgeom(v,par[0],par[1],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("zmgeom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zmgeom_mle({}).status==cppunivariate::ZmgeomStatus::InvalidInput);
 CHECK(cppunivariate::zmgeom_mle({negative.data(),negative.size()}).status==cppunivariate::ZmgeomStatus::InvalidInput);
 const auto one=cppunivariate::zmgeom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZmgeomStatus::TwoPoint);
 const auto zero=cppunivariate::zmgeom_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZmgeomStatus::AllZero);
 std::vector<int> two{0,0,1,1};
 CHECK(cppunivariate::zmgeom_mle({two.data(),two.size()}).status==cppunivariate::ZmgeomStatus::TwoPoint);
}
TEST_CASE("zmgeom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zmgeom");
 CHECK(std::isfinite(cppunivariate::distributions::dzmgeom(10000,g.estimates.at("prob"),g.estimates.at("p0"),true)));
 CHECK(cppunivariate::distributions::qzmgeom(0,g.estimates.at("prob"),g.estimates.at("p0"))==0);
 CHECK(cppunivariate::distributions::qzmgeom(1,g.estimates.at("prob"),g.estimates.at("p0"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("zmgeom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zmgeom");
 const double q=cppunivariate::distributions::qzmgeom(.37,g.estimates.at("prob"),g.estimates.at("p0"));
 CHECK(cppunivariate::distributions::pzmgeom(q,g.estimates.at("prob"),g.estimates.at("p0"))>=.37);
 CHECK(cppunivariate::distributions::pzmgeom(q-1,g.estimates.at("prob"),g.estimates.at("p0"))<.37);
 CHECK(cppunivariate::distributions::pzmgeom(q,g.estimates.at("prob"),g.estimates.at("p0"),true,false)+cppunivariate::distributions::pzmgeom(q,g.estimates.at("prob"),g.estimates.at("p0"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzmgeom(rng,g.estimates.at("prob"),g.estimates.at("p0"));CHECK(v>=0);CHECK(v==std::floor(v));}
}
