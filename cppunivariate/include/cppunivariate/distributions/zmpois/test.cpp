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
