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
