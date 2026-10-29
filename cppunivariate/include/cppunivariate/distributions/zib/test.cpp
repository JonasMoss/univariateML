#include "test_support.hpp"
#include "cppunivariate/distributions/zib/mle.hpp"
#include "cppunivariate/distributions/zib/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("zib: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("zib");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::zib_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZibStatus::Ok);
 CHECK(fit.size==doctest::Approx(g.estimates.at("size")).epsilon(1e-10));
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(1e-10));
 CHECK(fit.pi==doctest::Approx(g.estimates.at("pi")).epsilon(1e-10));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
 std::vector<double> p{fit.size,fit.prob,fit.pi};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dzib(v,par[0],par[1],par[2],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[1]+=h;b[1]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 {auto a=p,b=p;const double h=1e-5;a[2]+=h;b[2]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
 const auto fixed=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,15);
 CHECK(fixed.status==cppunivariate::ZibStatus::Ok);
 CHECK(fixed.size==15);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,-1).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,std::numeric_limits<double>::infinity()).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0.5).status==cppunivariate::ZibStatus::InvalidSize);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0).status==cppunivariate::ZibStatus::InvalidSize);
}
TEST_CASE("zib: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::zib_mle({}).status==cppunivariate::ZibStatus::InvalidInput);
 CHECK(cppunivariate::zib_mle({negative.data(),negative.size()}).status==cppunivariate::ZibStatus::InvalidInput);
 const auto one=cppunivariate::zib_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZibStatus::Ok);
 const auto zero=cppunivariate::zib_mle({zeros.data(),zeros.size()});
 CHECK(zero.logLik==0);
 CHECK(zero.status==cppunivariate::ZibStatus::Ok);
}
TEST_CASE("zib: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("zib");
 CHECK(std::isfinite(cppunivariate::distributions::dzib(4,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),true)));
 CHECK(cppunivariate::distributions::qzib(0,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))==0);
 CHECK(cppunivariate::distributions::qzib(1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))==g.estimates.at("size"));
}

TEST_CASE("zib: infinite-size escape and fixed-size removal") {
 std::vector<int> x{0,0,0,0,0,0,0,0,1,1,1,2,3,7,12};
 CHECK(cppunivariate::zib_mle({x.data(),x.size()}).status==cppunivariate::ZibStatus::ZipLimit);
 CHECK(cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,20).status==cppunivariate::ZibStatus::Ok);
}

TEST_CASE("zib: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("zib");
 const double q=cppunivariate::distributions::qzib(.37,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"));
 CHECK(cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))>=.37);
 CHECK(cppunivariate::distributions::pzib(q-1,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"))<.37);
 CHECK(cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),true,false)+cppunivariate::distributions::pzib(q,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rzib(rng,g.estimates.at("size"),g.estimates.at("prob"),g.estimates.at("pi"));CHECK(v>=0);CHECK(v==std::floor(v));}
}

TEST_CASE("zib retains tiny positive probabilities") {
 CHECK(std::isfinite(cppunivariate::distributions::dzib(1,10,1e-20,.5,true)));
 CHECK(cppunivariate::distributions::pzib(0,10,1e-20,.5,false,true)==doctest::Approx(std::log(5e-20)));
 CHECK(cppunivariate::distributions::qzib(-50,10,1e-20,.5,false,true)==1);
 CHECK(cppunivariate::distributions::qzib(1,10,.4,1)==0);
 CHECK(cppunivariate::distributions::pzib(4,5,1,.3)==doctest::Approx(.3));
}

TEST_CASE("zib fixed zero size is an attained member") {
 std::vector<int> x{0,0};
 const auto fit=cppunivariate::zib_mle({x.data(),x.size()},1e-10,200,0);
 CHECK(fit.status==cppunivariate::ZibStatus::Ok);
 CHECK(fit.size==0);
 CHECK(fit.logLik==0);
 CHECK(cppunivariate::distributions::dzib(0,0,.5,.3)==1);
 CHECK(cppunivariate::distributions::qzib(1,0,.5,.3)==0);
}
