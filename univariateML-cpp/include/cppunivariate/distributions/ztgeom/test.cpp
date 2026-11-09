#include "test_support.hpp"
#include "cppunivariate/distributions/ztgeom/mle.hpp"
#include "cppunivariate/distributions/ztgeom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
#include <cmath>
TEST_CASE("ztgeom: golden, score and distribution") {
 const auto g=mltest::load_distribution_golden("ztgeom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const auto fit=cppunivariate::ztgeom_mle({x.data(),x.size()});
 REQUIRE(fit.status==cppunivariate::ZtgeomStatus::Ok);
 CHECK(fit.prob==doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_ztgeom).scale(0));
 CHECK(fit.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_ztgeom).scale(0));
 std::vector<double> p{fit.prob};
 auto ll=[&](const std::vector<double> &par){double l=0;for(int v:x)l+=cppunivariate::distributions::dztgeom(v,par[0],true);return l;};
 CHECK(ll(p)==doctest::Approx(fit.logLik).epsilon(1e-10));
 double norm2=0;
 {auto a=p,b=p;const double h=1e-5;a[0]+=h;b[0]-=h;const double score=(ll(a)-ll(b))/(2*h);norm2+=score*score;}
 CHECK(std::sqrt(norm2)<1e-3);
}
TEST_CASE("ztgeom: attained boundaries and invalid input") {
 std::vector<int> ones{1,1,1},zeros{0,0,0},negative{-1,1};
 CHECK(cppunivariate::ztgeom_mle({}).status==cppunivariate::ZtgeomStatus::InvalidInput);
 CHECK(cppunivariate::ztgeom_mle({negative.data(),negative.size()}).status==cppunivariate::ZtgeomStatus::InvalidInput);
 const auto one=cppunivariate::ztgeom_mle({ones.data(),ones.size()});
 CHECK(one.logLik==0);
 CHECK(one.status==cppunivariate::ZtgeomStatus::Ok);
 CHECK(cppunivariate::ztgeom_mle({zeros.data(),zeros.size()}).status==cppunivariate::ZtgeomStatus::InvalidInput);
}
TEST_CASE("ztgeom: scalar tails and quantile endpoints") {
 const auto g=mltest::load_distribution_golden("ztgeom");
 CHECK(std::isfinite(cppunivariate::distributions::dztgeom(10000,g.estimates.at("prob"),true)));
 CHECK(cppunivariate::distributions::qztgeom(0,g.estimates.at("prob"))==1);
 CHECK(cppunivariate::distributions::qztgeom(1,g.estimates.at("prob"))==std::numeric_limits<double>::infinity());
}

TEST_CASE("ztgeom: quantile inversion and sampling") {
 const auto g=mltest::load_distribution_golden("ztgeom");
 const double q=cppunivariate::distributions::qztgeom(.37,g.estimates.at("prob"));
 CHECK(cppunivariate::distributions::pztgeom(q,g.estimates.at("prob"))>=.37);
 CHECK(cppunivariate::distributions::pztgeom(q-1,g.estimates.at("prob"))<.37);
 CHECK(cppunivariate::distributions::pztgeom(q,g.estimates.at("prob"),true,false)+cppunivariate::distributions::pztgeom(q,g.estimates.at("prob"),false,false)==doctest::Approx(1));
 cppunivariate::distributions::StdRng<> rng(19);
 for(int j=0;j<20;++j) {const double v=cppunivariate::distributions::rztgeom(rng,g.estimates.at("prob"));CHECK(v>=1);CHECK(v==std::floor(v));}
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("ztgeom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ztgeom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dztgeom(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pztgeom(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qztgeom(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rztgeom(rng, p[0]); });
}

TEST_CASE("ztgeom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("ztgeom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::ztgeom_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("ztgeom: P2 held boundary likelihood and domain") {
 const std::vector<int> x{1,2},same{1,1};
 const auto incompatible=cppunivariate::ztgeom_mle({x.data(),x.size()},1);
 CHECK(incompatible.status==cppunivariate::ZtgeomStatus::InvalidInput);
 const auto valid=cppunivariate::ztgeom_mle({same.data(),same.size()},1);
 CHECK(valid.prob==1);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::ztgeom_mle({x.data(),x.size()},-1);
 CHECK(invalid.status==cppunivariate::ZtgeomStatus::InvalidInput);
}

#include "cppunivariate/distributions/ztgeom/dpqr.hpp"
TEST_CASE("ztgeom: overflow quantiles reach the target beyond DBL_MAX") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qztgeom(.5,std::numeric_limits<double>::denorm_min()),.5,true,
      [](double x) {return cppunivariate::distributions::pztgeom(x,std::numeric_limits<double>::denorm_min());},true));
}
