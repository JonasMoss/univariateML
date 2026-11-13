#include "test_support.hpp"
#include "cppunivariate/distributions/geom/ztgeom_mle.hpp"
#include "cppunivariate/distributions/geom/ztgeom_dpqr.hpp"
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

#include "cppunivariate/distributions/geom/ztgeom_dpqr.hpp"
TEST_CASE("ztgeom: overflow quantiles reach the target beyond DBL_MAX") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qztgeom(.5,std::numeric_limits<double>::denorm_min()),.5,true,
      [](double x) {return cppunivariate::distributions::pztgeom(x,std::numeric_limits<double>::denorm_min());},true));
}

#include "roundtrips.hpp"
TEST_CASE("ztgeom: modifier four-mode round trips") {
 mltest::modifier_roundtrips("ztgeom",
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::pztgeom(x,p[0],l,g);},
 [](double x,const std::vector<double>& p,bool l,bool g){return cppunivariate::distributions::qztgeom(x,p[0],l,g);});
}

TEST_CASE("ztgeom: public fuzz does not change the sampling inverse") {
  struct FixedRng {double runif(double,double) {return .5+std::numeric_limits<double>::epsilon();}} rng;
  CHECK(cppunivariate::distributions::qztgeom(rng.runif(0,1),.5)==1);
  CHECK(cppunivariate::distributions::rztgeom(rng,.5)==2);
}

TEST_CASE("ztgeom: exact rational modifier ties") {
 CHECK(cppunivariate::distributions::qztgeom(0.5,0.5,true,false)==1);
 CHECK(cppunivariate::distributions::qztgeom(-0.6931471805599453,0.5,true,true)==1);
 CHECK(cppunivariate::distributions::qztgeom(0.5,0.5,false,false)==1);
 CHECK(cppunivariate::distributions::qztgeom(-0.6931471805599453,0.5,false,true)==1);
 CHECK(cppunivariate::distributions::qztgeom(0.75,0.5,true,false)==2);
 CHECK(cppunivariate::distributions::qztgeom(-0.2876820724517809,0.5,true,true)==2);
 CHECK(cppunivariate::distributions::qztgeom(0.25,0.5,false,false)==2);
 CHECK(cppunivariate::distributions::qztgeom(-1.3862943611198906,0.5,false,true)==2);
 CHECK(cppunivariate::distributions::qztgeom(0.875,0.5,true,false)==3);
 CHECK(cppunivariate::distributions::qztgeom(-0.13353139262452263,0.5,true,true)==3);
 CHECK(cppunivariate::distributions::qztgeom(0.125,0.5,false,false)==3);
 CHECK(cppunivariate::distributions::qztgeom(-2.0794415416798357,0.5,false,true)==3);
 CHECK(cppunivariate::distributions::qztgeom(0.9375,0.5,true,false)==4);
 CHECK(cppunivariate::distributions::qztgeom(-0.06453852113757118,0.5,true,true)==4);
 CHECK(cppunivariate::distributions::qztgeom(0.0625,0.5,false,false)==4);
 CHECK(cppunivariate::distributions::qztgeom(-2.772588722239781,0.5,false,true)==4);
 CHECK(cppunivariate::distributions::qztgeom(0.96875,0.5,true,false)==5);
 CHECK(cppunivariate::distributions::qztgeom(-0.0317486983145803,0.5,true,true)==5);
 CHECK(cppunivariate::distributions::qztgeom(0.03125,0.5,false,false)==5);
 CHECK(cppunivariate::distributions::qztgeom(-3.4657359027997265,0.5,false,true)==5);
 CHECK(cppunivariate::distributions::qztgeom(0.984375,0.5,true,false)==6);
 CHECK(cppunivariate::distributions::qztgeom(-0.015748356968139168,0.5,true,true)==6);
 CHECK(cppunivariate::distributions::qztgeom(0.015625,0.5,false,false)==6);
 CHECK(cppunivariate::distributions::qztgeom(-4.1588830833596715,0.5,false,true)==6);
 CHECK(cppunivariate::distributions::qztgeom(0.9921875,0.5,true,false)==7);
 CHECK(cppunivariate::distributions::qztgeom(-0.007843177461025893,0.5,true,true)==7);
 CHECK(cppunivariate::distributions::qztgeom(0.0078125,0.5,false,false)==7);
 CHECK(cppunivariate::distributions::qztgeom(-4.852030263919617,0.5,false,true)==7);
 CHECK(cppunivariate::distributions::qztgeom(0.99609375,0.5,true,false)==8);
 CHECK(cppunivariate::distributions::qztgeom(-0.003913899321136329,0.5,true,true)==8);
 CHECK(cppunivariate::distributions::qztgeom(0.00390625,0.5,false,false)==8);
 CHECK(cppunivariate::distributions::qztgeom(-5.545177444479562,0.5,false,true)==8);
 CHECK(cppunivariate::distributions::qztgeom(0.998046875,0.5,true,false)==9);
 CHECK(cppunivariate::distributions::qztgeom(-0.0019550348358033506,0.5,true,true)==9);
 CHECK(cppunivariate::distributions::qztgeom(0.001953125,0.5,false,false)==9);
 CHECK(cppunivariate::distributions::qztgeom(-6.238324625039508,0.5,false,true)==9);
 CHECK(cppunivariate::distributions::qztgeom(0.9990234375,0.5,true,false)==10);
 CHECK(cppunivariate::distributions::qztgeom(-0.0009770396478266127,0.5,true,true)==10);
 CHECK(cppunivariate::distributions::qztgeom(0.0009765625,0.5,false,false)==10);
 CHECK(cppunivariate::distributions::qztgeom(-6.931471805599453,0.5,false,true)==10);
 CHECK(cppunivariate::distributions::qztgeom(0.99951171875,0.5,true,false)==11);
 CHECK(cppunivariate::distributions::qztgeom(-0.0004884004981088745,0.5,true,true)==11);
 CHECK(cppunivariate::distributions::qztgeom(0.00048828125,0.5,false,false)==11);
 CHECK(cppunivariate::distributions::qztgeom(-7.6246189861593985,0.5,false,true)==11);
 CHECK(cppunivariate::distributions::qztgeom(0.999755859375,0.5,true,false)==12);
 CHECK(cppunivariate::distributions::qztgeom(-0.0002441704321739145,0.5,true,true)==12);
 CHECK(cppunivariate::distributions::qztgeom(0.000244140625,0.5,false,false)==12);
 CHECK(cppunivariate::distributions::qztgeom(-8.317766166719343,0.5,false,true)==12);
 CHECK(cppunivariate::distributions::qztgeom(0.9998779296875,0.5,true,false)==13);
 CHECK(cppunivariate::distributions::qztgeom(-0.00012207776368698225,0.5,true,true)==13);
 CHECK(cppunivariate::distributions::qztgeom(0.0001220703125,0.5,false,false)==13);
 CHECK(cppunivariate::distributions::qztgeom(-9.010913347279288,0.5,false,true)==13);
 CHECK(cppunivariate::distributions::qztgeom(0.99993896484375,0.5,true,false)==14);
 CHECK(cppunivariate::distributions::qztgeom(-6.103701897094392e-05,0.5,true,true)==14);
 CHECK(cppunivariate::distributions::qztgeom(6.103515625e-05,0.5,false,false)==14);
 CHECK(cppunivariate::distributions::qztgeom(-9.704060527839234,0.5,false,true)==14);
 CHECK(cppunivariate::distributions::qztgeom(0.999969482421875,0.5,true,false)==15);
 CHECK(cppunivariate::distributions::qztgeom(-3.0518043795761425e-05,0.5,true,true)==15);
 CHECK(cppunivariate::distributions::qztgeom(3.0517578125e-05,0.5,false,false)==15);
 CHECK(cppunivariate::distributions::qztgeom(-10.39720770839918,0.5,false,true)==15);
 CHECK(cppunivariate::distributions::qztgeom(0.9999847412109375,0.5,true,false)==16);
 CHECK(cppunivariate::distributions::qztgeom(-1.5258905479006077e-05,0.5,true,true)==16);
 CHECK(cppunivariate::distributions::qztgeom(1.52587890625e-05,0.5,false,false)==16);
 CHECK(cppunivariate::distributions::qztgeom(-11.090354888959125,0.5,false,true)==16);
 CHECK(cppunivariate::distributions::qztgeom(0.9999923706054688,0.5,true,false)==17);
 CHECK(cppunivariate::distributions::qztgeom(-7.629423635228488e-06,0.5,true,true)==17);
 CHECK(cppunivariate::distributions::qztgeom(7.62939453125e-06,0.5,false,false)==17);
 CHECK(cppunivariate::distributions::qztgeom(-11.78350206951907,0.5,false,true)==17);
 CHECK(cppunivariate::distributions::qztgeom(0.9999961853027344,0.5,true,false)==18);
 CHECK(cppunivariate::distributions::qztgeom(-3.814704541601118e-06,0.5,true,true)==18);
 CHECK(cppunivariate::distributions::qztgeom(3.814697265625e-06,0.5,false,false)==18);
 CHECK(cppunivariate::distributions::qztgeom(-12.476649250079015,0.5,false,true)==18);
 CHECK(cppunivariate::distributions::qztgeom(0.9999980926513672,0.5,true,false)==19);
 CHECK(cppunivariate::distributions::qztgeom(-1.9073504518042164e-06,0.5,true,true)==19);
 CHECK(cppunivariate::distributions::qztgeom(1.9073486328125e-06,0.5,false,false)==19);
 CHECK(cppunivariate::distributions::qztgeom(-13.16979643063896,0.5,false,true)==19);
 CHECK(cppunivariate::distributions::qztgeom(0.9999990463256836,0.5,true,false)==20);
 CHECK(cppunivariate::distributions::qztgeom(-9.5367477115389e-07,0.5,true,true)==20);
 CHECK(cppunivariate::distributions::qztgeom(9.5367431640625e-07,0.5,false,false)==20);
 CHECK(cppunivariate::distributions::qztgeom(-13.862943611198906,0.5,false,true)==20);
}
