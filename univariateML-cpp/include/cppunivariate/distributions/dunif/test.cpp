#include "test_support.hpp"
#include "cppunivariate/distributions/dunif/mle.hpp"
#include "cppunivariate/distributions/dunif/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
namespace {
 std::vector<int> as_ints(const std::vector<double> &v) {
   std::vector<int> out;
   for (double x : v) out.push_back(static_cast<int>(x));
   return out;
 }
 cppunivariate::ispan as_ispan(const std::vector<int> &v) { return {v.data(), v.size()}; }
 }
 TEST_CASE("dunif") {
  auto g = mltest::load_distribution_golden("dunif");
  auto v = as_ints(g.data);
  auto r = cppunivariate::dunif_mle(as_ispan(v));
  CHECK(r.min == doctest::Approx(g.estimates.at("min")).epsilon(mltest::kGolden_dunif).scale(0));
  CHECK(r.max == doctest::Approx(g.estimates.at("max")).epsilon(mltest::kGolden_dunif).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_dunif).scale(0));
}

TEST_CASE("rdunif samples are integers in [min, max]") {
  cppunivariate::distributions::StdRng<> rng(107);
  const int min = 1, max = 6;
  std::vector<int> counts(max - min + 1, 0);
  const int n = 30000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rdunif(rng, min, max);
    REQUIRE(x >= min);
    REQUIRE(x <= max);
    REQUIRE(x == std::floor(x));
    counts[static_cast<int>(x) - min]++;
  }
  for (int k = min; k <= max; ++k) {
    const double empirical =
        static_cast<double>(counts[k - min]) / n;
    CHECK(std::abs(empirical - 1.0 / (max - min + 1)) < 0.02);
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("dunif: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dunif",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::ddunif(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pdunif(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qdunif(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rdunif(rng, p[0], p[1]); });
}

TEST_CASE("dunif: held endpoints tighten only free coordinates") {
  const std::vector<int> x={0,1,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(double a:{nan,-2.})for(double b:{nan,5.}) {
    const auto f=cppunivariate::dunif_mle({x.data(),x.size()},a,b);
    CHECK(f.min==(std::isnan(a)?0:a));
    CHECK(f.max==(std::isnan(b)?3:b));
    CHECK(f.logLik==doctest::Approx(-3*std::log(f.max-f.min+1)).epsilon(1e-12));
  }
  CHECK(std::isnan(cppunivariate::dunif_mle({x.data(),x.size()},1).logLik));
  CHECK(std::isnan(cppunivariate::dunif_mle({x.data(),x.size()},nan,2).logLik));
  CHECK(std::isnan(cppunivariate::dunif_mle({x.data(),x.size()},-.5).logLik));
}


TEST_CASE("dunif: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // a=mpf(2.0); b=mpf(10.0)
  // d=lambda k:1/mp.mpf(b-a+1) if a<=k<=b else mpf(0); S=lambda k:mp.mpf(b-k)/(b-a+1)
  // print(log(d(mpf(6.0)))); print(log(S(mpf(6.0))))
  // lo,hi=0,10
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::ddunif(6.0, 2.0, 10.0, true) == doctest::Approx(-2.1972245773362196).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pdunif(6.0, 2.0, 10.0, false, true) == doctest::Approx(-0.8109302162163288).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qdunif(-10000, 2.0, 10.0, false, true) == 10.0);
  CHECK(cppunivariate::distributions::ddunif(0.5, 2.0, 10.0, false) == 0);
}

TEST_CASE("dunif: tiny lower quantile stays in support") {
  // from mpmath import *; mp.dps=500; print(ceil(mpf(1e-300)*9)+2-1)
  CHECK(cppunivariate::distributions::qdunif(1e-300,2,10,true,false)==2);
}

TEST_CASE("dunif singleton is a point law including RNG endpoints") {
  // min=max=m: P(X=m)=1, F(x)=1{x>=m}, Q(p)=m.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rdunif(rng,3,3)==3);
  for (bool log : {false,true}) {
    CHECK(cppunivariate::distributions::ddunif(3,3,3,log)==(log?0:1));
    for (bool lower : {false,true}) {
      CHECK(cppunivariate::distributions::pdunif(3,3,3,lower,log)==(log?(lower?0:-INFINITY):(lower?1:0)));
      for (double p : {0.,.5,1.})
        CHECK(cppunivariate::distributions::qdunif(log?std::log(p):p,3,3,lower,log)==3);
    }
  }
}

#include "cppunivariate/distributions/dunif/dpqr.hpp"
TEST_CASE("dunif: quantile search guards and representability") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::distributions::qdunif(.5,1e308,std::numeric_limits<double>::max(),true,false),.5,true,
      [](double x) {return cppunivariate::distributions::pdunif(x,1e308,std::numeric_limits<double>::max(),true,false);},false,true,false));
}

TEST_CASE("dunif: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("dunif",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pdunif(x,t[0], t[1],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qdunif(x,t[0], t[1],l,g);});
}

TEST_CASE("dunif: rational jump ties") {
  // Expected indices computed with Fraction in 2026-11-13-qtie-data/exact.py.
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,3.0,true,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,3.0,true,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,3.0,false,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,3.0,false,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,3.0,true,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,3.0,true,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,3.0,false,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,3.0,false,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,3.0,true,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,3.0,true,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,3.0,false,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,3.0,false,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.125,0.0,7.0,true,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-2.0794415416798357,0.0,7.0,true,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.875,0.0,7.0,false,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-0.13353139262452263,0.0,7.0,false,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,7.0,true,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,7.0,true,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,7.0,false,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,7.0,false,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.375,0.0,7.0,true,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-0.9808292530117262,0.0,7.0,true,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.625,0.0,7.0,false,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-0.4700036292457356,0.0,7.0,false,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,7.0,true,false)==3);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,7.0,true,true)==3);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,7.0,false,false)==3);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,7.0,false,true)==3);
  CHECK(cppunivariate::distributions::qdunif(0.625,0.0,7.0,true,false)==4);
  CHECK(cppunivariate::distributions::qdunif(-0.4700036292457356,0.0,7.0,true,true)==4);
  CHECK(cppunivariate::distributions::qdunif(0.375,0.0,7.0,false,false)==4);
  CHECK(cppunivariate::distributions::qdunif(-0.9808292530117262,0.0,7.0,false,true)==4);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,7.0,true,false)==5);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,7.0,true,true)==5);
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,7.0,false,false)==5);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,7.0,false,true)==5);
  CHECK(cppunivariate::distributions::qdunif(0.875,0.0,7.0,true,false)==6);
  CHECK(cppunivariate::distributions::qdunif(-0.13353139262452263,0.0,7.0,true,true)==6);
  CHECK(cppunivariate::distributions::qdunif(0.125,0.0,7.0,false,false)==6);
  CHECK(cppunivariate::distributions::qdunif(-2.0794415416798357,0.0,7.0,false,true)==6);
  CHECK(cppunivariate::distributions::qdunif(0.0625,0.0,15.0,true,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-2.772588722239781,0.0,15.0,true,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.9375,0.0,15.0,false,false)==0);
  CHECK(cppunivariate::distributions::qdunif(-0.06453852113757118,0.0,15.0,false,true)==0);
  CHECK(cppunivariate::distributions::qdunif(0.125,0.0,15.0,true,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-2.0794415416798357,0.0,15.0,true,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.875,0.0,15.0,false,false)==1);
  CHECK(cppunivariate::distributions::qdunif(-0.13353139262452263,0.0,15.0,false,true)==1);
  CHECK(cppunivariate::distributions::qdunif(0.1875,0.0,15.0,true,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-1.6739764335716716,0.0,15.0,true,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.8125,0.0,15.0,false,false)==2);
  CHECK(cppunivariate::distributions::qdunif(-0.2076393647782445,0.0,15.0,false,true)==2);
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,15.0,true,false)==3);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,15.0,true,true)==3);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,15.0,false,false)==3);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,15.0,false,true)==3);
  CHECK(cppunivariate::distributions::qdunif(0.3125,0.0,15.0,true,false)==4);
  CHECK(cppunivariate::distributions::qdunif(-1.1631508098056809,0.0,15.0,true,true)==4);
  CHECK(cppunivariate::distributions::qdunif(0.6875,0.0,15.0,false,false)==4);
  CHECK(cppunivariate::distributions::qdunif(-0.3746934494414107,0.0,15.0,false,true)==4);
  CHECK(cppunivariate::distributions::qdunif(0.375,0.0,15.0,true,false)==5);
  CHECK(cppunivariate::distributions::qdunif(-0.9808292530117262,0.0,15.0,true,true)==5);
  CHECK(cppunivariate::distributions::qdunif(0.625,0.0,15.0,false,false)==5);
  CHECK(cppunivariate::distributions::qdunif(-0.4700036292457356,0.0,15.0,false,true)==5);
  CHECK(cppunivariate::distributions::qdunif(0.4375,0.0,15.0,true,false)==6);
  CHECK(cppunivariate::distributions::qdunif(-0.8266785731844679,0.0,15.0,true,true)==6);
  CHECK(cppunivariate::distributions::qdunif(0.5625,0.0,15.0,false,false)==6);
  CHECK(cppunivariate::distributions::qdunif(-0.5753641449035618,0.0,15.0,false,true)==6);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,15.0,true,false)==7);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,15.0,true,true)==7);
  CHECK(cppunivariate::distributions::qdunif(0.5,0.0,15.0,false,false)==7);
  CHECK(cppunivariate::distributions::qdunif(-0.6931471805599453,0.0,15.0,false,true)==7);
  CHECK(cppunivariate::distributions::qdunif(0.5625,0.0,15.0,true,false)==8);
  CHECK(cppunivariate::distributions::qdunif(-0.5753641449035618,0.0,15.0,true,true)==8);
  CHECK(cppunivariate::distributions::qdunif(0.4375,0.0,15.0,false,false)==8);
  CHECK(cppunivariate::distributions::qdunif(-0.8266785731844679,0.0,15.0,false,true)==8);
  CHECK(cppunivariate::distributions::qdunif(0.625,0.0,15.0,true,false)==9);
  CHECK(cppunivariate::distributions::qdunif(-0.4700036292457356,0.0,15.0,true,true)==9);
  CHECK(cppunivariate::distributions::qdunif(0.375,0.0,15.0,false,false)==9);
  CHECK(cppunivariate::distributions::qdunif(-0.9808292530117262,0.0,15.0,false,true)==9);
  CHECK(cppunivariate::distributions::qdunif(0.6875,0.0,15.0,true,false)==10);
  CHECK(cppunivariate::distributions::qdunif(-0.3746934494414107,0.0,15.0,true,true)==10);
  CHECK(cppunivariate::distributions::qdunif(0.3125,0.0,15.0,false,false)==10);
  CHECK(cppunivariate::distributions::qdunif(-1.1631508098056809,0.0,15.0,false,true)==10);
  CHECK(cppunivariate::distributions::qdunif(0.75,0.0,15.0,true,false)==11);
  CHECK(cppunivariate::distributions::qdunif(-0.2876820724517809,0.0,15.0,true,true)==11);
  CHECK(cppunivariate::distributions::qdunif(0.25,0.0,15.0,false,false)==11);
  CHECK(cppunivariate::distributions::qdunif(-1.3862943611198906,0.0,15.0,false,true)==11);
  CHECK(cppunivariate::distributions::qdunif(0.8125,0.0,15.0,true,false)==12);
  CHECK(cppunivariate::distributions::qdunif(-0.2076393647782445,0.0,15.0,true,true)==12);
  CHECK(cppunivariate::distributions::qdunif(0.1875,0.0,15.0,false,false)==12);
  CHECK(cppunivariate::distributions::qdunif(-1.6739764335716716,0.0,15.0,false,true)==12);
  CHECK(cppunivariate::distributions::qdunif(0.875,0.0,15.0,true,false)==13);
  CHECK(cppunivariate::distributions::qdunif(-0.13353139262452263,0.0,15.0,true,true)==13);
  CHECK(cppunivariate::distributions::qdunif(0.125,0.0,15.0,false,false)==13);
  CHECK(cppunivariate::distributions::qdunif(-2.0794415416798357,0.0,15.0,false,true)==13);
  CHECK(cppunivariate::distributions::qdunif(0.9375,0.0,15.0,true,false)==14);
  CHECK(cppunivariate::distributions::qdunif(-0.06453852113757118,0.0,15.0,true,true)==14);
  CHECK(cppunivariate::distributions::qdunif(0.0625,0.0,15.0,false,false)==14);
  CHECK(cppunivariate::distributions::qdunif(-2.772588722239781,0.0,15.0,false,true)==14);
}

TEST_CASE("dunif: large inverse samples retain the unfuzzed crossing") {
  struct FixedRng {double runif(double,double) {return .5;}} rng;
  const double maximum=std::ldexp(1.,60);
  const double draw=cppunivariate::distributions::rdunif(rng,0,maximum);
  CHECK(draw>9007199254740992.);
  CHECK(cppunivariate::distributions::pdunif(draw,0,maximum,true,false)>=.5);
  CHECK(cppunivariate::distributions::pdunif(std::nextafter(draw,-INFINITY),0,maximum,true,false)<.5);
  CHECK(draw>cppunivariate::distributions::qdunif(.5,0,maximum,true,false));
}
