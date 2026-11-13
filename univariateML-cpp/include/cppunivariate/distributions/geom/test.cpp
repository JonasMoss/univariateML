#include "test_support.hpp"
#include "cppunivariate/distributions/geom/mle.hpp"
#include "cppunivariate/distributions/geom/dpqr.hpp"
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
 TEST_CASE("geom") {
  auto g = mltest::load_distribution_golden("geom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_geom).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_geom).scale(0));
}

TEST_CASE("geom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == 1.0);
  CHECK(r.logLik == 0.0);
  CHECK(cppunivariate::special::dgeom(1.0, 1.0, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("geom dpqr: closed-form CDF") {
  // F(0; prob=0.5) = prob.
  CHECK(dist::pgeom(0.0, 0.5, true, false) == doctest::Approx(0.5));
  // P(X = 0) = prob.
  CHECK(dist::dgeom(0.0, 0.5, false) == doctest::Approx(0.5));
  CHECK(dist::qgeom(0.99, 0.5, true, false) >= 0.0);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("geom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("geom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dgeom(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pgeom(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qgeom(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rgeom(rng, p[0]); });
}

TEST_CASE("geom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("geom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<2;++mask) {
   const auto r=cppunivariate::geom_mle({x.data(),x.size()},(mask & 1)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("geom: P2 held boundary likelihood and domain") {
 const std::vector<int> x{0,2},same{0,0};
 const auto incompatible=cppunivariate::geom_mle({x.data(),x.size()},1);
 CHECK_FALSE(std::isfinite(incompatible.logLik));
 const auto valid=cppunivariate::geom_mle({same.data(),same.size()},1);
 CHECK(valid.prob==1);
 CHECK(std::isfinite(valid.logLik));
 const auto invalid=cppunivariate::geom_mle({x.data(),x.size()},-1);
 CHECK_FALSE(std::isfinite(invalid.logLik));
}


TEST_CASE("geom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // p=mpf(0.5)
  // d=lambda k:p*(1-p)**k; S=lambda k:(1-p)**(k+1)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(123.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dgeom(1e+100, 0.5, true) == doctest::Approx(-6.931471805599453e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pgeom(123.0, 0.5, false, true) == doctest::Approx(-85.95025038943322).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qgeom(-10000, 0.5, false, true) == 14426.0);
  CHECK(cppunivariate::distributions::dgeom(0.5, 0.5, false) == 0);
}

TEST_CASE("geom: exact and rounded half probabilities") {
  // mp.dps=100; print(mpf(-0.6931471805599453)-log(mpf('.5')))
  // The input exceeds exact log(1/2), inside the left-continuity band.
  CHECK(cppunivariate::distributions::qgeom(-0.6931471805599453,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.5,0.5,true,false)==0);
}

TEST_CASE("geom: adjacent noninteger at zero has no mass") {
  // mp.dps=100; x=mpf(float.fromhex("0x0.0000000000001p-1022")); print(x==floor(x))
  CHECK(cppunivariate::distributions::dgeom(std::nextafter(0.0,1.0),0.5,false)==0);
}

TEST_CASE("geom: TASK-132 invalid counts and empty input have statuses") {
  const std::vector<int> empty, negative{-1,2}, zero(4,0), one(4,1);
  CHECK(cppunivariate::geom_mle(cppunivariate::ispan{empty.data(),empty.size()}).status == cppunivariate::GeomStatus::InvalidInput);
  CHECK(cppunivariate::geom_mle(cppunivariate::ispan{negative.data(),negative.size()}).status == cppunivariate::GeomStatus::InvalidInput);
  CHECK(cppunivariate::geom_mle(cppunivariate::ispan{zero.data(),zero.size()}).status == cppunivariate::GeomStatus::Ok);
}

#include "cppunivariate/distributions/geom/dpqr.hpp"
TEST_CASE("geom: failed integer inversion reports NaN") {
  CHECK(cppunivariate::distributions::qgeom(.5,std::numeric_limits<double>::denorm_min(),true,false)==INFINITY);
}

TEST_CASE("geom: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("geom",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pgeom(x,t[0],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qgeom(x,t[0],l,g);});
}

TEST_CASE("geom: rational jump ties") {
  // Expected indices computed with Fraction in 2026-11-13-qtie-data/exact.py.
  CHECK(cppunivariate::distributions::qgeom(0.25,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-1.3862943611198906,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.75,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-0.2876820724517809,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.4375,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-0.8266785731844679,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.5625,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-0.5753641449035618,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.578125,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-0.5479651707154474,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.421875,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-0.8630462173553428,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.68359375,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-0.38039147055604844,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.31640625,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-1.1507282898071236,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.7626953125,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-0.2708966557597671,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.2373046875,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-1.4384103622589046,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.822021484375,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-0.19598874755826923,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.177978515625,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-1.7260924347106856,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.86651611328125,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-0.14327457417566036,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qgeom(0.13348388671875,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-2.0137745071624664,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qgeom(0.8998870849609375,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-0.10548598468324917,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qgeom(0.1001129150390625,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-2.301456579614247,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qgeom(0.9249153137207031,0.25,true,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-0.07805309839529426,0.25,true,true)==8);
  CHECK(cppunivariate::distributions::qgeom(0.07508468627929688,0.25,false,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-2.5891386520660284,0.25,false,true)==8);
  CHECK(cppunivariate::distributions::qgeom(0.9436864852905273,0.25,true,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-0.057961281038081174,0.25,true,true)==9);
  CHECK(cppunivariate::distributions::qgeom(0.056313514709472656,0.25,false,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-2.8768207245178092,0.25,false,true)==9);
  CHECK(cppunivariate::distributions::qgeom(0.9577648639678955,0.25,true,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-0.04315297584768019,0.25,true,true)==10);
  CHECK(cppunivariate::distributions::qgeom(0.04223513603210449,0.25,false,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-3.16450279696959,0.25,false,true)==10);
  CHECK(cppunivariate::distributions::qgeom(0.9683236479759216,0.25,true,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-0.03218890050452664,0.25,true,true)==11);
  CHECK(cppunivariate::distributions::qgeom(0.03167635202407837,0.25,false,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-3.4521848694213713,0.25,false,true)==11);
  CHECK(cppunivariate::distributions::qgeom(0.9762427359819412,0.25,true,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-0.024044018591451298,0.25,true,true)==12);
  CHECK(cppunivariate::distributions::qgeom(0.023757264018058777,0.25,false,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-3.739866941873152,0.25,false,true)==12);
  CHECK(cppunivariate::distributions::qgeom(0.9821820519864559,0.25,true,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-0.017978598821841742,0.25,true,true)==13);
  CHECK(cppunivariate::distributions::qgeom(0.017817948013544083,0.25,false,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-4.027549014324933,0.25,false,true)==13);
  CHECK(cppunivariate::distributions::qgeom(0.9866365389898419,0.25,true,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-0.013453555605930045,0.25,true,true)==14);
  CHECK(cppunivariate::distributions::qgeom(0.013363461010158062,0.25,false,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-4.315231086776714,0.25,false,true)==14);
  CHECK(cppunivariate::distributions::qgeom(0.9899774042423815,0.25,true,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-0.01007316011156733,0.25,true,true)==15);
  CHECK(cppunivariate::distributions::qgeom(0.010022595757618546,0.25,false,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-4.602913159228494,0.25,false,true)==15);
  CHECK(cppunivariate::distributions::qgeom(0.9924830531817861,0.25,true,false)==16);
  CHECK(cppunivariate::distributions::qgeom(-0.007545341446381497,0.25,true,true)==16);
  CHECK(cppunivariate::distributions::qgeom(0.00751694681821391,0.25,false,false)==16);
  CHECK(cppunivariate::distributions::qgeom(-4.890595231680276,0.25,false,true)==16);
  CHECK(cppunivariate::distributions::qgeom(0.9943622898863396,0.25,true,false)==17);
  CHECK(cppunivariate::distributions::qgeom(-0.005653661984256992,0.25,true,true)==17);
  CHECK(cppunivariate::distributions::qgeom(0.005637710113660432,0.25,false,false)==17);
  CHECK(cppunivariate::distributions::qgeom(-5.178277304132057,0.25,false,true)==17);
  CHECK(cppunivariate::distributions::qgeom(0.9957717174147547,0.25,true,false)==18);
  CHECK(cppunivariate::distributions::qgeom(-0.0042372470505079324,0.25,true,true)==18);
  CHECK(cppunivariate::distributions::qgeom(0.004228282585245324,0.25,false,false)==18);
  CHECK(cppunivariate::distributions::qgeom(-5.465959376583838,0.25,false,true)==18);
  CHECK(cppunivariate::distributions::qgeom(0.996828788061066,0.25,true,false)==19);
  CHECK(cppunivariate::distributions::qgeom(-0.0031762508873838286,0.25,true,true)==19);
  CHECK(cppunivariate::distributions::qgeom(0.0031712119389339932,0.25,false,false)==19);
  CHECK(cppunivariate::distributions::qgeom(-5.7536414490356185,0.25,false,true)==19);
  CHECK(cppunivariate::distributions::qgeom(0.9976215910457995,0.25,true,false)==20);
  CHECK(cppunivariate::distributions::qgeom(-0.0023812418615434235,0.25,true,true)==20);
  CHECK(cppunivariate::distributions::qgeom(0.002378408954200495,0.25,false,false)==20);
  CHECK(cppunivariate::distributions::qgeom(-6.041323521487399,0.25,false,true)==20);
  CHECK(cppunivariate::distributions::qgeom(0.9982161932843496,0.25,true,false)==21);
  CHECK(cppunivariate::distributions::qgeom(-0.0017853995933889597,0.25,true,true)==21);
  CHECK(cppunivariate::distributions::qgeom(0.0017838067156503712,0.25,false,false)==21);
  CHECK(cppunivariate::distributions::qgeom(-6.32900559393918,0.25,false,true)==21);
  CHECK(cppunivariate::distributions::qgeom(0.9986621449632622,0.25,true,false)==22);
  CHECK(cppunivariate::distributions::qgeom(-0.0013387507637785275,0.25,true,true)==22);
  CHECK(cppunivariate::distributions::qgeom(0.0013378550367377784,0.25,false,false)==22);
  CHECK(cppunivariate::distributions::qgeom(-6.616687666390962,0.25,false,true)==22);
  CHECK(cppunivariate::distributions::qgeom(0.9989966087224467,0.25,true,false)==23);
  CHECK(cppunivariate::distributions::qgeom(-0.0010038950115710057,0.25,true,true)==23);
  CHECK(cppunivariate::distributions::qgeom(0.0010033912775533338,0.25,false,false)==23);
  CHECK(cppunivariate::distributions::qgeom(-6.9043697388427425,0.25,false,true)==23);
  CHECK(cppunivariate::distributions::qgeom(0.999247456541835,0.25,true,false)==24);
  CHECK(cppunivariate::distributions::qgeom(-0.0007528267611339948,0.25,true,true)==24);
  CHECK(cppunivariate::distributions::qgeom(0.0007525434581650003,0.25,false,false)==24);
  CHECK(cppunivariate::distributions::qgeom(-7.192051811294523,0.25,false,true)==24);
  CHECK(cppunivariate::distributions::qgeom(0.5,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-0.6931471805599453,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.5,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-0.6931471805599453,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.75,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-0.2876820724517809,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.25,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-1.3862943611198906,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.875,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-0.13353139262452263,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.125,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-2.0794415416798357,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.9375,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-0.06453852113757118,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.0625,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-2.772588722239781,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.96875,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-0.0317486983145803,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.03125,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-3.4657359027997265,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.984375,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-0.015748356968139168,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.015625,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-4.1588830833596715,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.9921875,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-0.007843177461025893,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qgeom(0.0078125,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-4.852030263919617,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qgeom(0.99609375,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-0.003913899321136329,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qgeom(0.00390625,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-5.545177444479562,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qgeom(0.998046875,0.5,true,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-0.0019550348358033506,0.5,true,true)==8);
  CHECK(cppunivariate::distributions::qgeom(0.001953125,0.5,false,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-6.238324625039508,0.5,false,true)==8);
  CHECK(cppunivariate::distributions::qgeom(0.9990234375,0.5,true,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-0.0009770396478266127,0.5,true,true)==9);
  CHECK(cppunivariate::distributions::qgeom(0.0009765625,0.5,false,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-6.931471805599453,0.5,false,true)==9);
  CHECK(cppunivariate::distributions::qgeom(0.99951171875,0.5,true,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-0.0004884004981088745,0.5,true,true)==10);
  CHECK(cppunivariate::distributions::qgeom(0.00048828125,0.5,false,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-7.6246189861593985,0.5,false,true)==10);
  CHECK(cppunivariate::distributions::qgeom(0.999755859375,0.5,true,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-0.0002441704321739145,0.5,true,true)==11);
  CHECK(cppunivariate::distributions::qgeom(0.000244140625,0.5,false,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-8.317766166719343,0.5,false,true)==11);
  CHECK(cppunivariate::distributions::qgeom(0.9998779296875,0.5,true,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-0.00012207776368698225,0.5,true,true)==12);
  CHECK(cppunivariate::distributions::qgeom(0.0001220703125,0.5,false,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-9.010913347279288,0.5,false,true)==12);
  CHECK(cppunivariate::distributions::qgeom(0.99993896484375,0.5,true,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-6.103701897094392e-05,0.5,true,true)==13);
  CHECK(cppunivariate::distributions::qgeom(6.103515625e-05,0.5,false,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-9.704060527839234,0.5,false,true)==13);
  CHECK(cppunivariate::distributions::qgeom(0.999969482421875,0.5,true,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-3.0518043795761425e-05,0.5,true,true)==14);
  CHECK(cppunivariate::distributions::qgeom(3.0517578125e-05,0.5,false,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-10.39720770839918,0.5,false,true)==14);
  CHECK(cppunivariate::distributions::qgeom(0.9999847412109375,0.5,true,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-1.5258905479006077e-05,0.5,true,true)==15);
  CHECK(cppunivariate::distributions::qgeom(1.52587890625e-05,0.5,false,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-11.090354888959125,0.5,false,true)==15);
  CHECK(cppunivariate::distributions::qgeom(0.9999923706054688,0.5,true,false)==16);
  CHECK(cppunivariate::distributions::qgeom(-7.629423635228488e-06,0.5,true,true)==16);
  CHECK(cppunivariate::distributions::qgeom(7.62939453125e-06,0.5,false,false)==16);
  CHECK(cppunivariate::distributions::qgeom(-11.78350206951907,0.5,false,true)==16);
  CHECK(cppunivariate::distributions::qgeom(0.9999961853027344,0.5,true,false)==17);
  CHECK(cppunivariate::distributions::qgeom(-3.814704541601118e-06,0.5,true,true)==17);
  CHECK(cppunivariate::distributions::qgeom(3.814697265625e-06,0.5,false,false)==17);
  CHECK(cppunivariate::distributions::qgeom(-12.476649250079015,0.5,false,true)==17);
  CHECK(cppunivariate::distributions::qgeom(0.9999980926513672,0.5,true,false)==18);
  CHECK(cppunivariate::distributions::qgeom(-1.9073504518042164e-06,0.5,true,true)==18);
  CHECK(cppunivariate::distributions::qgeom(1.9073486328125e-06,0.5,false,false)==18);
  CHECK(cppunivariate::distributions::qgeom(-13.16979643063896,0.5,false,true)==18);
  CHECK(cppunivariate::distributions::qgeom(0.9999990463256836,0.5,true,false)==19);
  CHECK(cppunivariate::distributions::qgeom(-9.5367477115389e-07,0.5,true,true)==19);
  CHECK(cppunivariate::distributions::qgeom(9.5367431640625e-07,0.5,false,false)==19);
  CHECK(cppunivariate::distributions::qgeom(-13.862943611198906,0.5,false,true)==19);
  CHECK(cppunivariate::distributions::qgeom(0.9999995231628418,0.5,true,false)==20);
  CHECK(cppunivariate::distributions::qgeom(-4.768372718899988e-07,0.5,true,true)==20);
  CHECK(cppunivariate::distributions::qgeom(4.76837158203125e-07,0.5,false,false)==20);
  CHECK(cppunivariate::distributions::qgeom(-14.556090791758852,0.5,false,true)==20);
  CHECK(cppunivariate::distributions::qgeom(0.9999997615814209,0.5,true,false)==21);
  CHECK(cppunivariate::distributions::qgeom(-2.3841860752327643e-07,0.5,true,true)==21);
  CHECK(cppunivariate::distributions::qgeom(2.384185791015625e-07,0.5,false,false)==21);
  CHECK(cppunivariate::distributions::qgeom(-15.249237972318797,0.5,false,true)==21);
  CHECK(cppunivariate::distributions::qgeom(0.9999998807907104,0.5,true,false)==22);
  CHECK(cppunivariate::distributions::qgeom(-1.1920929665620916e-07,0.5,true,true)==22);
  CHECK(cppunivariate::distributions::qgeom(1.1920928955078125e-07,0.5,false,false)==22);
  CHECK(cppunivariate::distributions::qgeom(-15.942385152878742,0.5,false,true)==22);
  CHECK(cppunivariate::distributions::qgeom(0.9999999403953552,0.5,true,false)==23);
  CHECK(cppunivariate::distributions::qgeom(-5.960464655174753e-08,0.5,true,true)==23);
  CHECK(cppunivariate::distributions::qgeom(5.960464477539063e-08,0.5,false,false)==23);
  CHECK(cppunivariate::distributions::qgeom(-16.635532333438686,0.5,false,true)==23);
  CHECK(cppunivariate::distributions::qgeom(0.9999999701976776,0.5,true,false)==24);
  CHECK(cppunivariate::distributions::qgeom(-2.980232283178453e-08,0.5,true,true)==24);
  CHECK(cppunivariate::distributions::qgeom(2.9802322387695312e-08,0.5,false,false)==24);
  CHECK(cppunivariate::distributions::qgeom(-17.328679513998633,0.5,false,true)==24);
  CHECK(cppunivariate::distributions::qgeom(0.75,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-0.2876820724517809,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.25,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qgeom(-1.3862943611198906,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qgeom(0.9375,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-0.06453852113757118,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.0625,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qgeom(-2.772588722239781,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qgeom(0.984375,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-0.015748356968139168,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.015625,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qgeom(-4.1588830833596715,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qgeom(0.99609375,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-0.003913899321136329,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.00390625,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qgeom(-5.545177444479562,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qgeom(0.9990234375,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-0.0009770396478266127,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.0009765625,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qgeom(-6.931471805599453,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qgeom(0.999755859375,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-0.0002441704321739145,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.000244140625,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qgeom(-8.317766166719343,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qgeom(0.99993896484375,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-6.103701897094392e-05,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qgeom(6.103515625e-05,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qgeom(-9.704060527839234,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qgeom(0.9999847412109375,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-1.5258905479006077e-05,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qgeom(1.52587890625e-05,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qgeom(-11.090354888959125,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qgeom(0.9999961853027344,0.75,true,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-3.814704541601118e-06,0.75,true,true)==8);
  CHECK(cppunivariate::distributions::qgeom(3.814697265625e-06,0.75,false,false)==8);
  CHECK(cppunivariate::distributions::qgeom(-12.476649250079015,0.75,false,true)==8);
  CHECK(cppunivariate::distributions::qgeom(0.9999990463256836,0.75,true,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-9.5367477115389e-07,0.75,true,true)==9);
  CHECK(cppunivariate::distributions::qgeom(9.5367431640625e-07,0.75,false,false)==9);
  CHECK(cppunivariate::distributions::qgeom(-13.862943611198906,0.75,false,true)==9);
  CHECK(cppunivariate::distributions::qgeom(0.9999997615814209,0.75,true,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-2.3841860752327643e-07,0.75,true,true)==10);
  CHECK(cppunivariate::distributions::qgeom(2.384185791015625e-07,0.75,false,false)==10);
  CHECK(cppunivariate::distributions::qgeom(-15.249237972318797,0.75,false,true)==10);
  CHECK(cppunivariate::distributions::qgeom(0.9999999403953552,0.75,true,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-5.960464655174753e-08,0.75,true,true)==11);
  CHECK(cppunivariate::distributions::qgeom(5.960464477539063e-08,0.75,false,false)==11);
  CHECK(cppunivariate::distributions::qgeom(-16.635532333438686,0.75,false,true)==11);
  CHECK(cppunivariate::distributions::qgeom(0.9999999850988388,0.75,true,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-1.490116130486996e-08,0.75,true,true)==12);
  CHECK(cppunivariate::distributions::qgeom(1.4901161193847656e-08,0.75,false,false)==12);
  CHECK(cppunivariate::distributions::qgeom(-18.021826694558577,0.75,false,true)==12);
  CHECK(cppunivariate::distributions::qgeom(0.9999999962747097,0.75,true,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-3.725290305400808e-09,0.75,true,true)==13);
  CHECK(cppunivariate::distributions::qgeom(3.725290298461914e-09,0.75,false,false)==13);
  CHECK(cppunivariate::distributions::qgeom(-19.408121055678468,0.75,false,true)==13);
  CHECK(cppunivariate::distributions::qgeom(0.9999999990686774,0.75,true,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-9.313225750491594e-10,0.75,true,true)==14);
  CHECK(cppunivariate::distributions::qgeom(9.313225746154785e-10,0.75,false,false)==14);
  CHECK(cppunivariate::distributions::qgeom(-20.79441541679836,0.75,false,true)==14);
  CHECK(cppunivariate::distributions::qgeom(0.9999999997671694,0.75,true,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-2.328306436809747e-10,0.75,true,true)==15);
  CHECK(cppunivariate::distributions::qgeom(2.3283064365386963e-10,0.75,false,false)==15);
  CHECK(cppunivariate::distributions::qgeom(-22.18070977791825,0.75,false,true)==15);
}

TEST_CASE("geom: large inverse samples retain the unfuzzed crossing") {
  struct FixedRng {double runif(double,double) {return .5;}} rng;
  const double prob=std::ldexp(1.,-60);
  const double draw=cppunivariate::distributions::rgeom(rng,prob);
  CHECK(draw>9007199254740992.);
  CHECK(cppunivariate::distributions::pgeom(draw,prob,true,false)>=.5);
  CHECK(cppunivariate::distributions::pgeom(std::nextafter(draw,-INFINITY),prob,true,false)<.5);
  CHECK(draw>cppunivariate::distributions::qgeom(.5,prob,true,false));
}
