#include "test_support.hpp"
#include "cppunivariate/distributions/nbinom/mle.hpp"
#include "cppunivariate/distributions/nbinom/dpqr.hpp"
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
 TEST_CASE("nbinom") {
  auto g = mltest::load_distribution_golden("nbinom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == doctest::Approx(g.estimates.at("size")).epsilon(mltest::kGolden_nbinom).scale(0));
  CHECK(r.prob == doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_nbinom).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_nbinom).scale(0));
}

TEST_CASE("nbinom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::nbinom_mle(as_ispan(v), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == 0.0);
  CHECK(r.prob == .5);
  CHECK(r.logLik == 0.0);
  // Shared special/dpqr size-zero support belongs to TASK-78. Preserve
  // density coverage on the equally valid held-positive-size member.
  const auto held=cppunivariate::nbinom_mle(as_ispan(v),mltest::kFitTol,mltest::kFitMaxit,1);
  CHECK(cppunivariate::special::dnbinom(0.0, held.size, held.prob, true) == 0.0);
  CHECK(cppunivariate::special::dnbinom(1.0, held.size, held.prob, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("nbinom dpqr: matches dnbinom from special::") {
  CHECK(dist::dnbinom(0.0, 5.0, 0.5, false) > 0.0);
  // CDF round-trip.
  const double k = dist::qnbinom(0.6, 5.0, 0.5, true, false);
  CHECK(dist::pnbinom(k, 5.0, 0.5, true, false) >= 0.6);
  CHECK(dist::pnbinom(k - 1.0, 5.0, 0.5, true, false) < 0.6);
}

TEST_CASE("nbinom: underdispersion status") {
  const std::vector<int> x{1, 1, 1, 1};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::NbinomStatus::Underdispersed);
  CHECK(r.underdispersed);
}

TEST_CASE("nbinom: variance equal to the mean is the Poisson face") {
  // Population variance 2 equals the mean 2: no finite MLE.
  const std::vector<int> x{0, 1, 2, 3, 4};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::NbinomStatus::Underdispersed);
}

TEST_CASE("nbinom: bracketed fallback when Newton leaves the domain") {
  std::vector<int> x(9, 0);
  x.push_back(1000);
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), mltest::kFitTol, mltest::kFitMaxit);
  REQUIRE(r.status == cppunivariate::NbinomStatus::Ok);
  REQUIRE(std::isfinite(r.size));
  // Profile score at the returned size is zero to rounding.
  double score = -10.0 * cppunivariate::special::digamma(r.size) -
                 10.0 * std::log1p(100.0 / r.size);
  for (int v : x) score += cppunivariate::special::digamma(r.size + v);
  CHECK(std::abs(score) < 1e-8);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("nbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("nbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dnbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pnbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qnbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rnbinom(rng, p[0], p[1]); });
}

TEST_CASE("nbinom: P2 every hold subset preserves coefficients and golden likelihood") {
 const auto g=mltest::load_distribution_golden("nbinom");
 std::vector<int> x;for(double v:g.data)x.push_back(static_cast<int>(v));
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(unsigned mask=1;mask<4;++mask) {
   const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,(mask & 1)?g.estimates.at("size"):nan,(mask & 2)?g.estimates.at("prob"):nan);
   REQUIRE(std::isfinite(r.logLik));
   CHECK(r.logLik>=g.logLik-1e-6*x.size());
   if(mask & 1)CHECK(r.size==g.estimates.at("size"));
   if(mask & 2)CHECK(r.prob==g.estimates.at("prob"));
 }
}

TEST_CASE("nbinom: P2 held-domain and non-existence statuses") {
 const std::vector<int> x{0,1,5},z{0,0};
 const double nan=std::numeric_limits<double>::quiet_NaN();
 CHECK(cppunivariate::nbinom_mle({z.data(),z.size()},1e-10,200,nan,.4).status==cppunivariate::NbinomStatus::Ok);
 CHECK(cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,1).status==cppunivariate::NbinomStatus::InvalidInput);
 const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,.4);
 double score=3*std::log(.4);
 for(int v:x)for(int j=0;j<v;++j)score+=1/(r.size+j);
 CHECK(std::abs(score)<1e-10);
}

TEST_CASE("nbinom: P2 held probability near one has a stable likelihood") {
 const std::vector<int> x{0,1,5};
 const double p=1-1e-12,nan=std::numeric_limits<double>::quiet_NaN();
 const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1e-10,200,nan,p);
 CHECK(r.prob==p);
 double ll=3*r.size*std::log(p)-cppunivariate::special::lgamma(6.0);
 ll+=6*std::log(r.size*(1-p));
 for(int j=0;j<5;++j)ll+=std::log1p(j/r.size);
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-13));
 CHECK(r.logLik==doctest::Approx(-6+6*std::log(2.0)-cppunivariate::special::lgamma(6.0)).epsilon(1e-10));
}


TEST_CASE("nbinom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // s=mpf(10.0); p=mpf(0.3)
  // d=lambda k:gamma(k+s)/(gamma(s)*gamma(k+1))*p**s*(1-p)**k; S=lambda k:betainc(k+1,s,0,1-p,regularized=True)
  // print(log(d(mpf(1e+100)))); print(log(S(mpf(174.0))))
  // lo,hi=0,1
  // while log(S(hi))>-10000: hi*=2
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dnbinom(1e+100, 10.0, 0.3, true) == doctest::Approx(-3.5667494393873236e+99).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pnbinom(174.0, 10.0, 0.3, false, true) == doctest::Approx(-39.195008524923715).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qnbinom(-10000, 10.0, 0.3, false, true) == 28229.0);
  CHECK(cppunivariate::distributions::dnbinom(0.5, 10.0, 0.3, false) == 0);
}

TEST_CASE("nbinom: adjacent noninteger at zero has no mass") {
  // mp.dps=100; x=mpf(float.fromhex("0x0.0000000000001p-1022")); print(x==floor(x))
  CHECK(cppunivariate::distributions::dnbinom(std::nextafter(0.0,1.0),10,0.3,false)==0);
}

TEST_CASE("nbinom: P3 attained size zero preserves holds") {
 const std::vector<int> z{0,0,0};
 const auto r=cppunivariate::nbinom_mle({z.data(),z.size()},1e-10,200,0,.4);
 CHECK(r.status==cppunivariate::NbinomStatus::Ok);
 CHECK(r.size==0); CHECK(r.prob==.4); CHECK(r.logLik==0);
}

TEST_CASE("nbinom size zero is delta zero") {
  // (prob/(1-(1-prob)*z))^size -> 1: point mass at zero.
  cppunivariate::distributions::StdRng<> rng(42);
  for (bool log : {false,true}) {
    CHECK(cppunivariate::special::dnbinom(0,0,.4,log)==(log?0:1));
    for (double x : {-1.,0.,1.,cppunivariate::special::kPosInf}) {
      CHECK(dist::dnbinom(x,0,.4,log)==(log?(x==0?0:-INFINITY):(x==0?1:0)));
      for (bool lower : {false,true}) {
        const bool one=lower ? x>=0 : x<0;
        CHECK(dist::pnbinom(x,0,.4,lower,log)==(log?(one?0:-INFINITY):(one?1:0)));
      }
    }
    for (bool lower : {false,true}) for (double p : {0.,.5,1.})
      CHECK(dist::qnbinom(log?std::log(p):p,0,.4,lower,log)==0);
  }
  for (int i=0;i<10;++i) CHECK(dist::rnbinom(rng,0,.4)==0);
}

TEST_CASE("nbinom core preserves adapter held-size precision rejection") {
  const std::vector<int> x{1,2,3};
  const auto r=cppunivariate::nbinom_mle(cppunivariate::ispan{x.data(),x.size()},1e-8,100,1e30);
  CHECK(r.status==cppunivariate::NbinomStatus::PrecisionFailure);
}

TEST_CASE("nbinom: unresolved overdispersed fallback is not underdispersion") {
  const std::vector<int> x{0, 0, 1, 3, 6};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), 1e-12, 1);
  CHECK(r.status != cppunivariate::NbinomStatus::Underdispersed);
}

TEST_CASE("nbinom: centered variance avoids false underdispersion for large counts") {
  const int k = 9743;
  const int m = k * k - 1;
  const std::vector<int> x{m - k, m + k};
  const auto r = cppunivariate::nbinom_mle(as_ispan(x), 1e-8, 1);
  CHECK(r.status == cppunivariate::NbinomStatus::PrecisionFailure);
}

TEST_CASE("nbinom certificate: Poisson stop retains the limit fit") {
  const std::vector<int> x{0,1,2,3,4};
  const auto r=cppunivariate::nbinom_mle(as_ispan(x),1e-10,100);
  REQUIRE(r.status==cppunivariate::NbinomStatus::Underdispersed);
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_coef[0]==2);
  double ll=0;for(int v:x)ll+=v*std::log(2.)-2-cppunivariate::special::lgamma(v+1.);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(ll));
}

TEST_CASE("nbinom certificate: one solver iteration still permits 128 certificate evaluations") {
  const std::vector<int> x{0,0,1,3,6};
  const auto r=cppunivariate::nbinom_mle(as_ispan(x),1e-12,1);
  REQUIRE(r.status==cppunivariate::NbinomStatus::Ok);
  const auto reference=cppunivariate::nbinom_mle(as_ispan(x),1e-12,200);
  CHECK(r.size==doctest::Approx(reference.size).epsilon(1e-6));
  CHECK(r.prob==doctest::Approx(reference.prob).epsilon(1e-6));
  CHECK(r.iterations<=128);
}

TEST_CASE("nbinom certificate: large-count expansion agrees with integer recurrence") {
  const std::vector<int> x{9000,11000};
  const auto r=cppunivariate::nbinom_mle(as_ispan(x),1e-12,200);
  REQUIRE(r.status==cppunivariate::NbinomStatus::Ok);
  double score=-2*std::log1p(10000/r.size);
  for(int v:x)for(int j=0;j<v;++j)score+=1/(r.size+j);
  CHECK(std::abs(score)<1e-7);
}

TEST_CASE("nbinom certificate: cancellation-free near-Poisson root") {
  std::vector<int> x;
  for(int i=0;i<999;++i)for(int j=0;j<5;++j)x.push_back(j);
  x.push_back(0);x.push_back(4);
  const auto r=cppunivariate::nbinom_mle(as_ispan(x),1e-10,200);
  REQUIRE(r.status==cppunivariate::NbinomStatus::Ok);
  CHECK(r.size>1000);
}

#include "cppunivariate/distributions/nbinom/dpqr.hpp"
TEST_CASE("nbinom: quantile search guards and representability") {
  CHECK(std::isnan(cppunivariate::special::negative_binomial_quantile(.5,1e308,.1,true)));
  CHECK(std::isnan(cppunivariate::distributions::qnbinom(.5,1e308,.1,true,false)));
}

// Independent exact-binary64 mpmath reference, 80 decimal digits.
TEST_CASE("nbinom: finite-size Poisson face log mass and tiny survival") {
  const double prob=0.9999999900000001;
  CHECK(cppunivariate::distributions::dnbinom(161,1e8,prob,true)==
        doctest::Approx(-661.5661332533377).epsilon(1e-12));
  const double survival=cppunivariate::distributions::pnbinom(161,1e8,prob,false,false);
  CHECK(std::abs(survival/3.010489309529307e-290-1)<1e-10);
}

TEST_CASE("nbinom: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dnbinom(1000001,10.391389312562472,1.0391233533535134e-05,true)==doctest::Approx(-13.571983521811106).epsilon(1e-10));
}

TEST_CASE("nbinom: held probability large-count logLik evaluates the public size") {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto r=cppunivariate::nbinom_mle({x.data(),x.size()},1.490116e-8,100,NAN,0.30610275261187947);
  REQUIRE(r.status==cppunivariate::NbinomStatus::Ok);
  double ll=0;
  for(int v:x)ll+=cppunivariate::distributions::dnbinom(v,r.size,r.prob,true);
  CHECK(std::abs(r.logLik-ll)<=1e-10*std::max(1.,std::abs(ll)));
  CHECK(r.logLik==doctest::Approx(-42.09308040331988).epsilon(1e-12));
}

TEST_CASE("nbinom: diffuse geometric survival needs no positive tail sum") {
  const double lower=cppunivariate::distributions::pnbinom(138155105234.0,1,5e-9,true,true);
  CHECK(std::isfinite(lower));
  CHECK(std::abs(lower/(-9.999999962748696e-301)-1)<1e-10);
}

TEST_CASE("nbinom: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("nbinom",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pnbinom(x,t[0], t[1],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qnbinom(x,t[0], t[1],l,g);});
}

TEST_CASE("nbinom: rational jump ties") {
  // Expected indices computed with Fraction in 2026-11-13-qtie-data/exact.py.
  CHECK(cppunivariate::distributions::qnbinom(0.25,1.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-1.3862943611198906,1.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.75,1.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.2876820724517809,1.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.4375,1.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.8266785731844679,1.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.5625,1.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.5753641449035618,1.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.578125,1.0,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.5479651707154474,1.0,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.421875,1.0,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.8630462173553428,1.0,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.68359375,1.0,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.38039147055604844,1.0,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.31640625,1.0,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-1.1507282898071236,1.0,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.7626953125,1.0,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.2708966557597671,1.0,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.2373046875,1.0,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-1.4384103622589046,1.0,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.822021484375,1.0,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.19598874755826923,1.0,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.177978515625,1.0,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-1.7260924347106856,1.0,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.86651611328125,1.0,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.14327457417566036,1.0,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.13348388671875,1.0,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-2.0137745071624664,1.0,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.8998870849609375,1.0,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.10548598468324917,1.0,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.1001129150390625,1.0,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-2.301456579614247,1.0,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.9249153137207031,1.0,0.25,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.07805309839529426,1.0,0.25,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.07508468627929688,1.0,0.25,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-2.5891386520660284,1.0,0.25,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.9436864852905273,1.0,0.25,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.057961281038081174,1.0,0.25,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.056313514709472656,1.0,0.25,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-2.8768207245178092,1.0,0.25,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.9577648639678955,1.0,0.25,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.04315297584768019,1.0,0.25,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.04223513603210449,1.0,0.25,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-3.16450279696959,1.0,0.25,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.9683236479759216,1.0,0.25,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.03218890050452664,1.0,0.25,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.03167635202407837,1.0,0.25,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-3.4521848694213713,1.0,0.25,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9762427359819412,1.0,0.25,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.024044018591451298,1.0,0.25,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.023757264018058777,1.0,0.25,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-3.739866941873152,1.0,0.25,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9821820519864559,1.0,0.25,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-0.017978598821841742,1.0,0.25,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.017817948013544083,1.0,0.25,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-4.027549014324933,1.0,0.25,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9866365389898419,1.0,0.25,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-0.013453555605930045,1.0,0.25,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.013363461010158062,1.0,0.25,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-4.315231086776714,1.0,0.25,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9899774042423815,1.0,0.25,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-0.01007316011156733,1.0,0.25,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.010022595757618546,1.0,0.25,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-4.602913159228494,1.0,0.25,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.9924830531817861,1.0,0.25,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-0.007545341446381497,1.0,0.25,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.00751694681821391,1.0,0.25,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-4.890595231680276,1.0,0.25,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9943622898863396,1.0,0.25,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-0.005653661984256992,1.0,0.25,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.005637710113660432,1.0,0.25,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-5.178277304132057,1.0,0.25,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9957717174147547,1.0,0.25,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-0.0042372470505079324,1.0,0.25,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.004228282585245324,1.0,0.25,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-5.465959376583838,1.0,0.25,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.996828788061066,1.0,0.25,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-0.0031762508873838286,1.0,0.25,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.0031712119389339932,1.0,0.25,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-5.7536414490356185,1.0,0.25,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.9976215910457995,1.0,0.25,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-0.0023812418615434235,1.0,0.25,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.002378408954200495,1.0,0.25,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-6.041323521487399,1.0,0.25,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9982161932843496,1.0,0.25,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-0.0017853995933889597,1.0,0.25,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.0017838067156503712,1.0,0.25,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-6.32900559393918,1.0,0.25,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.9986621449632622,1.0,0.25,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-0.0013387507637785275,1.0,0.25,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.0013378550367377784,1.0,0.25,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-6.616687666390962,1.0,0.25,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9989966087224467,1.0,0.25,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-0.0010038950115710057,1.0,0.25,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.0010033912775533338,1.0,0.25,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-6.9043697388427425,1.0,0.25,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.999247456541835,1.0,0.25,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-0.0007528267611339948,1.0,0.25,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.0007525434581650003,1.0,0.25,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-7.192051811294523,1.0,0.25,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.5,1.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,1.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.5,1.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,1.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.75,1.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.2876820724517809,1.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.25,1.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-1.3862943611198906,1.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.875,1.0,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.13353139262452263,1.0,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.125,1.0,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-2.0794415416798357,1.0,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.9375,1.0,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.06453852113757118,1.0,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.0625,1.0,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-2.772588722239781,1.0,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.96875,1.0,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.0317486983145803,1.0,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.03125,1.0,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-3.4657359027997265,1.0,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.984375,1.0,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.015748356968139168,1.0,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.015625,1.0,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-4.1588830833596715,1.0,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.9921875,1.0,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.007843177461025893,1.0,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.0078125,1.0,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-4.852030263919617,1.0,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.99609375,1.0,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.003913899321136329,1.0,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.00390625,1.0,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-5.545177444479562,1.0,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.998046875,1.0,0.5,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.0019550348358033506,1.0,0.5,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.001953125,1.0,0.5,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-6.238324625039508,1.0,0.5,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.9990234375,1.0,0.5,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.0009770396478266127,1.0,0.5,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.0009765625,1.0,0.5,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-6.931471805599453,1.0,0.5,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.99951171875,1.0,0.5,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.0004884004981088745,1.0,0.5,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.00048828125,1.0,0.5,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-7.6246189861593985,1.0,0.5,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.999755859375,1.0,0.5,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.0002441704321739145,1.0,0.5,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.000244140625,1.0,0.5,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-8.317766166719343,1.0,0.5,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9998779296875,1.0,0.5,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.00012207776368698225,1.0,0.5,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.0001220703125,1.0,0.5,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-9.010913347279288,1.0,0.5,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.99993896484375,1.0,0.5,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-6.103701897094392e-05,1.0,0.5,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(6.103515625e-05,1.0,0.5,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-9.704060527839234,1.0,0.5,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.999969482421875,1.0,0.5,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-3.0518043795761425e-05,1.0,0.5,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(3.0517578125e-05,1.0,0.5,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-10.39720770839918,1.0,0.5,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9999847412109375,1.0,0.5,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-1.5258905479006077e-05,1.0,0.5,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(1.52587890625e-05,1.0,0.5,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-11.090354888959125,1.0,0.5,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.9999923706054688,1.0,0.5,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-7.629423635228488e-06,1.0,0.5,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(7.62939453125e-06,1.0,0.5,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-11.78350206951907,1.0,0.5,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9999961853027344,1.0,0.5,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-3.814704541601118e-06,1.0,0.5,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(3.814697265625e-06,1.0,0.5,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-12.476649250079015,1.0,0.5,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9999980926513672,1.0,0.5,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-1.9073504518042164e-06,1.0,0.5,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(1.9073486328125e-06,1.0,0.5,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-13.16979643063896,1.0,0.5,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.9999990463256836,1.0,0.5,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-9.5367477115389e-07,1.0,0.5,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(9.5367431640625e-07,1.0,0.5,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-13.862943611198906,1.0,0.5,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.9999995231628418,1.0,0.5,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-4.768372718899988e-07,1.0,0.5,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(4.76837158203125e-07,1.0,0.5,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-14.556090791758852,1.0,0.5,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9999997615814209,1.0,0.5,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-2.3841860752327643e-07,1.0,0.5,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(2.384185791015625e-07,1.0,0.5,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-15.249237972318797,1.0,0.5,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.9999998807907104,1.0,0.5,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-1.1920929665620916e-07,1.0,0.5,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(1.1920928955078125e-07,1.0,0.5,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-15.942385152878742,1.0,0.5,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999403953552,1.0,0.5,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-5.960464655174753e-08,1.0,0.5,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(5.960464477539063e-08,1.0,0.5,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-16.635532333438686,1.0,0.5,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999701976776,1.0,0.5,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-2.980232283178453e-08,1.0,0.5,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(2.9802322387695312e-08,1.0,0.5,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-17.328679513998633,1.0,0.5,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.75,1.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.2876820724517809,1.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.25,1.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-1.3862943611198906,1.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.9375,1.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.06453852113757118,1.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.0625,1.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-2.772588722239781,1.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.984375,1.0,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.015748356968139168,1.0,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.015625,1.0,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-4.1588830833596715,1.0,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.99609375,1.0,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.003913899321136329,1.0,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.00390625,1.0,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-5.545177444479562,1.0,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.9990234375,1.0,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.0009770396478266127,1.0,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.0009765625,1.0,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-6.931471805599453,1.0,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.999755859375,1.0,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.0002441704321739145,1.0,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.000244140625,1.0,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-8.317766166719343,1.0,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.99993896484375,1.0,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-6.103701897094392e-05,1.0,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(6.103515625e-05,1.0,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-9.704060527839234,1.0,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.9999847412109375,1.0,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-1.5258905479006077e-05,1.0,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(1.52587890625e-05,1.0,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-11.090354888959125,1.0,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.9999961853027344,1.0,0.75,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-3.814704541601118e-06,1.0,0.75,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(3.814697265625e-06,1.0,0.75,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-12.476649250079015,1.0,0.75,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.9999990463256836,1.0,0.75,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-9.5367477115389e-07,1.0,0.75,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(9.5367431640625e-07,1.0,0.75,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-13.862943611198906,1.0,0.75,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.9999997615814209,1.0,0.75,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-2.3841860752327643e-07,1.0,0.75,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(2.384185791015625e-07,1.0,0.75,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-15.249237972318797,1.0,0.75,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999403953552,1.0,0.75,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-5.960464655174753e-08,1.0,0.75,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(5.960464477539063e-08,1.0,0.75,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-16.635532333438686,1.0,0.75,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999850988388,1.0,0.75,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-1.490116130486996e-08,1.0,0.75,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(1.4901161193847656e-08,1.0,0.75,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-18.021826694558577,1.0,0.75,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999962747097,1.0,0.75,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-3.725290305400808e-09,1.0,0.75,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(3.725290298461914e-09,1.0,0.75,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-19.408121055678468,1.0,0.75,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999990686774,1.0,0.75,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-9.313225750491594e-10,1.0,0.75,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(9.313225746154785e-10,1.0,0.75,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-20.79441541679836,1.0,0.75,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999997671694,1.0,0.75,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-2.328306436809747e-10,1.0,0.75,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(2.3283064365386963e-10,1.0,0.75,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-22.18070977791825,1.0,0.75,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.0625,2.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-2.772588722239781,2.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.9375,2.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.06453852113757118,2.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.15625,2.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-1.8562979903656263,2.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.84375,2.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.16989903679539747,2.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.26171875,2.0,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-1.3404848250885963,2.0,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.73828125,2.0,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.3034304294199201,2.0,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.3671875,2.0,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-1.0018826622095587,2.0,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.6328125,2.0,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.4575811092471784,2.0,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.466064453125,2.0,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.763431342993596,2.0,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.533935546875,2.0,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.6274801460425758,2.0,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.5550537109375,2.0,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.5886903934542784,2.0,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.4449462890625,2.0,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.8098017028365305,2.0,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.6329193115234375,2.0,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.4574123349101212,2.0,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.3670806884765625,2.0,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-1.0021735954839865,2.0,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.6996612548828125,2.0,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.3571589826625587,2.0,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.3003387451171875,2.0,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-1.2028442909461377,2.0,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.7559747695922852,2.0,0.25,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.2797472769146919,2.0,0.25,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.24402523040771484,2.0,0.25,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-1.4104836557243823,2.0,0.25,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.8029026985168457,2.0,0.25,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.21952174483457895,2.0,0.25,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.1970973014831543,2.0,0.25,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-1.6240577560224412,2.0,0.25,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.8416182398796082,2.0,0.25,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.17242876437279356,2.0,0.25,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.15838176012039185,2.0,0.25,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-1.8427469569872708,2.0,0.25,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.8732945919036865,2.0,0.25,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.13548233229468265,2.0,0.25,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.12670540809631348,2.0,0.25,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-2.0658905083014805,2.0,0.25,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.8990316279232502,2.0,0.25,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.10643706390216454,2.0,0.25,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.1009683720767498,2.0,0.25,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-2.2929479589368267,2.0,0.25,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9198192339390516,2.0,0.25,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-0.08357811309358479,2.0,0.25,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.08018076606094837,2.0,0.25,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-2.523471617548659,2.0,0.25,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9365235602017492,2.0,0.25,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-0.0655805996976802,2.0,0.25,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.0634764397982508,2.0,0.25,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-2.757086468730164,2.0,0.25,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9498870212119073,2.0,0.25,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-0.051412226499791436,2.0,0.25,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.05011297878809273,2.0,0.25,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-2.9934752467943944,2.0,0.25,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.960536029204377,2.0,0.25,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-0.04026378659273424,2.0,0.25,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.03946397079562303,2.0,0.25,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-3.2323671550767434,2.0,0.25,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9689925943748676,2.0,0.25,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-0.03149830966457147,2.0,0.25,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.031007405625132378,2.0,0.25,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-3.4735292118936316,2.0,0.25,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9756873751348394,2.0,0.25,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-0.02461305624185135,2.0,0.25,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.024312624865160615,2.0,0.25,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-3.7167595217745784,2.0,0.25,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.980972728366396,2.0,0.25,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-0.019210619633577616,2.0,0.25,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.01902727163360396,2.0,0.25,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-3.9618819798075635,2.0,0.25,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.9851349440362469,2.0,0.25,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-0.014976648171986651,2.0,0.25,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.014865055963753093,2.0,0.25,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-4.208742057739089,2.0,0.25,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9884052563482726,2.0,0.25,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-0.01166248684431508,2.0,0.25,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.011594743651727413,2.0,0.25,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-4.457203417037589,2.0,0.25,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.99096947850202,2.0,0.25,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-0.009071543812593738,2.0,0.25,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.009030521497980004,2.0,0.25,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-4.7071451615065225,2.0,0.25,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9929762610571267,2.0,0.25,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-0.007048521509605797,2.0,0.25,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.0070237389428733366,2.0,0.25,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-4.958459589787429,2.0,0.25,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.9945440599283037,2.0,0.25,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-0.0054708780713923185,2.0,0.25,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.0054559400716962525,2.0,0.25,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-5.2110503424279395,2.0,0.25,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.25,2.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-1.3862943611198906,2.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.75,2.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.2876820724517809,2.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.5,2.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,2.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.5,2.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,2.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.6875,2.0,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.3746934494414107,2.0,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.3125,2.0,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-1.1631508098056809,2.0,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.8125,2.0,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.2076393647782445,2.0,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.1875,2.0,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-1.6739764335716716,2.0,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.890625,2.0,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.1158318155251217,2.0,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.109375,2.0,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-2.2129729343043585,2.0,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.9375,2.0,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.06453852113757118,2.0,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.0625,2.0,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-2.772588722239781,2.0,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.96484375,2.0,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.03578910785158528,2.0,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.03515625,2.0,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-3.347952867143343,2.0,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.98046875,2.0,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.01972450534777859,2.0,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.01953125,2.0,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-3.9357395320454622,2.0,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.9892578125,2.0,0.5,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.010800301350769714,2.0,0.5,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.0107421875,2.0,0.5,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-4.533576532801082,2.0,0.5,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.994140625,2.0,0.5,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.005876608488985042,2.0,0.5,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.005859375,2.0,0.5,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-5.139712336371398,2.0,0.5,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.996826171875,2.0,0.5,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.00317887539976795,2.0,0.5,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.003173828125,2.0,0.5,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-5.752816809257807,2.0,0.5,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.998291015625,2.0,0.5,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.0017104463547014076,2.0,0.5,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.001708984375,2.0,0.5,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-6.37185601766403,2.0,0.5,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.99908447265625,2.0,0.5,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.0009159466948797313,2.0,0.5,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.00091552734375,2.0,0.5,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-6.996010326737024,2.0,0.5,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.99951171875,2.0,0.5,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-0.0004884004981088745,2.0,0.5,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.00048828125,2.0,0.5,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-7.6246189861593985,2.0,0.5,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9997406005859375,2.0,0.5,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-0.0002594330639098009,2.0,0.5,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.0002593994140625,2.0,0.5,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-8.25714154490291,2.0,0.5,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9998626708984375,2.0,0.5,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-0.00013733853206696634,2.0,0.5,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.0001373291015625,2.0,0.5,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-8.893130311622906,2.0,0.5,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.9999275207519531,2.0,0.5,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-7.248187479449761e-05,2.0,0.5,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(7.2479248046875e-05,2.0,0.5,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-9.532210270912575,2.0,0.5,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9999618530273438,2.0,0.5,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-3.8147700270515666e-05,2.0,0.5,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(3.814697265625e-05,2.0,0.5,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-10.17406415708497,2.0,0.5,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9999799728393555,2.0,0.5,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-2.0027361190790577e-05,2.0,0.5,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(2.002716064453125e-05,2.0,0.5,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-10.818421173475484,2.0,0.5,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.9999895095825195,2.0,0.5,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-1.049047250528303e-05,2.0,0.5,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(1.049041748046875e-05,2.0,0.5,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-11.465048338400535,2.0,0.5,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.9999945163726807,2.0,0.5,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-5.483642354475191e-06,2.0,0.5,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(5.4836273193359375e-06,2.0,0.5,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-12.113743756389647,2.0,0.5,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9999971389770508,2.0,0.5,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-2.8610270419527142e-06,2.0,0.5,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(2.86102294921875e-06,2.0,0.5,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-12.764331322530797,2.0,0.5,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.9999985098838806,2.0,0.5,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-1.490117229608893e-06,2.0,0.5,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(1.4901161193847656e-06,2.0,0.5,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-13.416656508570487,2.0,0.5,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9999992251396179,2.0,0.5,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-7.748606822845391e-07,2.0,0.5,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(7.748603820800781e-07,2.0,0.5,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-14.070582975977151,2.0,0.5,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.9999995976686478,2.0,0.5,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-4.023314331691669e-07,2.0,0.5,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(4.023313522338867e-07,2.0,0.5,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-14.72598982855425,2.0,0.5,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.5625,2.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.5753641449035618,2.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.4375,2.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.8266785731844679,2.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.84375,2.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.16989903679539747,2.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.15625,2.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-1.8562979903656263,2.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.94921875,2.0,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.05211600113901402,2.0,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.05078125,2.0,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-2.9802280870180256,2.0,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.984375,2.0,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.015748356968139168,2.0,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.015625,2.0,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-4.1588830833596715,2.0,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.995361328125,2.0,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.00464946390009031,2.0,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.004638671875,2.0,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-5.373327187552904,2.0,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.9986572265625,2.0,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.001343675765590806,2.0,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.0013427734375,2.0,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-6.613018074480919,2.0,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.9996185302734375,2.0,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.00038154250464765446,2.0,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.0003814697265625,2.0,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-7.8714790640909245,2.0,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.9998931884765625,2.0,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.00010681722819449567,2.0,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.0001068115234375,2.0,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-9.144444739903811,2.0,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.9999704360961914,2.0,0.75,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-2.9564340829411335e-05,2.0,0.75,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(2.956390380859375e-05,2.0,0.75,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-10.42895640671376,2.0,0.75,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.9999918937683105,2.0,0.75,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-8.106264545126783e-06,2.0,0.75,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(8.106231689453125e-06,2.0,0.75,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-11.722877447702636,2.0,0.75,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.9999977946281433,2.0,0.75,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-2.2053742885255416e-06,2.0,0.75,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(2.205371856689453e-06,2.0,0.75,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-13.024614420794464,2.0,0.75,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.9999994039535522,2.0,0.75,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-5.960466253896608e-07,2.0,0.75,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(5.960464477539062e-07,2.0,0.75,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-14.332947240444641,2.0,0.75,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9999998398125172,2.0,0.75,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-1.601874956638785e-07,2.0,0.75,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(1.601874828338623e-07,2.0,0.75,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-15.646920939984906,2.0,0.75,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999571591616,2.0,0.75,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-4.284083934998076e-08,2.0,0.75,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(4.284083843231201e-08,2.0,0.75,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-16.965774020309265,2.0,0.75,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999885912985,2.0,0.75,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-1.1408701604118847e-08,2.0,0.75,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(1.1408701539039612e-08,2.0,0.75,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-18.288889479807622,2.0,0.75,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999969732016,2.0,0.75,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-3.0267983720810594e-09,2.0,0.75,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(3.026798367500305e-09,2.0,0.75,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-19.615760420456713,2.0,0.75,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999991996447,2.0,0.75,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-8.003553378804612e-10,2.0,0.75,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(8.003553375601768e-10,2.0,0.75,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-20.94596531492556,2.0,0.75,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999997889972,2.0,0.75,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-2.1100277083358044e-10,2.0,0.75,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(2.1100277081131935e-10,2.0,0.75,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-22.279149850731503,2.0,0.75,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.00390625,4.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-5.545177444479562,4.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.99609375,4.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.003913899321136329,4.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.015625,4.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-4.1588830833596715,4.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.984375,4.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.015748356968139168,4.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.03759765625,4.0,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-3.2808135643057144,4.0,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.96240234375,4.0,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.0383226790066782,4.0,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.070556640625,4.0,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-2.6513394786069115,4.0,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.929443359375,4.0,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.07316941033684636,4.0,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.1138153076171875,4.0,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-2.173178253003962,4.0,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.8861846923828125,4.0,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.12082989368706583,4.0,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.1657257080078125,4.0,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-1.797421218679089,4.0,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.8342742919921875,4.0,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.18119304341415637,4.0,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.22412490844726562,4.0,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-1.4955517555465359,4.0,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.7758750915527344,4.0,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.2537637362489918,4.0,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.2866954803466797,4.0,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-1.2493346706165154,4.0,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.7133045196533203,4.0,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.3378468534595332,4.0,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.35122138261795044,4.0,0.25,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-1.046338534627091,4.0,0.25,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.6487786173820496,4.0,0.25,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.4326637339033654,4.0,0.25,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.4157472848892212,4.0,0.25,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.877677691563687,4.0,0.25,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.5842527151107788,4.0,0.25,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.5374216584010473,4.0,0.25,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.4786600396037102,4.0,0.25,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.7367646629769635,4.0,0.25,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.5213399603962898,4.0,0.25,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.6513529348681949,4.0,0.25,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.5387131236493587,4.0,0.25,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.6185720878894784,4.0,0.25,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.46128687635064125,4.0,0.25,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.7737351380954518,4.0,0.25,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.5950128899421543,4.0,0.25,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.5191722099028367,4.0,0.25,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.4049871100578457,4.0,0.25,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.903900039399753,4.0,0.25,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.6469819049816579,4.0,0.25,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-0.4354369524431897,4.0,0.25,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.35301809501834214,4.0,0.25,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-1.0412359626874337,4.0,0.25,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.6943108293926343,4.0,0.25,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-0.3648355377748623,4.0,0.25,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.30568917060736567,4.0,0.25,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-1.185186475615129,4.0,0.25,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.7369068613625132,4.0,0.25,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-0.3052937701349944,4.0,0.25,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.26309313863748685,4.0,0.25,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-1.3352471701908621,4.0,0.25,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.7748439523356865,4.0,0.25,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-0.25509362172771094,4.0,0.25,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.22515604766431352,4.0,0.25,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-1.4909615719943032,4.0,0.25,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.8083178561355453,4.0,0.25,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-0.2127999115107802,4.0,0.25,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.1916821438644547,4.0,0.25,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-1.6519167795158953,4.0,0.25,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.8376075219604218,4.0,0.25,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-0.17720563909313689,4.0,0.25,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.16239247803957824,4.0,0.25,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-1.8177391698175294,4.0,0.25,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.8630432843872882,4.0,0.25,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-0.14729043343538972,4.0,0.25,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.13695671561271183,4.0,0.25,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-1.9880903475077298,4.0,0.25,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.8849816294804604,4.0,0.25,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-0.12218839183884776,4.0,0.25,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.11501837051953956,4.0,0.25,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-2.162663419728375,4.0,0.25,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9037859252746081,4.0,0.25,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-0.10116275495802778,4.0,0.25,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.0962140747253919,4.0,0.25,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-2.341179625095534,4.0,0.25,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.9198123137355294,4.0,0.25,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-0.08358563656047793,4.0,0.25,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.08018768626447059,4.0,0.25,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-2.5233853137473043,4.0,0.25,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9333999039523975,4.0,0.25,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-0.06892154836722263,4.0,0.25,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.06660009604760253,4.0,0.25,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-2.7090492592806394,4.0,0.25,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.9448644331978799,4.0,0.25,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-0.056713818712784816,4.0,0.25,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.0551355668021201,4.0,0.25,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-2.897960275564609,4.0,0.25,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.0625,4.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-2.772588722239781,4.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.9375,4.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.06453852113757118,4.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.1875,4.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-1.6739764335716716,4.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.8125,4.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.2076393647782445,4.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.34375,4.0,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-1.067840630001356,4.0,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.65625,4.0,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.42121346507630353,4.0,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.5,4.0,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,4.0,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.5,4.0,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.6931471805599453,4.0,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.63671875,4.0,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.4514272436728001,4.0,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.36328125,4.0,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-1.0125779513263065,4.0,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.74609375,4.0,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.2929040164329326,4.0,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.25390625,4.0,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-1.3707901745839253,4.0,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.828125,4.0,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.18859116980755003,4.0,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.171875,4.0,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-1.7609878105613013,4.0,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.88671875,4.0,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.1202274269981598,4.0,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.11328125,4.0,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-2.1778816144930886,4.0,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.927001953125,4.0,0.5,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.07579960648754139,4.0,0.5,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.072998046875,4.0,0.5,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-2.6173225933286575,4.0,0.5,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.953857421875,4.0,0.5,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.047241071664273125,4.0,0.5,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.046142578125,4.0,0.5,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-3.0760191516597013,4.0,0.5,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.9713134765625,4.0,0.5,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-0.029106023901754194,4.0,0.5,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.0286865234375,4.0,0.5,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-3.55132783313513,4.0,0.5,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.982421875,4.0,0.5,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-0.01773445493976858,4.0,0.5,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.017578125,4.0,0.5,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-4.041100047703289,4.0,0.5,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9893646240234375,4.0,0.5,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-0.010692335807096885,4.0,0.5,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.0106353759765625,4.0,0.5,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-4.543569478198601,4.0,0.5,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9936370849609375,4.0,0.5,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-0.006383244665967956,4.0,0.5,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.0063629150390625,4.0,0.5,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-5.057268667160323,4.0,0.5,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9962310791015625,4.0,0.5,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-0.0037760411769481427,4.0,0.5,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.0037689208984375,4.0,0.5,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-5.580966552331148,4.0,0.5,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9977874755859375,4.0,0.5,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-0.0022149756625026843,4.0,0.5,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.0022125244140625,4.0,0.5,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-6.11362114653855,4.0,0.5,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.9987115859985352,4.0,0.5,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-0.0012892447204010322,4.0,0.5,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.0012884140014648438,4.0,0.5,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-6.654343273238707,4.0,0.5,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9992551803588867,4.0,0.5,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-0.0007450971570702886,4.0,0.5,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.0007448196411132812,4.0,0.5,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-7.20236846135922,4.0,0.5,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9995722770690918,4.0,0.5,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-0.0004278144304529146,4.0,0.5,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.0004277229309082031,4.0,0.5,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-7.757034929700056,4.0,0.5,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.999755859375,4.0,0.5,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-0.0002441704321739145,4.0,0.5,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.000244140625,4.0,0.5,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-8.317766166719343,4.0,0.5,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.9998614192008972,4.0,0.5,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-0.00013859040230894675,4.0,0.5,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.0001385807991027832,4.0,0.5,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-8.88405701541723,4.0,0.5,false,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(0.9999217391014099,4.0,0.5,true,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-7.826396113399796e-05,4.0,0.5,true,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(7.826089859008789e-05,4.0,0.5,false,false)==21);
  CHECK(cppunivariate::distributions::qnbinom(-9.455462459135891,4.0,0.5,false,true)==21);
  CHECK(cppunivariate::distributions::qnbinom(0.9999560117721558,4.0,0.5,true,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-4.398919535470554e-05,4.0,0.5,true,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(4.398822784423828e-05,4.0,0.5,false,false)==22);
  CHECK(cppunivariate::distributions::qnbinom(-10.031588508838215,4.0,0.5,false,true)==22);
  CHECK(cppunivariate::distributions::qnbinom(0.9999753832817078,4.0,0.5,true,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-2.4617021288618595e-05,4.0,0.5,true,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(2.4616718292236328e-05,4.0,0.5,false,false)==23);
  CHECK(cppunivariate::distributions::qnbinom(-10.612084740477655,4.0,0.5,false,true)==23);
  CHECK(cppunivariate::distributions::qnbinom(0.9999862797558308,4.0,0.5,true,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-1.3720338292646192e-05,4.0,0.5,true,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(1.372024416923523e-05,4.0,0.5,false,false)==24);
  CHECK(cppunivariate::distributions::qnbinom(-11.196638139233404,4.0,0.5,false,true)==24);
  CHECK(cppunivariate::distributions::qnbinom(0.31640625,4.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-1.1507282898071236,4.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.68359375,4.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qnbinom(-0.38039147055604844,4.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qnbinom(0.6328125,4.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-0.4575811092471784,4.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.3671875,4.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qnbinom(-1.0018826622095587,4.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qnbinom(0.83056640625,4.0,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-0.18564739376353664,4.0,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.16943359375,4.0,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qnbinom(-1.7752942062125392,4.0,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qnbinom(0.929443359375,4.0,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-0.07316941033684636,4.0,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.070556640625,4.0,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qnbinom(-2.6513394786069115,4.0,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qnbinom(0.9727020263671875,4.0,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-0.027677485869282956,4.0,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.0272979736328125,4.0,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qnbinom(-3.600942805450406,4.0,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qnbinom(0.9900054931640625,4.0,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-0.010044787218326964,4.0,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.0099945068359375,4.0,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qnbinom(-4.605719653323873,4.0,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qnbinom(0.9964942932128906,4.0,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-0.00351186617670394,4.0,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.003505706787109375,4.0,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qnbinom(-5.6533631277233285,4.0,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qnbinom(0.9988117218017578,4.0,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-0.0011889847605642597,4.0,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.0011882781982421875,4.0,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qnbinom(-6.735249911851508,4.0,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qnbinom(0.9996083378791809,4.0,0.75,true,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-0.00039173884046030626,4.0,0.75,true,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.0003916621208190918,4.0,0.75,false,false)==8);
  CHECK(cppunivariate::distributions::qnbinom(-7.8451110265408195,4.0,0.75,false,true)==8);
  CHECK(cppunivariate::distributions::qnbinom(0.9998738765716553,4.0,0.75,true,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-0.000126131382573132,4.0,0.75,true,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.00012612342834472656,4.0,0.75,false,false)==9);
  CHECK(cppunivariate::distributions::qnbinom(-8.978249540460498,4.0,0.75,false,true)==9);
  CHECK(cppunivariate::distributions::qnbinom(0.9999601766467094,4.0,0.75,true,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-3.982414626134409e-05,4.0,0.75,true,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(3.982335329055786e-05,4.0,0.75,false,false)==10);
  CHECK(cppunivariate::distributions::qnbinom(-10.131057051659377,4.0,0.75,false,true)==10);
  CHECK(cppunivariate::distributions::qnbinom(0.9999876357614994,4.0,0.75,true,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-1.2364314938422006e-05,4.0,0.75,true,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(1.2364238500595093e-05,4.0,0.75,false,false)==11);
  CHECK(cppunivariate::distributions::qnbinom(-11.300702243958494,4.0,0.75,false,true)==11);
  CHECK(cppunivariate::distributions::qnbinom(0.9999962167348713,4.0,0.75,true,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-3.783272285297295e-06,4.0,0.75,true,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(3.7832651287317276e-06,4.0,0.75,false,false)==12);
  CHECK(cppunivariate::distributions::qnbinom(-12.484923130515474,4.0,0.75,false,true)==12);
  CHECK(cppunivariate::distributions::qnbinom(0.9999988570343703,4.0,0.75,true,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-1.142966282882559e-06,4.0,0.75,true,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(1.142965629696846e-06,4.0,0.75,false,false)==13);
  CHECK(cppunivariate::distributions::qnbinom(-13.681884243860198,4.0,0.75,false,true)==13);
  CHECK(cppunivariate::distributions::qnbinom(0.9999996585538611,4.0,0.75,true,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-3.41446197211146e-07,4.0,0.75,true,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(3.414461389183998e-07,4.0,0.75,false,false)==14);
  CHECK(cppunivariate::distributions::qnbinom(-14.890075889500745,4.0,0.75,false,true)==14);
  CHECK(cppunivariate::distributions::qnbinom(0.9999998990097083,4.0,0.75,true,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-1.009902967843858e-07,4.0,0.75,true,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(1.0099029168486595e-07,4.0,0.75,false,false)==15);
  CHECK(cppunivariate::distributions::qnbinom(-16.108241446657182,4.0,0.75,false,true)==15);
  CHECK(cppunivariate::distributions::qnbinom(0.999999970395038,4.0,0.75,true,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-2.9604962475637485e-08,4.0,0.75,true,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(2.9604962037410587e-08,4.0,0.75,false,false)==16);
  CHECK(cppunivariate::distributions::qnbinom(-17.3353238532668,4.0,0.75,false,true)==16);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999913907232,4.0,0.75,true,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-8.609276884042363e-09,4.0,0.75,true,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(8.609276846982539e-09,4.0,0.75,false,false)==17);
  CHECK(cppunivariate::distributions::qnbinom(-18.57042551193162,4.0,0.75,false,true)==17);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999975144647,4.0,0.75,true,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-2.4855353361966343e-09,4.0,0.75,true,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(2.4855353331076913e-09,4.0,0.75,false,false)==18);
  CHECK(cppunivariate::distributions::qnbinom(-19.81277777480076,4.0,0.75,false,true)==18);
  CHECK(cppunivariate::distributions::qnbinom(0.9999999992871267,4.0,0.75,true,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-7.128733161874875e-10,4.0,0.75,true,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(7.128733159333933e-10,4.0,0.75,false,false)==19);
  CHECK(cppunivariate::distributions::qnbinom(-21.061717388813545,4.0,0.75,false,true)==19);
  CHECK(cppunivariate::distributions::qnbinom(0.999999999796767,4.0,0.75,true,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-2.0323298601643449e-10,4.0,0.75,true,true)==20);
  CHECK(cppunivariate::distributions::qnbinom(2.0323298599578266e-10,4.0,0.75,false,false)==20);
  CHECK(cppunivariate::distributions::qnbinom(-22.316668080742332,4.0,0.75,false,true)==20);
}
