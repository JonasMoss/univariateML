#include "test_support.hpp"
#include "cppunivariate/distributions/binom/mle.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
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
 TEST_CASE("binom") {
  auto g = mltest::load_distribution_golden("binom");
  auto v = as_ints(g.data);
  // size_or_nan: estimate size when NaN. Wrapper passes NaN by default.
  auto r = cppunivariate::binom_mle(as_ispan(v),
                             std::numeric_limits<double>::quiet_NaN(),
                             mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.size == doctest::Approx(g.estimates.at("size")).epsilon(mltest::kGolden_binom).scale(0));
  CHECK(r.prob == doctest::Approx(g.estimates.at("prob")).epsilon(mltest::kGolden_binom).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_binom).scale(0));
}

TEST_CASE("binom probability boundaries") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (const double size : {nan, 5.0}) {
    const std::vector<int> v{5, 5, 5};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, mltest::kFitTol, mltest::kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == 5.0);
    CHECK(r.prob == 1.0);
    CHECK(r.logLik == 0.0);
  }
  for (const double size : {nan, 0.0, 5.0}) {
    const std::vector<int> v{0, 0, 0};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, mltest::kFitTol, mltest::kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == (std::isnan(size) ? 0.0 : size));
    CHECK(r.prob == 0.0);
    CHECK(r.logLik == 0.0);
  }
}

TEST_CASE("binom dpqr: trivial trial size, CDF endpoints") {
  // Bernoulli(0.5): P(X=0) = P(X=1) = 0.5.
  CHECK(dist::dbinom(0.0, 1.0, 0.5, false) == doctest::Approx(0.5));
  CHECK(dist::dbinom(1.0, 1.0, 0.5, false) == doctest::Approx(0.5));
  // CDF endpoint.
  CHECK(dist::pbinom(10.0, 10.0, 0.3, true, false) == doctest::Approx(1.0));
  // Round-trip.
  const double k = dist::qbinom(0.7, 20.0, 0.4, true, false);
  CHECK(dist::pbinom(k, 20.0, 0.4, true, false) >= 0.7);
}

// Independent high-precision oracle; see quality/truth.py.
// Extreme-tail and quantile references are independent of the C++ backend.
TEST_CASE("binom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("binom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbinom(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbinom(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbinom(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbinom(rng, p[0], p[1]); });
}

TEST_CASE("binom: free size brackets the integer profile maximum") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for (auto x : {std::vector<int>{2,6,6},std::vector<int>{1,2,3,4,5},std::vector<int>{1,3,3,4}}) {
   const auto f=cppunivariate::binom_mle(as_ispan(x),nan,1e-10,100);
   REQUIRE(f.status==cppunivariate::BinomStatus::Ok);
   for(int k=*std::max_element(x.begin(),x.end()); k<=500; ++k)
     CHECK(f.logLik>=cppunivariate::binom_mle(as_ispan(x),k,1e-10,100).logLik-1e-10);
 }
 const std::vector<int> x{2,6,6};
 CHECK(cppunivariate::binom_mle(as_ispan(x),nan,1e-10,100).size==13);
}

TEST_CASE("binom: held probability profiles every integer and endpoint") {
 const double nan=std::numeric_limits<double>::quiet_NaN();
 for(auto x:{std::vector<int>{1,2,2,4},std::vector<int>{0,0},std::vector<int>{3,3}}) {
  const auto f=cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,.4);
  REQUIRE(f.status==cppunivariate::BinomStatus::Ok);CHECK(f.prob==.4);
  for(int k=*std::max_element(x.begin(),x.end());k<100;++k) {
   const auto o=cppunivariate::binom_mle({x.data(),x.size()},k,1e-10,100,.4);
   CHECK(f.logLik>=o.logLik-1e-10);
  }
 }
 const std::vector<int> x{2,2};
 CHECK(cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,1).size==2);
 CHECK(cppunivariate::binom_mle({x.data(),x.size()},nan,1e-10,100,0).status==cppunivariate::BinomStatus::InvalidInput);
}


TEST_CASE("binom: extreme tails and discrete support") {
  // mpmath snippet (parameters converted from the binary64 inputs):
  // from mpmath import *; mp.dps=500
  // n=mpf(9.0); p=mpf(0.5)
  // d=lambda k:binomial(n,k)*p**k*(1-p)**(n-k); S=lambda k:fsum(d(j) for j in range(int(k)+1,int(n)+1))
  // print(log(d(mpf(4.0)))); print(log(S(mpf(4.0))))
  // lo,hi=0,9
  // while hi-lo>1:
  //     k=(lo+hi)//2
  //     if log(S(k))>-10000: lo=k
  //     else: hi=k
  // print(hi)
  CHECK(cppunivariate::distributions::dbinom(4.0, 9.0, 0.5, true) == doctest::Approx(-1.4020427180880297).epsilon(1e-10));
  CHECK(cppunivariate::distributions::pbinom(4.0, 9.0, 0.5, false, true) == doctest::Approx(-0.6931471805599453).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qbinom(-10000, 9.0, 0.5, false, true) == 9.0);
  CHECK(cppunivariate::distributions::dbinom(0.5, 9.0, 0.5, false) == 0);
}

TEST_CASE("binom: exact and rounded half probabilities") {
  // mp.dps=100; print(fsum(binomial(9,k) for k in range(5,10))/2**9)
  // Exactly 0.5, including the upper-tail equality at x=4.
  CHECK(cppunivariate::distributions::qbinom(0.5,9,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.6931471805599453,9,0.5,true,true)==4);
}

// TASK-85/87: the free-size search used to hard-fail once its bracket
// reached 1,000,000, regardless of how much of the iteration budget
// (`maxit`) was left. These samples (one 0, v copies of v) push x_bar
// just barely below the population variance -- DeRiggi (1983) still
// guarantees a unique finite maximizer there -- and have a true optimal
// size far past that old cap. See test.R for an independent R-side
// lgamma profile cross-check; here we check self-consistency (status,
// no cap, and the found size beating coarse alternatives) and that the
// iteration budget, not a magnitude, is the only limit.
TEST_CASE("binom: free size has no magnitude cap at very large optimal sizes") {
  for (long long v : {1000LL, 5000LL}) {
    std::vector<int> x;
    x.reserve(static_cast<std::size_t>(v) + 1);
    x.push_back(0);
    for (long long i = 0; i < v; ++i) x.push_back(static_cast<int>(v));
    const auto f = cppunivariate::binom_mle(as_ispan(x), std::numeric_limits<double>::quiet_NaN(), 1e-10, 1000);
    REQUIRE(f.status == cppunivariate::BinomStatus::Ok);
    CHECK(f.size > 1e5);
    // Coarse, far-off alternatives are unambiguously worse (the profile
    // is far too flat near the true optimum itself for an adjacent- or
    // even nearby-integer comparison to be numerically meaningful at
    // this scale; see the report).
    for (double factor : {0.01, 0.1, 0.5, 2.0, 10.0, 100.0}) {
      const double alt_size = std::max<double>(v, std::floor(f.size * factor));
      const auto alt = cppunivariate::binom_mle(as_ispan(x), alt_size, 1e-10, 1000);
      REQUIRE(alt.status == cppunivariate::BinomStatus::Ok);
      CHECK(f.logLik + 1e-6 >= alt.logLik);
    }
    // The pre-TASK-85/87 cap: still dramatically worse once v is large
    // enough that it actually bites.
    if (f.size > 1e6) {
      const auto at_cap = cppunivariate::binom_mle(as_ispan(x), 1e6, 1e-10, 1000);
      REQUIRE(at_cap.status == cppunivariate::BinomStatus::Ok);
      CHECK(f.logLik > at_cap.logLik + 1e-3);
    }
  }
}

TEST_CASE("binom: free size search does not hang at an astronomically large optimum") {
  // v = 1e6 pushes the true optimal size to roughly 3e11: the search
  // must terminate (within a generous but fixed iteration budget -- the
  // bracket doubles, so this is O(log2(size)), never a function of a
  // hardcoded magnitude) and land far above any size the old 1e6 cap
  // could reach.
  const long long v = 1000000LL;
  std::vector<int> x;
  x.reserve(static_cast<std::size_t>(v) + 1);
  x.push_back(0);
  for (long long i = 0; i < v; ++i) x.push_back(static_cast<int>(v));
  const auto f = cppunivariate::binom_mle(as_ispan(x), std::numeric_limits<double>::quiet_NaN(), 1e-10, 1000);
  REQUIRE(f.status == cppunivariate::BinomStatus::Ok);
  CHECK(f.size > 1e8);
  const auto at_cap = cppunivariate::binom_mle(as_ispan(x), 1e6, 1e-10, 1000);
  REQUIRE(at_cap.status == cppunivariate::BinomStatus::Ok);
  CHECK(f.logLik > at_cap.logLik + 1e6);
}

TEST_CASE("binom: free size search budget is iterations, not a magnitude cap") {
  // A tiny iteration budget fails cleanly (IterationFailure, not a
  // wrong answer) rather than silently returning a capped/truncated
  // size; the same sample succeeds once the budget is large enough for
  // its O(log2(size)) requirement, with no change needed to any fixed
  // magnitude.
  const long long v = 5000LL;
  std::vector<int> x;
  x.reserve(static_cast<std::size_t>(v) + 1);
  x.push_back(0);
  for (long long i = 0; i < v; ++i) x.push_back(static_cast<int>(v));
  const auto starved = cppunivariate::binom_mle(as_ispan(x), std::numeric_limits<double>::quiet_NaN(), 1e-10, 2);
  CHECK(starved.status == cppunivariate::BinomStatus::IterationFailure);
  const auto ok = cppunivariate::binom_mle(as_ispan(x), std::numeric_limits<double>::quiet_NaN(), 1e-10, 1000);
  CHECK(ok.status == cppunivariate::BinomStatus::Ok);
}

TEST_CASE("binom attained point sampler ignores zero uniform") {
  // At success probability one the (possibly truncated) law is delta_size.
  struct ZeroRng { double runif(double,double) { return 0; } } rng;
  CHECK(cppunivariate::distributions::rbinom(rng,5,1)==5);
}

TEST_CASE("binom: TASK-132 negative counts never report success") {
  const std::vector<int> data{-1,-2,-3};
  for (double size : {double(NAN),10.0})
    CHECK(cppunivariate::binom_mle(cppunivariate::ispan{data.data(),data.size()},size,1e-8,200).status == cppunivariate::BinomStatus::InvalidInput);
}

TEST_CASE("binom: structured stop payloads") {
  const std::vector<int> x{0,0,6};
  const auto r=cppunivariate::binom_mle({x.data(),x.size()},NAN,1e-8,100);
  REQUIRE(r.status==cppunivariate::BinomStatus::Underdispersed);
  REQUIRE(r.stop_detail.limit_coef.size()==1);
  CHECK(r.stop_detail.limit_coef[0]==2);
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(6*std::log(2.)-6-std::lgamma(7.)));
}

#include "cppunivariate/distributions/binom/dpqr.hpp"
TEST_CASE("binom: quantile search guards and representability") {
  CHECK(mltest::truth_lattice_quantile(cppunivariate::special::binomial_quantile(.5,1e308,.9,true),.5,true,
      [](double x) {return cppunivariate::special::binomial_cdf(x,1e308,.9,true);}));
  CHECK(std::isnan(cppunivariate::distributions::qbinom(.5,1e308,.9,true,false)));
}

TEST_CASE("binom: exact-input density from likelihood seam replay") {
 CHECK(cppunivariate::distributions::dbinom(1000000,1000023.0,0.9999816004231903,true)==doctest::Approx(-3.0226093617274734).epsilon(1e-10));
}

TEST_CASE("binom: held probability large-count logLik uses the accurate density") {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto r=cppunivariate::binom_mle({x.data(),x.size()},NAN,1.490116e-8,100,0.5022);
  REQUIRE(r.status==cppunivariate::BinomStatus::Ok);
  double ll=0;
  for(int v:x)ll+=cppunivariate::distributions::dbinom(v,r.size,r.prob,true);
  CHECK(std::abs(r.logLik-ll)<=1e-10*std::max(1.,std::abs(ll)));
  CHECK(r.logLik==doctest::Approx(-37.389678299088466).epsilon(1e-12));
}

// Independent 100/150-digit PMF sum; also exercises double-only intermediates.
TEST_CASE("binom: large-count free-size final likelihood avoids gamma cancellation") {
  const std::vector<int> x{1000000,1000001,1000003,1000008,1000011};
  const auto r=cppunivariate::binom_mle({x.data(),x.size()},NAN,1e-10,1000);
  REQUIRE(r.status==cppunivariate::BinomStatus::Ok);
  CHECK(r.size==1000023);
  CHECK(r.prob==0.9999816004231903);
  CHECK(r.logLik==doctest::Approx(-14.363696715670825).epsilon(1e-12));
}

TEST_CASE("binom: slow reference discrete quantile round trips on every truth point") {
  mltest::discrete_roundtrips("binom",
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::pbinom(x,t[0], t[1],l,g);},
    [](double x,const std::vector<double>& t,bool l,bool g) {return cppunivariate::distributions::qbinom(x,t[0], t[1],l,g);});
}

TEST_CASE("binom: rational jump ties") {
  // Expected indices computed with Fraction in 2026-11-13-qtie-data/exact.py.
  CHECK(cppunivariate::distributions::qbinom(0.75,1.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.2876820724517809,1.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.25,1.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-1.3862943611198906,1.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.5,1.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.6931471805599453,1.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.5,1.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.6931471805599453,1.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.25,1.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-1.3862943611198906,1.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.75,1.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.2876820724517809,1.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.5625,2.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.5753641449035618,2.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.4375,2.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.8266785731844679,2.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9375,2.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.06453852113757118,2.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.0625,2.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-2.772588722239781,2.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.25,2.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-1.3862943611198906,2.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.75,2.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.2876820724517809,2.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.75,2.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.2876820724517809,2.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.25,2.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-1.3862943611198906,2.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.0625,2.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-2.772588722239781,2.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9375,2.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.06453852113757118,2.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.4375,2.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.8266785731844679,2.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.5625,2.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.5753641449035618,2.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.1001129150390625,8.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-2.301456579614247,8.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.8998870849609375,8.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.10548598468324917,8.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.3670806884765625,8.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-1.0021735954839865,8.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.6329193115234375,8.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.4574123349101212,8.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.6785430908203125,8.0,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-0.38780729277715553,8.0,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.3214569091796875,8.0,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-1.1348917748326546,8.0,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.8861846923828125,8.0,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.12082989368706583,8.0,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.1138153076171875,8.0,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-2.173178253003962,8.0,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.9727020263671875,8.0,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.027677485869282956,8.0,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.0272979736328125,8.0,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-3.600942805450406,8.0,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.9957733154296875,8.0,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.004235642251312865,8.0,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.0042266845703125,8.0,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-5.466337382771786,8.0,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.9996185302734375,8.0,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.00038154250464765446,8.0,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.0003814697265625,8.0,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-7.8714790640909245,8.0,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.9999847412109375,8.0,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-1.5258905479006077e-05,8.0,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(1.52587890625e-05,8.0,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-11.090354888959125,8.0,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.00390625,8.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-5.545177444479562,8.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.99609375,8.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.003913899321136329,8.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.03515625,8.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-3.347952867143343,8.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.96484375,8.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.03578910785158528,8.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.14453125,8.0,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-1.934259531835338,8.0,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.85546875,8.0,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-0.15610571466306167,8.0,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.36328125,8.0,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-1.0125779513263065,8.0,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.63671875,8.0,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.4514272436728001,8.0,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.63671875,8.0,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.4514272436728001,8.0,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.36328125,8.0,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-1.0125779513263065,8.0,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.85546875,8.0,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.15610571466306167,8.0,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.14453125,8.0,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-1.934259531835338,8.0,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.96484375,8.0,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.03578910785158528,8.0,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.03515625,8.0,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-3.347952867143343,8.0,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.99609375,8.0,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.003913899321136329,8.0,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.00390625,8.0,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-5.545177444479562,8.0,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(1.52587890625e-05,8.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-11.090354888959125,8.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9999847412109375,8.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-1.5258905479006077e-05,8.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.0003814697265625,8.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-7.8714790640909245,8.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.9996185302734375,8.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.00038154250464765446,8.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.0042266845703125,8.0,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-5.466337382771786,8.0,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.9957733154296875,8.0,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-0.004235642251312865,8.0,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.0272979736328125,8.0,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-3.600942805450406,8.0,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.9727020263671875,8.0,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.027677485869282956,8.0,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.1138153076171875,8.0,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-2.173178253003962,8.0,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.8861846923828125,8.0,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.12082989368706583,8.0,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.3214569091796875,8.0,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-1.1348917748326546,8.0,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.6785430908203125,8.0,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.38780729277715553,8.0,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.6329193115234375,8.0,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.4574123349101212,8.0,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.3670806884765625,8.0,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-1.0021735954839865,8.0,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.8998870849609375,8.0,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.10548598468324917,8.0,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.1001129150390625,8.0,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-2.301456579614247,8.0,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.010022595757618546,16.0,0.25,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-4.602913159228494,16.0,0.25,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9899774042423815,16.0,0.25,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-0.01007316011156733,16.0,0.25,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.0634764397982508,16.0,0.25,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-2.757086468730164,16.0,0.25,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.9365235602017492,16.0,0.25,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.0655805996976802,16.0,0.25,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.19711104989983141,16.0,0.25,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-1.6239880039908852,16.0,0.25,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.8028889501001686,16.0,0.25,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-0.21953886837198094,16.0,0.25,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.4049871100578457,16.0,0.25,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.903900039399753,16.0,0.25,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.5950128899421543,16.0,0.25,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.5191722099028367,16.0,0.25,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.6301861752290279,16.0,0.25,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.461739987016329,16.0,0.25,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.36981382477097213,16.0,0.25,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.9947555762742104,16.0,0.25,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.8103454273659736,16.0,0.25,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.21029466868288002,16.0,0.25,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.1896545726340264,16.0,0.25,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-1.6625509002281829,16.0,0.25,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.9204427481163293,16.0,0.25,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.08290047674917624,16.0,0.25,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.07955725188367069,16.0,0.25,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-2.531278367024927,16.0,0.25,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.9728700437117368,16.0,0.25,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.0275047681914843,16.0,0.25,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.0271299562882632,16.0,0.25,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-3.607116763508201,16.0,0.25,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.9925302795600146,16.0,0.25,true,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-0.007497758513351829,16.0,0.25,true,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.0074697204399853945,16.0,0.25,false,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-4.8968977049037985,16.0,0.25,false,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.998355534626171,16.0,0.25,true,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-0.0016458189912000078,16.0,0.25,true,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.0016444653738290071,16.0,0.25,false,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-6.4103399483008285,16.0,0.25,false,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.9997147608082741,16.0,0.25,true,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-0.0002852798801615733,16.0,0.25,true,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.0002852391917258501,16.0,0.25,false,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-8.162182460471591,16.0,0.25,false,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.9999618928413838,16.0,0.25,true,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-3.810788471240045e-05,16.0,0.25,true,true)==11);
  CHECK(cppunivariate::distributions::qbinom(3.810715861618519e-05,16.0,0.25,false,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-10.175108403292182,16.0,0.25,false,true)==11);
  CHECK(cppunivariate::distributions::qbinom(0.9999962167348713,16.0,0.25,true,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-3.783272285297295e-06,16.0,0.25,true,true)==12);
  CHECK(cppunivariate::distributions::qbinom(3.7832651287317276e-06,16.0,0.25,false,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-12.484923130515474,16.0,0.25,false,true)==12);
  CHECK(cppunivariate::distributions::qbinom(0.9999997371342033,16.0,0.25,true,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-2.628658312344384e-07,16.0,0.25,true,true)==13);
  CHECK(cppunivariate::distributions::qbinom(2.628657966852188e-07,16.0,0.25,false,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-15.151622213768588,16.0,0.25,false,true)==13);
  CHECK(cppunivariate::distributions::qbinom(0.9999999885912985,16.0,0.25,true,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-1.1408701604118847e-08,16.0,0.25,true,true)==14);
  CHECK(cppunivariate::distributions::qbinom(1.1408701539039612e-08,16.0,0.25,false,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-18.288889479807622,16.0,0.25,false,true)==14);
  CHECK(cppunivariate::distributions::qbinom(0.9999999997671694,16.0,0.25,true,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-2.328306436809747e-10,16.0,0.25,true,true)==15);
  CHECK(cppunivariate::distributions::qbinom(2.3283064365386963e-10,16.0,0.25,false,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-22.18070977791825,16.0,0.25,false,true)==15);
  CHECK(cppunivariate::distributions::qbinom(1.52587890625e-05,16.0,0.5,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-11.090354888959125,16.0,0.5,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9999847412109375,16.0,0.5,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-1.5258905479006077e-05,16.0,0.5,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.0002593994140625,16.0,0.5,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-8.25714154490291,16.0,0.5,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.9997406005859375,16.0,0.5,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-0.0002594330639098009,16.0,0.5,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.0020904541015625,16.0,0.5,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-6.170373963131,16.0,0.5,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.9979095458984375,16.0,0.5,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-0.002092642150613748,16.0,0.5,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.0106353759765625,16.0,0.5,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-4.543569478198601,16.0,0.5,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.9893646240234375,16.0,0.5,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-0.010692335807096885,16.0,0.5,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.0384063720703125,16.0,0.5,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-3.259531893823809,16.0,0.5,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.9615936279296875,16.0,0.5,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-0.0391633417521766,16.0,0.5,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.1050567626953125,16.0,0.5,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-2.2532544777963697,16.0,0.5,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.8949432373046875,16.0,0.5,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.1109949847244779,16.0,0.5,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.2272491455078125,16.0,0.5,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-1.4817083060732574,16.0,0.5,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.7727508544921875,16.0,0.5,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.2577985921896266,16.0,0.5,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.4018096923828125,16.0,0.5,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.9117767044860402,16.0,0.5,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.5981903076171875,16.0,0.5,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.513846335496644,16.0,0.5,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.5981903076171875,16.0,0.5,true,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-0.513846335496644,16.0,0.5,true,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.4018096923828125,16.0,0.5,false,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-0.9117767044860402,16.0,0.5,false,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.7727508544921875,16.0,0.5,true,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-0.2577985921896266,16.0,0.5,true,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.2272491455078125,16.0,0.5,false,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-1.4817083060732574,16.0,0.5,false,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.8949432373046875,16.0,0.5,true,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-0.1109949847244779,16.0,0.5,true,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.1050567626953125,16.0,0.5,false,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-2.2532544777963697,16.0,0.5,false,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.9615936279296875,16.0,0.5,true,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-0.0391633417521766,16.0,0.5,true,true)==11);
  CHECK(cppunivariate::distributions::qbinom(0.0384063720703125,16.0,0.5,false,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-3.259531893823809,16.0,0.5,false,true)==11);
  CHECK(cppunivariate::distributions::qbinom(0.9893646240234375,16.0,0.5,true,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-0.010692335807096885,16.0,0.5,true,true)==12);
  CHECK(cppunivariate::distributions::qbinom(0.0106353759765625,16.0,0.5,false,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-4.543569478198601,16.0,0.5,false,true)==12);
  CHECK(cppunivariate::distributions::qbinom(0.9979095458984375,16.0,0.5,true,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-0.002092642150613748,16.0,0.5,true,true)==13);
  CHECK(cppunivariate::distributions::qbinom(0.0020904541015625,16.0,0.5,false,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-6.170373963131,16.0,0.5,false,true)==13);
  CHECK(cppunivariate::distributions::qbinom(0.9997406005859375,16.0,0.5,true,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-0.0002594330639098009,16.0,0.5,true,true)==14);
  CHECK(cppunivariate::distributions::qbinom(0.0002593994140625,16.0,0.5,false,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-8.25714154490291,16.0,0.5,false,true)==14);
  CHECK(cppunivariate::distributions::qbinom(0.9999847412109375,16.0,0.5,true,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-1.5258905479006077e-05,16.0,0.5,true,true)==15);
  CHECK(cppunivariate::distributions::qbinom(1.52587890625e-05,16.0,0.5,false,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-11.090354888959125,16.0,0.5,false,true)==15);
  CHECK(cppunivariate::distributions::qbinom(2.3283064365386963e-10,16.0,0.75,true,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-22.18070977791825,16.0,0.75,true,true)==0);
  CHECK(cppunivariate::distributions::qbinom(0.9999999997671694,16.0,0.75,false,false)==0);
  CHECK(cppunivariate::distributions::qbinom(-2.328306436809747e-10,16.0,0.75,false,true)==0);
  CHECK(cppunivariate::distributions::qbinom(1.1408701539039612e-08,16.0,0.75,true,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-18.288889479807622,16.0,0.75,true,true)==1);
  CHECK(cppunivariate::distributions::qbinom(0.9999999885912985,16.0,0.75,false,false)==1);
  CHECK(cppunivariate::distributions::qbinom(-1.1408701604118847e-08,16.0,0.75,false,true)==1);
  CHECK(cppunivariate::distributions::qbinom(2.628657966852188e-07,16.0,0.75,true,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-15.151622213768588,16.0,0.75,true,true)==2);
  CHECK(cppunivariate::distributions::qbinom(0.9999997371342033,16.0,0.75,false,false)==2);
  CHECK(cppunivariate::distributions::qbinom(-2.628658312344384e-07,16.0,0.75,false,true)==2);
  CHECK(cppunivariate::distributions::qbinom(3.7832651287317276e-06,16.0,0.75,true,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-12.484923130515474,16.0,0.75,true,true)==3);
  CHECK(cppunivariate::distributions::qbinom(0.9999962167348713,16.0,0.75,false,false)==3);
  CHECK(cppunivariate::distributions::qbinom(-3.783272285297295e-06,16.0,0.75,false,true)==3);
  CHECK(cppunivariate::distributions::qbinom(3.810715861618519e-05,16.0,0.75,true,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-10.175108403292182,16.0,0.75,true,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.9999618928413838,16.0,0.75,false,false)==4);
  CHECK(cppunivariate::distributions::qbinom(-3.810788471240045e-05,16.0,0.75,false,true)==4);
  CHECK(cppunivariate::distributions::qbinom(0.0002852391917258501,16.0,0.75,true,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-8.162182460471591,16.0,0.75,true,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.9997147608082741,16.0,0.75,false,false)==5);
  CHECK(cppunivariate::distributions::qbinom(-0.0002852798801615733,16.0,0.75,false,true)==5);
  CHECK(cppunivariate::distributions::qbinom(0.0016444653738290071,16.0,0.75,true,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-6.4103399483008285,16.0,0.75,true,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.998355534626171,16.0,0.75,false,false)==6);
  CHECK(cppunivariate::distributions::qbinom(-0.0016458189912000078,16.0,0.75,false,true)==6);
  CHECK(cppunivariate::distributions::qbinom(0.0074697204399853945,16.0,0.75,true,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-4.8968977049037985,16.0,0.75,true,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.9925302795600146,16.0,0.75,false,false)==7);
  CHECK(cppunivariate::distributions::qbinom(-0.007497758513351829,16.0,0.75,false,true)==7);
  CHECK(cppunivariate::distributions::qbinom(0.0271299562882632,16.0,0.75,true,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-3.607116763508201,16.0,0.75,true,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.9728700437117368,16.0,0.75,false,false)==8);
  CHECK(cppunivariate::distributions::qbinom(-0.0275047681914843,16.0,0.75,false,true)==8);
  CHECK(cppunivariate::distributions::qbinom(0.07955725188367069,16.0,0.75,true,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-2.531278367024927,16.0,0.75,true,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.9204427481163293,16.0,0.75,false,false)==9);
  CHECK(cppunivariate::distributions::qbinom(-0.08290047674917624,16.0,0.75,false,true)==9);
  CHECK(cppunivariate::distributions::qbinom(0.1896545726340264,16.0,0.75,true,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-1.6625509002281829,16.0,0.75,true,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.8103454273659736,16.0,0.75,false,false)==10);
  CHECK(cppunivariate::distributions::qbinom(-0.21029466868288002,16.0,0.75,false,true)==10);
  CHECK(cppunivariate::distributions::qbinom(0.36981382477097213,16.0,0.75,true,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-0.9947555762742104,16.0,0.75,true,true)==11);
  CHECK(cppunivariate::distributions::qbinom(0.6301861752290279,16.0,0.75,false,false)==11);
  CHECK(cppunivariate::distributions::qbinom(-0.461739987016329,16.0,0.75,false,true)==11);
  CHECK(cppunivariate::distributions::qbinom(0.5950128899421543,16.0,0.75,true,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-0.5191722099028367,16.0,0.75,true,true)==12);
  CHECK(cppunivariate::distributions::qbinom(0.4049871100578457,16.0,0.75,false,false)==12);
  CHECK(cppunivariate::distributions::qbinom(-0.903900039399753,16.0,0.75,false,true)==12);
  CHECK(cppunivariate::distributions::qbinom(0.8028889501001686,16.0,0.75,true,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-0.21953886837198094,16.0,0.75,true,true)==13);
  CHECK(cppunivariate::distributions::qbinom(0.19711104989983141,16.0,0.75,false,false)==13);
  CHECK(cppunivariate::distributions::qbinom(-1.6239880039908852,16.0,0.75,false,true)==13);
  CHECK(cppunivariate::distributions::qbinom(0.9365235602017492,16.0,0.75,true,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-0.0655805996976802,16.0,0.75,true,true)==14);
  CHECK(cppunivariate::distributions::qbinom(0.0634764397982508,16.0,0.75,false,false)==14);
  CHECK(cppunivariate::distributions::qbinom(-2.757086468730164,16.0,0.75,false,true)==14);
  CHECK(cppunivariate::distributions::qbinom(0.9899774042423815,16.0,0.75,true,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-0.01007316011156733,16.0,0.75,true,true)==15);
  CHECK(cppunivariate::distributions::qbinom(0.010022595757618546,16.0,0.75,false,false)==15);
  CHECK(cppunivariate::distributions::qbinom(-4.602913159228494,16.0,0.75,false,true)==15);
}

TEST_CASE("binom: inverse sampler retains the unfuzzed backend jump") {
  struct FixedRng {
    double runif(double,double) {return .5*(1+32*std::numeric_limits<double>::epsilon());}
  } rng;
  const double p=.5*(1+32*std::numeric_limits<double>::epsilon());
  CHECK(cppunivariate::distributions::qbinom(p,1,.5,true,false)==0);
  CHECK(cppunivariate::distributions::rbinom(rng,1,.5)==1);
}
