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
  CHECK(r.min == D_APPROX(g.estimates.at("min")));
  CHECK(r.max == D_APPROX(g.estimates.at("max")));
  CHECK(r.logLik == D_APPROX(g.logLik));
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
  CHECK(std::isnan(cppunivariate::distributions::qdunif(.5,1e308,std::numeric_limits<double>::max(),true,false)));
}
