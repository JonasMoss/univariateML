#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/kumar/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("rkumar samples lie in (0, 1); CDF round-trip at p=0.3") {
  cppunivariate::distributions::StdRng<> rng(137);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rkumar(rng, 2.0, 3.0);
    CHECK(x >= 0.0);
    CHECK(x <= 1.0);
  }
  const double q = cppunivariate::distributions::qkumar(0.3, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pkumar(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.3) < 1e-10);
}

TEST_CASE("kumar") {
  auto g = mltest::load_distribution_golden("kumar");
  auto r = cppunivariate::kumar_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.a == C_APPROX(g.estimates.at("a")));
  CHECK(r.b == C_APPROX(g.estimates.at("b")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("kumar: explicit start agrees with automatic start") {
  const auto g = mltest::load_distribution_golden("kumar");
  const auto x = mltest::as_span(g.data);
  const auto a = cppunivariate::kumar_mle(x, 1e-12, 200);
  const auto b = cppunivariate::kumar_mle(x, 1e-12, 200, 1.0);
  CHECK(b.a == doctest::Approx(a.a).epsilon(1e-9));
  CHECK(b.b == doctest::Approx(a.b).epsilon(1e-9));
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-12));
}

TEST_CASE("kumar clipped extremes: stable profile oracle and honest exhaustion") {
  std::vector<double> x;
  for (int i = 0; i < 100; ++i) {
    x.push_back(1e-12);
    x.push_back(1.0 - 1e-12);
  }
  const auto r = cppunivariate::kumar_mle(mltest::as_span(x), 1e-12, 100);
  CHECK(std::isfinite(r.logLik));
  // Independent high precision profile solution (mpmath).
  CHECK(r.a == doctest::Approx(0.007765975729959528).epsilon(1e-8));
  CHECK(r.logLik == doctest::Approx(4415.965322716401).epsilon(1e-12));
  CHECK(std::isnan(cppunivariate::kumar_mle(mltest::as_span(x), 1e-12, 1).a));
}

TEST_CASE("kumar ordinary golden is unchanged to 1e-12") {
  const auto g = mltest::load_distribution_golden("kumar");
  const auto r = cppunivariate::kumar_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(std::abs(r.a - g.estimates.at("a")) < 1e-12);
  CHECK(std::abs(r.b - g.estimates.at("b")) < 1e-12);
  CHECK(std::abs(r.logLik - g.logLik) < 1e-12);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("kumar: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("kumar",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dkumar(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pkumar(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qkumar(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rkumar(rng, p[0], p[1]); });
}

// from mpmath import mp, log, log1p, exp, expm1, sqrt
// mp.dps=500; v=mp.mpf
// log(-expm1(3*log1p(-v('1e-400'))));
// sqrt(-expm1(log1p(-exp(-1000))/3));
// 3*log1p(-v(float.fromhex('0x1.fffffffffffffp-1'))**2);
// sqrt(-expm1(v(-100)/3)); log(6)+log(v('1e-200'))+2*log1p(-v('1e-400'));
// sqrt(-expm1(-1)).
TEST_CASE("kumar: high precision extreme tails and densities") {
  CHECK(cppunivariate::distributions::pkumar(1e-200,2,3,true,true) == doctest::Approx(-919.93542490895016392).epsilon(2e-15));
  CHECK(std::abs(cppunivariate::distributions::qkumar(-1000,2,3,true,true)/4.1133761062941378630e-218-1) < 2e-13);
  CHECK(cppunivariate::distributions::pkumar(std::nextafter(1.0,0.0),2,3,false,true) == doctest::Approx(-108.13096016735146844).epsilon(2e-15));
  CHECK(cppunivariate::distributions::qkumar(-100,2,3,false,true) == 0.99999999999999833088);
  CHECK(cppunivariate::distributions::dkumar(1e-200,2,3,true) == doctest::Approx(-458.72525912958108180).epsilon(2e-15));
  CHECK(cppunivariate::distributions::qkumar(-1e4,2,3,true,true) == 0);
  CHECK(cppunivariate::distributions::qkumar(-1e4,2,3,false,true) == 1);
  CHECK(cppunivariate::distributions::qkumar(-1e4,2,1e4,false,true) == doctest::Approx(0.7950600976206501).epsilon(2e-15));
  CHECK(cppunivariate::distributions::dkumar(0,1,3,false) == doctest::Approx(3));
  CHECK(cppunivariate::distributions::dkumar(1,2,1,false) == doctest::Approx(2));
  CHECK(cppunivariate::distributions::dkumar(0,.5,3,false) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::dkumar(1,2,.5,false) == cppunivariate::special::kPosInf);
}

TEST_CASE("kumar: tail flags, endpoints, invalid probabilities") {
  const double lo = 0.0, hi = 1.0;
  for (bool lower : {false,true}) {
    CHECK(cppunivariate::distributions::qkumar(0,2,3,lower,false) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qkumar(1,2,3,lower,false) == (lower ? hi : lo));
    CHECK(cppunivariate::distributions::qkumar(cppunivariate::special::kNegInf,2,3,lower,true) == (lower ? lo : hi));
    CHECK(cppunivariate::distributions::qkumar(0,2,3,lower,true) == (lower ? hi : lo));
    CHECK(std::isnan(cppunivariate::distributions::qkumar(-.1,2,3,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qkumar(1.1,2,3,lower,false)));
    CHECK(std::isnan(cppunivariate::distributions::qkumar(.1,2,3,lower,true)));
    CHECK(cppunivariate::distributions::pkumar(lo,2,3,lower,false) == (lower ? 0 : 1));
    CHECK(cppunivariate::distributions::pkumar(hi,2,3,lower,false) == (lower ? 1 : 0));
    for (bool log_p : {false,true}) {
      for (double lp : {-1e-20,-.01,-1.0,-20.0,-100.0}) {
        const double p = log_p ? lp : std::exp(lp);
        const double q = cppunivariate::distributions::qkumar(p,2,3,lower,log_p);
        if (q > lo && q < hi) {
          const double back = cppunivariate::distributions::pkumar(q,2,3,lower,true);
          // Near finite support edges, test rounding by bracketing neighbours.
          const double left = cppunivariate::distributions::pkumar(std::nextafter(q,cppunivariate::special::kNegInf),2,3,lower,true);
          const double right = cppunivariate::distributions::pkumar(std::nextafter(q,cppunivariate::special::kPosInf),2,3,lower,true);
          const double target = log_p ? p : std::log(p);
          CHECK((std::abs(back-target) < 1e-12 ||
                 (target >= std::min(left,right)-1e-12 && target <= std::max(left,right)+1e-12)));
        }
      }
    }
  }
}

// mpmath: mp.dps=500; a=mp.mpf(1e-320); x=mp.mpf(float.fromhex('0x1.fffffffffffffp-1'))
// log(-expm1(a*log(x))); exp(log(-expm1(-mp.mpf(740)))/a).
TEST_CASE("kumar: double-only underflow before shape transforms") {
  CHECK(cppunivariate::distributions::pkumar(std::nextafter(1.0,0.0),1e-320,1,false,true) == doctest::Approx(-773.56404146065100749).epsilon(2e-15));
  CHECK(cppunivariate::distributions::qkumar(-740,1e-320,1,false,true) == doctest::Approx(0.95897730933901233978).epsilon(5e-14));
}

TEST_CASE("kumar: constant data cannot throw or have a finite MLE") {
  for(double v:{.01,.4,.99}) {
    std::vector<double> x(5,v);
    const auto r=cppunivariate::kumar_mle(mltest::as_span(x),1e-12,100);
    CHECK(r.status==cppunivariate::KumarStatus::Unbounded);
    CHECK(std::isnan(r.a));
  }
}
