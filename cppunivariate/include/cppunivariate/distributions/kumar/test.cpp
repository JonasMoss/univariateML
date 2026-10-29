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

TEST_CASE("kumar: constant data cannot throw or have a finite MLE") {
  for(double v:{.01,.4,.99}) {
    std::vector<double> x(5,v);
    const auto r=cppunivariate::kumar_mle(mltest::as_span(x),1e-12,100);
    CHECK(r.status==cppunivariate::KumarStatus::Unbounded);
    CHECK(std::isnan(r.a));
  }
}
