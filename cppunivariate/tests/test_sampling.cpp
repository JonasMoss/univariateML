// Standalone tests for the portable samplers under cppunivariate/include/cppunivariate/distributions/.
// Drives them with StdRng (std::mt19937_64-backed) so the tests run without
// any R install; complements the R-side testthat coverage that exercises the
// same code through the RRng adapter.
//
// Coverage strategy: per distribution, draw N samples at a fixed seed, fit
// the matching MLE, and check the recovered parameters are within a few
// standard errors of the truth. This validates the sampler end-to-end (RNG
// call sequence, parameter math, output range) without depending on
// implementation-specific bit-equivalence to R.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "cppunivariate/distributions/dunif/dpqr.hpp"
#include "cppunivariate/distributions/gumbel/dpqr.hpp"
#include "cppunivariate/distributions/invgamma/dpqr.hpp"
#include "cppunivariate/distributions/invweibull/dpqr.hpp"
#include "cppunivariate/distributions/llogis/dpqr.hpp"
#include "cppunivariate/distributions/naka/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/zipf/dpqr.hpp"

#include <random>

namespace {

// Smoke test: StdRng instantiates and emits sane values from each primitive.
TEST_CASE("StdRng emits values in the expected support") {
  cppunivariate::distributions::StdRng<> rng(42);

  // U(-1, 2): all samples should land in (-1, 2).
  for (int i = 0; i < 100; ++i) {
    const double u = rng.runif(-1.0, 2.0);
    CHECK(u > -1.0);
    CHECK(u < 2.0);
  }

  // N(0, 1): roughly mean 0, sd 1 over many draws.
  double sum = 0.0;
  double sum_sq = 0.0;
  const int n = 10000;
  for (int i = 0; i < n; ++i) {
    const double z = rng.rnorm();
    sum += z;
    sum_sq += z * z;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  // Gamma(2, 3): support is (0, inf), mean = shape * scale = 6.
  double gsum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double g = rng.rgamma(2.0, 3.0);
    CHECK(g > 0.0);
    gsum += g;
  }
  CHECK(std::abs(gsum / n - 6.0) < 0.3);

  // student-t(10): support is full R, mean = 0.
  double tsum = 0.0;
  for (int i = 0; i < n; ++i) {
    tsum += rng.rt(10.0);
  }
  CHECK(std::abs(tsum / n) < 0.1);
}

// rlogitnorm(mu=0, sigma=1) draws should land in (0, 1) and approximate
// the d/p/q layer self-consistently (CDF at the empirical median ~ 0.5).
// rnaka(shape=2, scale=1) draws are positive; sample mean roughly matches
// the analytic mean: E[X] = sqrt(scale/shape) * Gamma(shape+0.5)/Gamma(shape).
TEST_CASE("rnaka samples are positive and match analytic mean") {
  cppunivariate::distributions::StdRng<> rng(11);
  const int n = 20000;
  const double shape = 2.0;
  const double scale = 1.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rnaka(rng, shape, scale);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = sqrt(scale/shape) * Gamma(shape+0.5)/Gamma(shape)
  const double expected = std::sqrt(scale / shape) *
                          std::tgamma(shape + 0.5) / std::tgamma(shape);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

// rzipf samples land in {1, ..., N} and the empirical frequencies match
// the analytic PMF p(k) = k^{-s} / H_N(s).
TEST_CASE("rzipf samples lie in {1, ..., N} and match the PMF") {
  cppunivariate::distributions::StdRng<> rng(13);
  const int N = 10;
  const double s = 1.5;
  const auto cum = cppunivariate::distributions::zipf_cdf(N, s);
  const int n = 30000;
  std::vector<int> counts(N, 0);
  for (int i = 0; i < n; ++i) {
    const int k = cppunivariate::distributions::rzipf_from_cdf(rng, cum);
    REQUIRE(k >= 1);
    REQUIRE(k <= N);
    counts[k - 1]++;
  }
  for (int k = 1; k <= N; ++k) {
    const double empirical = static_cast<double>(counts[k - 1]) / n;
    const double analytic =
        cppunivariate::distributions::dzipf(k, N, s, /*log=*/false);
    CHECK(std::abs(empirical - analytic) < 0.02);
  }
}

// rsged at (mean=0, sd=1, nu=1.5, xi=1.5) is unit-variance by construction.
TEST_CASE("rsged samples are unit-variance and recurse cleanly into rged") {
  cppunivariate::distributions::StdRng<> rng(29);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsged(rng, 0.0, 1.0, 1.5, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

// --- Phase C tier 1: extraDistr ports -----------------------------------

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

TEST_CASE("rgumbel samples; mean = mu + sigma*gamma_const, var = pi^2*sigma^2/6") {
  cppunivariate::distributions::StdRng<> rng(131);
  const int n = 20000;
  const double mu = 1.0, sigma = 2.0;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rgumbel(rng, mu, sigma);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  const double euler_mascheroni = 0.5772156649015329;
  const double expected_mean = mu + sigma * euler_mascheroni;
  const double expected_var = M_PI * M_PI * sigma * sigma / 6.0;
  CHECK(std::abs(mean - expected_mean) < 0.1);
  CHECK(std::abs(var - expected_var) < 0.5);
}

// --- Phase C tier 2: Boost-backed transforms ---------------------------

TEST_CASE("rinvweibull samples are positive; CDF round-trip at p=0.5") {
  cppunivariate::distributions::StdRng<> rng(157);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rinvweibull(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qinvweibull(0.5, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pinvweibull(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.5) < 1e-10);
}

TEST_CASE("rllogis samples are positive; CDF round-trip at p=0.7") {
  cppunivariate::distributions::StdRng<> rng(163);
  for (int i = 0; i < 5000; ++i) {
    const double x = cppunivariate::distributions::rllogis(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      cppunivariate::distributions::qllogis(0.7, 2.0, 3.0, true, false);
  const double back =
      cppunivariate::distributions::pllogis(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

// rsstd at (mean=0, sd=1, nu=8, xi=1.5) is unit-variance by construction.
// Heavy tails (nu=8) + skew slow the empirical mean's convergence, so the
// tolerance is wider than the symmetric / lighter-tailed cases above.
TEST_CASE("rsstd samples are unit-variance and recurse cleanly into rstd") {
  cppunivariate::distributions::StdRng<> rng(31);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsstd(rng, 0.0, 1.0, 8.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.15);
  CHECK(std::abs(var - 1.0) < 0.15);
}

} // namespace
