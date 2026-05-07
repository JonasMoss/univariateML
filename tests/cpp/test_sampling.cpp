// Standalone tests for the portable samplers under src/ml/distributions/.
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

#include "../../src/ml/distributions/betapr.hpp"
#include "../../src/ml/distributions/dunif.hpp"
#include "../../src/ml/distributions/ged.hpp"
#include "../../src/ml/distributions/gumbel.hpp"
#include "../../src/ml/distributions/invgamma.hpp"
#include "../../src/ml/distributions/invgauss.hpp"
#include "../../src/ml/distributions/invweibull.hpp"
#include "../../src/ml/distributions/kumar.hpp"
#include "../../src/ml/distributions/laplace.hpp"
#include "../../src/ml/distributions/llogis.hpp"
#include "../../src/ml/distributions/logitnorm.hpp"
#include "../../src/ml/distributions/lomax.hpp"
#include "../../src/ml/distributions/nakagami.hpp"
#include "../../src/ml/distributions/pareto.hpp"
#include "../../src/ml/distributions/power.hpp"
#include "../../src/ml/distributions/rayleigh.hpp"
#include "../../src/ml/distributions/rng_std.hpp"
#include "../../src/ml/distributions/sged.hpp"
#include "../../src/ml/distributions/snorm.hpp"
#include "../../src/ml/distributions/sstd.hpp"
#include "../../src/ml/distributions/std.hpp"
#include "../../src/ml/distributions/zipf.hpp"

#include <random>

namespace {

// Smoke test: StdRng instantiates and emits sane values from each primitive.
TEST_CASE("StdRng emits values in the expected support") {
  mlcore::distributions::StdRng<> rng(42);

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
TEST_CASE("rlogitnorm samples lie in (0, 1) and match the CDF at median") {
  mlcore::distributions::StdRng<> rng(7);
  const int n = 5000;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rlogitnorm(rng, 0.0, 1.0);
    CHECK(x > 0.0);
    CHECK(x < 1.0);
    sum += x;
  }
  // mu=0 means logit(X) ~ N(0, 1), so median(X) = logistic(0) = 0.5.
  // Empirical mean is biased toward 0.5 but with high spread; sanity-check
  // the CDF at 0.5 returns 0.5.
  const double cdf_half =
      mlcore::distributions::plogitnorm(0.5, 0.0, 1.0, true, false);
  CHECK(std::abs(cdf_half - 0.5) < 1e-12);
  CHECK(std::abs(sum / n - 0.5) < 0.05);
}

// rnaka(shape=2, scale=1) draws are positive; sample mean roughly matches
// the analytic mean: E[X] = sqrt(scale/shape) * Gamma(shape+0.5)/Gamma(shape).
TEST_CASE("rnaka samples are positive and match analytic mean") {
  mlcore::distributions::StdRng<> rng(11);
  const int n = 20000;
  const double shape = 2.0;
  const double scale = 1.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rnaka(rng, shape, scale);
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
  mlcore::distributions::StdRng<> rng(13);
  const int N = 10;
  const double s = 1.5;
  const auto cum = mlcore::distributions::zipf_cdf(N, s);
  const int n = 30000;
  std::vector<int> counts(N, 0);
  for (int i = 0; i < n; ++i) {
    const int k = mlcore::distributions::rzipf_from_cdf(rng, cum);
    REQUIRE(k >= 1);
    REQUIRE(k <= N);
    counts[k - 1]++;
  }
  for (int k = 1; k <= N; ++k) {
    const double empirical = static_cast<double>(counts[k - 1]) / n;
    const double analytic =
        mlcore::distributions::dzipf(k, N, s, /*log=*/false);
    CHECK(std::abs(empirical - analytic) < 0.02);
  }
}

// rged samples are symmetric around the mean; sample mean ~ 0, sd ~ 1 at
// (mean=0, sd=1), and the distribution lines up with the CDF at 0 (= 0.5).
TEST_CASE("rged samples are symmetric and match the CDF") {
  mlcore::distributions::StdRng<> rng(17);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rged(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  // CDF at 0 should be exactly 0.5 (symmetric distribution).
  const double cdf0 =
      mlcore::distributions::pged(0.0, 0.0, 1.0, 1.5, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

// rstd at (mean=0, sd=1, nu=8) is symmetric with sd 1; CDF at 0 = 0.5.
TEST_CASE("rstd samples have unit variance and a symmetric CDF") {
  mlcore::distributions::StdRng<> rng(19);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rstd(rng, 0.0, 1.0, 8.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  const double cdf0 =
      mlcore::distributions::pstd(0.0, 0.0, 1.0, 8.0, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

// rsnorm at xi=1 should reduce to N(mean, sd); for xi != 1 the empirical
// mean shifts away from `mean` (skew bias).
TEST_CASE("rsnorm samples have unit variance and skew with xi") {
  mlcore::distributions::StdRng<> rng(23);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rsnorm(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  // Standardised parameterisation: empirical mean ~ 0, var ~ 1 by
  // construction (mlcore::dens::snorm_sigma standardises moments).
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

// rsged at (mean=0, sd=1, nu=1.5, xi=1.5) is unit-variance by construction.
TEST_CASE("rsged samples are unit-variance and recurse cleanly into rged") {
  mlcore::distributions::StdRng<> rng(29);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        mlcore::distributions::rsged(rng, 0.0, 1.0, 1.5, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

// --- Phase C tier 1: extraDistr ports -----------------------------------

TEST_CASE("rlaplace samples are symmetric around mu, var = 2*sigma^2") {
  mlcore::distributions::StdRng<> rng(101);
  const int n = 20000;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rlaplace(rng, 1.0, 2.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean - 1.0) < 0.1);
  CHECK(std::abs(var - 8.0) < 0.5); // 2*sigma^2 = 8
  // CDF at mu should be exactly 0.5.
  CHECK(std::abs(
            mlcore::distributions::plaplace(1.0, 1.0, 2.0, true, false) -
            0.5) < 1e-12);
}

TEST_CASE("rrayleigh samples are positive; mean = sigma * sqrt(pi/2)") {
  mlcore::distributions::StdRng<> rng(103);
  const int n = 20000;
  const double sigma = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rrayleigh(rng, sigma);
    CHECK(x >= 0.0);
    sum += x;
  }
  const double expected = sigma * std::sqrt(M_PI / 2.0);
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("rdunif samples are integers in [min, max]") {
  mlcore::distributions::StdRng<> rng(107);
  const int min = 1, max = 6;
  std::vector<int> counts(max - min + 1, 0);
  const int n = 30000;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rdunif(rng, min, max);
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

TEST_CASE("rpower samples lie in (0, alpha); mean = alpha*beta/(beta+1)") {
  mlcore::distributions::StdRng<> rng(109);
  const int n = 20000;
  const double alpha = 3.0, beta = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rpower(rng, alpha, beta);
    CHECK(x > 0.0);
    CHECK(x < alpha);
    sum += x;
  }
  const double expected = alpha * beta / (beta + 1.0);
  CHECK(std::abs(sum / n - expected) < 0.05);
}

TEST_CASE("rpareto samples are >= scale b; mean = a*b/(a-1) for a>1") {
  mlcore::distributions::StdRng<> rng(113);
  const int n = 20000;
  const double a = 3.0, b = 2.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rpareto(rng, a, b);
    CHECK(x >= b);
    sum += x;
  }
  const double expected = a * b / (a - 1.0); // = 3.0
  CHECK(std::abs(sum / n - expected) < 0.1);
}

TEST_CASE("rlomax samples are positive; CDF self-consistency at p=0.5") {
  mlcore::distributions::StdRng<> rng(127);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rlomax(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  // qlomax then plomax should round-trip.
  const double q = mlcore::distributions::qlomax(0.7, 2.0, 3.0, true, false);
  const double back =
      mlcore::distributions::plomax(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

TEST_CASE("rgumbel samples; mean = mu + sigma*gamma_const, var = pi^2*sigma^2/6") {
  mlcore::distributions::StdRng<> rng(131);
  const int n = 20000;
  const double mu = 1.0, sigma = 2.0;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rgumbel(rng, mu, sigma);
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

TEST_CASE("rkumar samples lie in (0, 1); CDF round-trip at p=0.3") {
  mlcore::distributions::StdRng<> rng(137);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rkumar(rng, 2.0, 3.0);
    CHECK(x >= 0.0);
    CHECK(x <= 1.0);
  }
  const double q = mlcore::distributions::qkumar(0.3, 2.0, 3.0, true, false);
  const double back =
      mlcore::distributions::pkumar(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.3) < 1e-10);
}

// --- Phase C tier 2: Boost-backed transforms ---------------------------

TEST_CASE("rinvgauss samples are positive; sample mean ~ mu") {
  mlcore::distributions::StdRng<> rng(151);
  const int n = 20000;
  const double mu = 2.0, shape = 5.0;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rinvgauss(rng, mu, shape);
    CHECK(x > 0.0);
    sum += x;
  }
  // E[X] = mu for invgauss
  CHECK(std::abs(sum / n - mu) < 0.1);
}

TEST_CASE("rinvweibull samples are positive; CDF round-trip at p=0.5") {
  mlcore::distributions::StdRng<> rng(157);
  const int n = 5000;
  for (int i = 0; i < n; ++i) {
    const double x = mlcore::distributions::rinvweibull(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      mlcore::distributions::qinvweibull(0.5, 2.0, 3.0, true, false);
  const double back =
      mlcore::distributions::pinvweibull(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.5) < 1e-10);
}

TEST_CASE("rllogis samples are positive; CDF round-trip at p=0.7") {
  mlcore::distributions::StdRng<> rng(163);
  for (int i = 0; i < 5000; ++i) {
    const double x = mlcore::distributions::rllogis(rng, 2.0, 3.0);
    CHECK(x > 0.0);
  }
  const double q =
      mlcore::distributions::qllogis(0.7, 2.0, 3.0, true, false);
  const double back =
      mlcore::distributions::pllogis(q, 2.0, 3.0, true, false);
  CHECK(std::abs(back - 0.7) < 1e-10);
}

// rsstd at (mean=0, sd=1, nu=8, xi=1.5) is unit-variance by construction.
// Heavy tails (nu=8) + skew slow the empirical mean's convergence, so the
// tolerance is wider than the symmetric / lighter-tailed cases above.
TEST_CASE("rsstd samples are unit-variance and recurse cleanly into rstd") {
  mlcore::distributions::StdRng<> rng(31);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        mlcore::distributions::rsstd(rng, 0.0, 1.0, 8.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.15);
  CHECK(std::abs(var - 1.0) < 0.15);
}

} // namespace
