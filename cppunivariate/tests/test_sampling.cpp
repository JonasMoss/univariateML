// Standalone tests for the portable samplers under cppunivariate/include/cppunivariate/distributions/.
// Drives them with StdRng (std::mt19937_64-backed) so the tests run without
// any R install; complements the R-side testthat coverage that exercises the
// same code through the RRng adapter.
//
// The per-distribution sampler checks live in distributions/<id>/test.cpp;
// this file keeps the shared StdRng smoke test.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "cppunivariate/distributions/rng_std.hpp"

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

}  // namespace
