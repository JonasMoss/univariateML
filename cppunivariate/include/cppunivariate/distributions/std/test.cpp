#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/std/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rstd at (mean=0, sd=1, nu=8) is symmetric with sd 1; CDF at 0 = 0.5.
TEST_CASE("rstd samples have unit variance and a symmetric CDF") {
  cppunivariate::distributions::StdRng<> rng(19);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rstd(rng, 0.0, 1.0, 8.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  const double cdf0 =
      cppunivariate::distributions::pstd(0.0, 0.0, 1.0, 8.0, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("std (Student-t with location/scale)") {
  auto g = mltest::load_distribution_golden("std");
  auto r = cppunivariate::student_t_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == C_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == C_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == C_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == C_APPROX_FD(g.logLik));

  cppunivariate::StudentTCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("std compares normal and df-two faces") {
  std::vector<double> uniform;
  for (int i = 0; i < 200; ++i) uniform.push_back((i - 99.5) / 100.0);
  auto normal = cppunivariate::student_t_mle(mltest::as_span(uniform), mltest::kFitTol, 200);
  CHECK(normal.status == cppunivariate::StudentTStatus::NormalBoundary);
  std::vector<double> cluster;
  for (int i = 0; i < 30; ++i) cluster.push_back(-1e-4 + 2e-4 * i / 29.0);
  for (int i = 0; i < 10; ++i) cluster.push_back(0.9999 + 2e-4 * i / 9.0);
  auto two = cppunivariate::student_t_mle(mltest::as_span(cluster), mltest::kFitTol, 200);
  CHECK(two.status == cppunivariate::StudentTStatus::DfTwoBoundary);
  CHECK(two.logLik == doctest::Approx(54.07217335).epsilon(1e-7));
}

TEST_CASE("std large nu density retains precision") {
  // Independent high precision log-density values at z=1.25.
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1e8) ==
        doctest::Approx(-1.7001885430386572).epsilon(1e-12));
}

#undef C_APPROX
#undef C_APPROX_FD
