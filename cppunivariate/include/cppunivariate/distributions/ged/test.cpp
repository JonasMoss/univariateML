#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rged samples are symmetric around the mean; sample mean ~ 0, sd ~ 1 at
// (mean=0, sd=1), and the distribution lines up with the CDF at 0 (= 0.5).
TEST_CASE("rged samples are symmetric and match the CDF") {
  cppunivariate::distributions::StdRng<> rng(17);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rged(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  // CDF at 0 should be exactly 0.5 (symmetric distribution).
  const double cdf0 =
      cppunivariate::distributions::pged(0.0, 0.0, 1.0, 1.5, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("ged (Generalized Error Distribution)") {
  auto g = mltest::load_distribution_golden("ged");
  auto r = cppunivariate::ged_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == C_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == C_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == C_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == C_APPROX_FD(g.logLik));

  cppunivariate::GedCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  if (r.nu > 1.0 && r.status == cppunivariate::GedStatus::Ok) CHECK(gnorm < 1e-3);
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("ged constrained faces and shapes") {
  std::vector<double> uniform, peaked;
  for (int i = 0; i < 40; ++i) uniform.push_back(-1.0 + 2.0*i/39.0);
  for (int i = 0; i < 18; ++i) peaked.push_back(-0.02 + 0.04*i/17.0);
  peaked.push_back(-10.0); peaked.push_back(10.0);
  auto u = cppunivariate::ged_mle(mltest::as_span(uniform), 1e-6, 500);
  CHECK(u.status == cppunivariate::GedStatus::UniformBoundary);
  auto l = cppunivariate::ged_mle(mltest::as_span(peaked), 1e-6, 500);
  CHECK(l.status == cppunivariate::GedStatus::Ok);
  CHECK(l.nu == 1.0);
  CHECK(l.nu >= 1.0);
  cppunivariate::distributions::StdRng<> rng(812);
  for (double shape : {1.5, 3.0}) {
    std::vector<double> x;
    for (int i = 0; i < 500; ++i)
      x.push_back(cppunivariate::distributions::rged(rng, 0.0, 1.0, shape));
    auto r = cppunivariate::ged_mle(mltest::as_span(x), 1e-6, 500);
    CHECK(r.status == cppunivariate::GedStatus::Ok);
    CHECK(r.nu > 1.0);
  }
}
