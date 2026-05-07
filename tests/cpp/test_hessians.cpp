// Per-estimator Hessian tests.
//
// Three layers, evaluated at the recovered MLE on golden data:
//   1. Analytical Hessian agrees with finite_diff_hessian (tol 1e-6 default).
//   2. For estimators with a known closed-form Fisher information (norm,
//      exp, gamma, beta — TBD), analytical Hessian agrees with n*I(theta_hat)
//      to 1e-8.
//   3. End-to-end CI agreement with parametric bootstrap is checked R-side
//      in tests/testthat/test_inference.R (Phase 5).
//
// The tests load the same goldens the estimator regression tests use, so
// the data is fixed and reproducible.

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "../../src/ml/core/finite_diff.hpp"
#include "../../src/ml/ml.hpp"
#include "json_loader.hpp"

#include <cmath>
#include <cstdlib>
#include <string>
#include <vector>

namespace {

std::string golden_path(const std::string &id) {
  const char *dir = std::getenv("UMVL_GOLDEN_DIR");
  std::string base = dir ? dir : "tests/golden";
  return base + "/" + id + ".json";
}

mltest::Golden load(const std::string &id) {
  return mltest::load_golden(golden_path(id));
}

// Combined tolerance: |a - b| <= atol + rtol * max(|a|, |b|, ref).
// FD-Hessian accuracy in double precision is ~rtol = 1e-4 on non-zero
// entries and atol ~ rtol * max(|H_ii|) on entries that are zero analytically
// (off-diagonals at MLE) — the stencil noise scales with the larger
// neighbouring values. `ref` lets the caller pin atol to a scale of
// interest (typically the max of the diagonal).
bool hessian_close(const double *a, const double *b, int n, double rtol,
                   double ref) {
  for (int i = 0; i < n; ++i) {
    const double scale = std::fmax(std::fmax(std::fabs(a[i]), std::fabs(b[i])),
                                   ref);
    if (std::fabs(a[i] - b[i]) > rtol * scale) return false;
  }
  return true;
}

double max_diag(const double *H, int n) {
  double m = 0.0;
  for (int i = 0; i < n; ++i) {
    const double v = std::fabs(H[static_cast<std::size_t>(i) * n + i]);
    if (v > m) m = v;
  }
  return m;
}

constexpr double kFdTol = 1e-4;
constexpr double kFisherTol = 1e-8;

}  // namespace

// --- Layer 1: analytical-vs-FD at the MLE ----------------------------------

TEST_CASE("normal: analytical Hessian agrees with FD") {
  auto g = load("norm");
  auto fit = mlcore::norm_mle(mltest::as_span(g.data));
  mlcore::NormCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mean, fit.sd};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("exp: analytical Hessian agrees with FD") {
  auto g = load("exp");
  auto fit = mlcore::exp_mle(mltest::as_span(g.data));
  mlcore::ExpCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.rate};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("rayleigh: analytical Hessian agrees with FD") {
  auto g = load("rayleigh");
  auto fit = mlcore::rayleigh_mle(mltest::as_span(g.data));
  mlcore::RayleighCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.sigma};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

// --- Layer 2: analytical vs closed-form Fisher information at MLE ----------
//
// For regular families with a closed-form Fisher information per observation,
// the observed information at the MLE equals n * I(theta_hat) (up to noise
// of order 1/sqrt(n)). For exact-information families (norm, exp, rayleigh)
// the equality holds at machine precision when the data is bit-perfectly
// generated under the model.
//
// At the MLE for normal: I(mu, sigma) = diag(1/sigma^2, 2/sigma^2), so
// observed info = n * I = diag(n/sigma^2, 2n/sigma^2). Exact at MLE because
// sum(x - mu_hat) = 0 and sum((x - mu_hat)^2) = n * sigma_hat^2.

TEST_CASE("normal: Hessian at MLE equals diag(n/s^2, 2n/s^2)") {
  auto g = load("norm");
  auto fit = mlcore::norm_mle(mltest::as_span(g.data));
  mlcore::NormCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mean, fit.sd};
  double H[4];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  const double s2 = fit.sd * fit.sd;
  CHECK(H[0] == doctest::Approx(n / s2).epsilon(kFisherTol));
  CHECK(std::fabs(H[1]) < kFisherTol);
  CHECK(std::fabs(H[2]) < kFisherTol);
  CHECK(H[3] == doctest::Approx(2.0 * n / s2).epsilon(kFisherTol));
}

TEST_CASE("exp: Hessian at MLE equals n / lambda^2") {
  auto g = load("exp");
  auto fit = mlcore::exp_mle(mltest::as_span(g.data));
  mlcore::ExpCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.rate};
  double H[1];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] == doctest::Approx(n / (fit.rate * fit.rate)).epsilon(kFisherTol));
}

TEST_CASE("rayleigh: Hessian at MLE equals 4n / sigma^2") {
  auto g = load("rayleigh");
  auto fit = mlcore::rayleigh_mle(mltest::as_span(g.data));
  mlcore::RayleighCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.sigma};
  double H[1];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] ==
        doctest::Approx(4.0 * n / (fit.sigma * fit.sigma)).epsilon(kFisherTol));
}
