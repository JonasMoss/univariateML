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

#include "cppunivariate/core/finite_diff.hpp"
#include "cppunivariate/cppunivariate.hpp"
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

// FD-Hessian accuracy in double precision is ~1e-4 to ~1e-3 in practice.
// Heavy-tailed / sign-changing densities (Cauchy's (1-z^2) numerator,
// the Logistic Hessian's (2P-1) cancellations) sit at the looser end
// because FD differences subtract nearly-equal positive/negative
// contributions. Layer 2 (closed-form Fisher info) is the strict check
// at 1e-8; Layer 1 here is a "no gross typos" sanity gate.
constexpr double kFdTol = 1e-3;
constexpr double kFisherTol = 1e-8;

}  // namespace

// --- Layer 1: analytical-vs-FD at the MLE ----------------------------------

}

// --- discrete + 1D continuous batch ---------------------------------------

namespace {
cppunivariate::ispan as_ispan(const std::vector<int> &v) {
  return {v.data(), v.size()};
}
std::vector<int> as_ints(const std::vector<double> &v) {
  std::vector<int> out;
  out.reserve(v.size());
  for (double d : v) out.push_back(static_cast<int>(d));
  return out;
}
}  // namespace

TEST_CASE("poisson: analytical Hessian agrees with FD") {
  auto g = load("pois");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::pois_mle(as_ispan(v));
  cppunivariate::PoisCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("poisson: Hessian at MLE equals n/lambda") {
  auto g = load("pois");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::pois_mle(as_ispan(v));
  cppunivariate::PoisCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda};
  double H[1];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] == doctest::Approx(n / fit.lambda).epsilon(kFisherTol));
}

TEST_CASE("geom: analytical Hessian agrees with FD") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::geom_mle(as_ispan(v));
  cppunivariate::GeomCost cost{as_ispan(v)};
  const double theta[] = {fit.prob};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("geom: Hessian at MLE equals n / (p^2 * (1-p))") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::geom_mle(as_ispan(v));
  cppunivariate::GeomCost cost{as_ispan(v)};
  const double theta[] = {fit.prob};
  double H[1];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] == doctest::Approx(n / (fit.prob * fit.prob * (1.0 - fit.prob)))
                    .epsilon(kFisherTol));
}

TEST_CASE("lgser: analytical Hessian agrees with FD") {
  auto g = load("lgser");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::lgser_mle(as_ispan(v));
  cppunivariate::LgserCost cost{as_ispan(v)};
  const double theta[] = {fit.theta};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

// --- 2D actuar/extraDistr family ------------------------------------------

// --- non-smooth: Laplace returns Fisher info, not observed info ----------
//
// We don't compare to FD-Hessian for Laplace: FD on `sum |x - mu|` over a
// stencil step h sees the kink and returns zero second-derivative noise
// for the mu direction, which doesn't match the (correct) Fisher info.
// Instead, assert directly that H equals the closed-form Fisher info at
// the MLE: diag(n/sigma^2, n/sigma^2). This is the asymptotic-variance
// matrix that gives correct Wald SEs (median ~ N(mu, sigma^2/n)).

TEST_CASE("binom: analytical Hessian agrees with FD (1-D in prob)") {
  auto g = load("binom");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::binom_mle(as_ispan(v),
                               std::numeric_limits<double>::quiet_NaN(),
                               1.220703e-4, 100);
  cppunivariate::BinomCost cost{as_ispan(v), fit.size};
  const double theta[] = {fit.prob};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("nbinom: analytical Hessian agrees with FD") {
  auto g = load("nbinom");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::nbinom_mle(as_ispan(v), 1.220703e-4, 100);
  cppunivariate::NbinomCost cost{as_ispan(v)};
  const double theta[] = {fit.size, fit.prob};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("zip: analytical Hessian agrees with FD") {
  auto g = load("zip");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::zip_mle(as_ispan(v));
  cppunivariate::ZipCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda, fit.pi};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

// --- boundary-non-regular estimators --------------------------------------
//
// Pareto, Uniform, DUnif, Power, Zipf have at least one parameter on the
// support boundary. The MLE for the boundary parameter sits AT the
// boundary and is NOT asymptotically normal. The Hessian formula in the
// smooth interior is still well-defined and is what we test here; Phase 4
// will mark the corresponding row/col as NaN via regular_params().
//
// Tests evaluate at theta_test slightly inside the smooth region (not at
// the MLE itself, which is on the boundary and FD across the boundary
// would return Inf).

TEST_CASE("dunif: analytical Hessian agrees with FD (smooth interior)") {
  auto g = load("dunif");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::dunif_mle(as_ispan(v));
  const double theta[] = {fit.min - 1.0, fit.max + 1.0};
  cppunivariate::DiscreteUnifCost cost{as_ispan(v)};
  CHECK(cost.regular_params() == std::vector<int>{0, 0});
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("zipf: analytical Hessian agrees with FD (1-D in s, N fixed)") {
  auto g = load("zipf");
  auto v = as_ints(g.data);
  auto fit = cppunivariate::zipf_mle(as_ispan(v), 1.220703e-4, 100);
  cppunivariate::ZipfCost cost{as_ispan(v), fit.N};
  CHECK(cost.regular_params() == std::vector<int>{1});
  const double theta[] = {fit.shape};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  cppunivariate::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

// --- multi-D BFGS estimators (Hessian in phi/transformed space) ------------
//
// These six estimators fit in transformed (log-reparameterised) parameter
// space via BFGS. Their Cost::hessian is therefore in phi-space too, and
// is compared against finite_diff_hessian at phi_hat (computed from the
// natural-space MLE via to_log / to_log_shift). Phase 4 will apply the
// chain-rule Jacobian to surface the Hessian in natural parameters.
//
// No Layer-2 (closed-form Fisher info) check here — the analytic Fisher
// information for these families is itself a paper-length result that
// would just duplicate the Hessian formula.

TEST_CASE("sstd: analytical Hessian agrees with FD") {
  auto g = load("sstd");
  auto fit = cppunivariate::sstd_mle(mltest::as_span(g.data), 1.220703e-4, 200);
  cppunivariate::SstdCost cost{mltest::as_span(g.data)};
  const double phi[] = {fit.mu, std::log(fit.sigma),
                        std::log(fit.nu - 2.0), std::log(fit.xi)};
  double H_anal[16], H_fd[16];
  cost.hessian(phi, H_anal);
  cppunivariate::finite_diff_hessian(cost, phi, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 16, kFdTol, max_diag(H_anal, 4)));
}

TEST_CASE("sged: analytical Hessian agrees with FD") {
  auto g = load("sged");
  auto fit = cppunivariate::sged_mle(mltest::as_span(g.data), 1.220703e-4, 200);
  cppunivariate::SgedCost cost{mltest::as_span(g.data)};
  const double phi[] = {fit.mu, std::log(fit.sigma),
                        std::log(fit.nu), std::log(fit.xi)};
  double H_anal[16], H_fd[16];
  cost.hessian(phi, H_anal);
  cppunivariate::finite_diff_hessian(cost, phi, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 16, kFdTol, max_diag(H_anal, 4)));
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
