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

TEST_CASE("lognormal: analytical Hessian agrees with FD") {
  auto g = load("lnorm");
  auto fit = mlcore::lognormal_mle(mltest::as_span(g.data));
  mlcore::LognormalCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.meanlog, fit.sdlog};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("lognormal: Hessian at MLE equals diag(n/sd^2, 2n/sd^2)") {
  auto g = load("lnorm");
  auto fit = mlcore::lognormal_mle(mltest::as_span(g.data));
  mlcore::LognormalCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.meanlog, fit.sdlog};
  double H[4];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  const double s2 = fit.sdlog * fit.sdlog;
  CHECK(H[0] == doctest::Approx(n / s2).epsilon(kFisherTol));
  CHECK(std::fabs(H[1]) < kFisherTol * (n / s2));
  CHECK(std::fabs(H[2]) < kFisherTol * (n / s2));
  CHECK(H[3] == doctest::Approx(2.0 * n / s2).epsilon(kFisherTol));
}

TEST_CASE("gamma: analytical Hessian agrees with FD") {
  auto g = load("gamma");
  auto fit = mlcore::gamma_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::GammaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.rate};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("gamma: Hessian formula matches Fisher info closed form") {
  // For Gamma(a, r): I = [[trigamma(a), -1/r], [-1/r, a/r^2]] per obs.
  // Observed info at any (a, r) is exactly n * I (data-independent).
  auto g = load("gamma");
  auto fit = mlcore::gamma_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::GammaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.rate};
  double H[4];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] ==
        doctest::Approx(n * mlcore::special::trigamma(fit.shape)).epsilon(kFisherTol));
  CHECK(H[1] == doctest::Approx(-n / fit.rate).epsilon(kFisherTol));
  CHECK(H[3] == doctest::Approx(n * fit.shape / (fit.rate * fit.rate)).epsilon(kFisherTol));
}

TEST_CASE("beta: analytical Hessian agrees with FD") {
  auto g = load("beta");
  auto fit = mlcore::beta_mle(mltest::as_span(g.data), 1.490116e-08, 100);
  mlcore::BetaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.alpha, fit.beta};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("beta: Hessian formula matches Fisher info closed form") {
  // I per obs: [[psi'(a)-psi'(a+b), -psi'(a+b)], [-psi'(a+b), psi'(b)-psi'(a+b)]].
  auto g = load("beta");
  auto fit = mlcore::beta_mle(mltest::as_span(g.data), 1.490116e-08, 100);
  mlcore::BetaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.alpha, fit.beta};
  double H[4];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  const double t1_ab = mlcore::special::trigamma(fit.alpha + fit.beta);
  CHECK(H[0] == doctest::Approx(
                    n * (mlcore::special::trigamma(fit.alpha) - t1_ab))
                    .epsilon(kFisherTol));
  CHECK(H[1] == doctest::Approx(-n * t1_ab).epsilon(kFisherTol));
  CHECK(H[3] == doctest::Approx(
                    n * (mlcore::special::trigamma(fit.beta) - t1_ab))
                    .epsilon(kFisherTol));
}

TEST_CASE("weibull: analytical Hessian agrees with FD") {
  auto g = load("weibull");
  auto fit = mlcore::weibull_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::WeibullCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.scale};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("cauchy: analytical Hessian agrees with FD") {
  auto g = load("cauchy");
  auto fit = mlcore::fit_cauchy(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::CauchyCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.scale};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("logistic: analytical Hessian agrees with FD") {
  auto g = load("logis");
  auto fit = mlcore::logistic_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::LogisticCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.scale};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

// --- discrete + 1D continuous batch ---------------------------------------

namespace {
mlcore::ispan as_ispan(const std::vector<int> &v) {
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
  auto fit = mlcore::pois_mle(as_ispan(v));
  mlcore::PoisCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("poisson: Hessian at MLE equals n/lambda") {
  auto g = load("pois");
  auto v = as_ints(g.data);
  auto fit = mlcore::pois_mle(as_ispan(v));
  mlcore::PoisCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda};
  double H[1];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  CHECK(H[0] == doctest::Approx(n / fit.lambda).epsilon(kFisherTol));
}

TEST_CASE("geom: analytical Hessian agrees with FD") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto fit = mlcore::geom_mle(as_ispan(v));
  mlcore::GeomCost cost{as_ispan(v)};
  const double theta[] = {fit.prob};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("geom: Hessian at MLE equals n / (p^2 * (1-p))") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto fit = mlcore::geom_mle(as_ispan(v));
  mlcore::GeomCost cost{as_ispan(v)};
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
  auto fit = mlcore::lgser_mle(as_ispan(v));
  mlcore::LgserCost cost{as_ispan(v)};
  const double theta[] = {fit.theta};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("invgauss: analytical Hessian agrees with FD") {
  auto g = load("invgauss");
  auto fit = mlcore::invgauss_mle(mltest::as_span(g.data));
  mlcore::InvGaussCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.lambda};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("gumbel: analytical Hessian agrees with FD") {
  auto g = load("gumbel");
  auto fit = mlcore::fit_gumbel_newton(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::GumbelCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.sigma};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("nakagami: analytical Hessian agrees with FD") {
  auto g = load("naka");
  auto fit = mlcore::naka_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::NakaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.scale};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

// --- 2D actuar/extraDistr family ------------------------------------------

TEST_CASE("lomax: analytical Hessian agrees with FD") {
  auto g = load("lomax");
  auto fit = mlcore::lomax_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::LomaxCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.lambda, fit.kappa};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("invgamma: analytical Hessian agrees with FD") {
  auto g = load("invgamma");
  auto fit = mlcore::invgamma_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::InvGammaCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.rate};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("invweibull: analytical Hessian agrees with FD") {
  auto g = load("invweibull");
  auto fit = mlcore::invweibull_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::InvWeibullCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.rate};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("betapr: analytical Hessian agrees with FD") {
  auto g = load("betapr");
  auto fit = mlcore::betapr_mle(mltest::as_span(g.data), 1.490116e-08, 100);
  mlcore::BetaPrimeCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.alpha, fit.beta};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("gompertz: analytical Hessian agrees with FD") {
  auto g = load("gompertz");
  auto fit = mlcore::gompertz_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::GompertzCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.a, fit.b};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("kumaraswamy: analytical Hessian agrees with FD") {
  auto g = load("kumar");
  auto fit = mlcore::kumar_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::KumarCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.a, fit.b};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("logitnorm: analytical Hessian agrees with FD") {
  auto g = load("logitnorm");
  auto fit = mlcore::logitnorm_mle(mltest::as_span(g.data));
  mlcore::LogitnormCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.sigma};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("loglogistic: analytical Hessian agrees with FD") {
  auto g = load("llogis");
  auto fit = mlcore::loglogistic_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::LogLogisticCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape, fit.rate};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("burr: analytical Hessian agrees with FD") {
  auto g = load("burr");
  // Burr fit uses the wrapper-style burr_mle_core seeded by burr_lmom_core.
  // For the Hessian test we only need theta_hat from the golden.
  const double theta[] = {g.estimates.at("shape1"), g.estimates.at("shape2")};
  mlcore::BurrCost cost{mltest::as_span(g.data)};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("invburr: analytical Hessian agrees with FD") {
  auto g = load("invburr");
  auto fit = mlcore::invburr_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::InvBurrCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.shape1, fit.shape2};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

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
  auto fit = mlcore::binom_mle(as_ispan(v),
                               std::numeric_limits<double>::quiet_NaN(),
                               1.220703e-4, 100);
  mlcore::BinomCost cost{as_ispan(v), fit.size};
  const double theta[] = {fit.prob};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("nbinom: analytical Hessian agrees with FD") {
  auto g = load("nbinom");
  auto v = as_ints(g.data);
  auto fit = mlcore::nbinom_mle(as_ispan(v), 1.220703e-4, 100);
  mlcore::NbinomCost cost{as_ispan(v)};
  const double theta[] = {fit.size, fit.prob};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("zip: analytical Hessian agrees with FD") {
  auto g = load("zip");
  auto v = as_ints(g.data);
  auto fit = mlcore::zip_mle(as_ispan(v));
  mlcore::ZipCost cost{as_ispan(v)};
  const double theta[] = {fit.lambda, fit.pi};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("fatigue: analytical Hessian agrees with FD") {
  auto g = load("fatigue");
  auto fit = mlcore::fatigue_mle(mltest::as_span(g.data), 1.220703e-4, 100);
  mlcore::FatigueCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.alpha, fit.beta};
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
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

TEST_CASE("pareto: analytical Hessian agrees with FD (smooth interior)") {
  auto g = load("pareto");
  auto fit = mlcore::pareto_mle(mltest::as_span(g.data));
  // Test at b = b_hat * 0.99 so b < min(x) strictly.
  const double theta[] = {fit.shape, fit.min * 0.99};
  mlcore::ParetoCost cost{mltest::as_span(g.data)};
  CHECK(cost.regular_params() == std::vector<int>{1, 0});
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("unif: analytical Hessian agrees with FD (smooth interior)") {
  auto g = load("unif");
  auto fit = mlcore::unif_mle(mltest::as_span(g.data));
  // Pad both endpoints so all data sits strictly inside [a, b].
  const double pad = 0.05 * (fit.max - fit.min);
  const double theta[] = {fit.min - pad, fit.max + pad};
  mlcore::UnifCost cost{mltest::as_span(g.data)};
  CHECK(cost.regular_params() == std::vector<int>{0, 0});
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  // Use absolute tolerance since H is sign-indefinite (negative diagonal):
  // hessian_close still works because it includes max(|a|,|b|, ref).
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("dunif: analytical Hessian agrees with FD (smooth interior)") {
  auto g = load("dunif");
  auto v = as_ints(g.data);
  auto fit = mlcore::dunif_mle(as_ispan(v));
  const double theta[] = {fit.min - 1.0, fit.max + 1.0};
  mlcore::DiscreteUnifCost cost{as_ispan(v)};
  CHECK(cost.regular_params() == std::vector<int>{0, 0});
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("power: analytical Hessian agrees with FD (smooth interior)") {
  auto g = load("power");
  auto fit = mlcore::power_mle(mltest::as_span(g.data), 1.490116e-08);
  // alpha is the upper bound — push slightly above max(x).
  double xmax = 0.0;
  for (double v : g.data) if (v > xmax) xmax = v;
  const double theta[] = {xmax * 1.01, fit.beta};
  mlcore::PowerCost cost{mltest::as_span(g.data)};
  CHECK(cost.regular_params() == std::vector<int>{0, 1});
  double H_anal[4], H_fd[4];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 4, kFdTol, max_diag(H_anal, 2)));
}

TEST_CASE("zipf: analytical Hessian agrees with FD (1-D in s, N fixed)") {
  auto g = load("zipf");
  auto v = as_ints(g.data);
  auto fit = mlcore::zipf_mle(as_ispan(v), 1.220703e-4, 100);
  mlcore::ZipfCost cost{as_ispan(v), fit.N};
  CHECK(cost.regular_params() == std::vector<int>{1});
  const double theta[] = {fit.shape};
  double H_anal[1], H_fd[1];
  cost.hessian(theta, H_anal);
  mlcore::finite_diff_hessian(cost, theta, H_fd);
  CHECK(hessian_close(H_anal, H_fd, 1, kFdTol, max_diag(H_anal, 1)));
}

TEST_CASE("laplace: Hessian equals Fisher info diag(n/s^2, n/s^2)") {
  auto g = load("laplace");
  auto fit = mlcore::laplace_mle(mltest::as_span(g.data));
  mlcore::LaplaceCost cost{mltest::as_span(g.data)};
  const double theta[] = {fit.mu, fit.sigma};
  double H[4];
  cost.hessian(theta, H);
  const double n = static_cast<double>(g.data.size());
  const double s2 = fit.sigma * fit.sigma;
  CHECK(H[0] == doctest::Approx(n / s2).epsilon(kFisherTol));
  CHECK(H[1] == 0.0);
  CHECK(H[2] == 0.0);
  CHECK(H[3] == doctest::Approx(n / s2).epsilon(kFisherTol));
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
