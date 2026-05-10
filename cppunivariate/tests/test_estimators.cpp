// C++ regression tests for the standalone ML core.
//
// Each TEST_CASE:
//  1. Loads tests/golden/<id>.json (data + recorded estimates + logLik).
//  2. Runs the corresponding cppunivariate::*_mle() function on the same data.
//  3. Asserts each parameter and the logLik to 1e-10 vs the recorded values.
//
// The R-side test_golden_regression.R checks the same thing through the
// R wrapper layer at 1e-12. This C++ harness verifies the analog from
// the standalone side: refactors that don't change algorithms must
// reproduce the goldens, even with no R install on PATH.
//
// Estimate-key mapping
// --------------------
// Golden "estimates" keys come from metadata/models.json (canonical param
// names). C++ Result struct fields come from the estimator-author choice.
// They do not always match (e.g. golden "a"/"b" vs CauchyResult.mu/scale,
// pareto's "a"/"b" vs ParetoResult.shape/min). Each test below resolves
// the mapping explicitly.
//
// All 41 estimators have a doctest case; lgamma is wrapper-only and
// covered by re-running gamma_mle on log(x).
//
// Each WS4 estimator pairs the golden check with a score-residual
// assertion: re-evaluate the FD gradient at the recovered MLE and
// require ||g||_2 < 1e-3. Independent of optimiser trajectory; catches
// the fake-paralogis class of bug (component score wrong because a
// parameter was implicitly fixed during the fit).

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "cppunivariate/cppunivariate.hpp"
#include "json_loader.hpp"

#include <cstdlib>
#include <string>
#include <vector>

namespace {

// Relative tolerance vs the goldens. R-side is 1e-12; we hold ourselves to
// 1e-10 here to absorb the trivial FP drift that comes from skipping the
// R wrapper layer (e.g. one fewer assignment-induced rounding).
constexpr double kTol = 1e-10;

std::string golden_path(const std::string &id) {
  // CMake passes UMVL_GOLDEN_DIR via ctest env; fall back to a relative
  // path so the binary can be run by hand from the package root.
  const char *dir = std::getenv("UMVL_GOLDEN_DIR");
  std::string base = dir ? dir : "tests/golden";
  return base + "/" + id + ".json";
}

mltest::Golden load(const std::string &id) {
  return mltest::load_golden(golden_path(id));
}

std::vector<int> as_ints(const std::vector<double> &v) {
  std::vector<int> out;
  out.reserve(v.size());
  for (double d : v) out.push_back(static_cast<int>(d));
  return out;
}

cppunivariate::ispan as_ispan(const std::vector<int> &v) {
  return {v.data(), v.size()};
}

#define APPROX(x) doctest::Approx(x).epsilon(kTol)

// Default tol/maxit used by the R wrappers in univariateML/src/ml_exports.cpp.
// Most estimators use kFitTol; beta/betapr/power use the tighter
// kFitTolTight = sqrt(.Machine$double.eps).
constexpr double kFitTol = 1.220703e-4;
constexpr double kFitTolTight = 1.490116e-08;
constexpr int kFitMaxit = 100;

}  // namespace

// -- closed-form continuous --------------------------------------------------

TEST_CASE("norm") {
  auto g = load("norm");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == APPROX(g.estimates.at("mean")));
  CHECK(r.sd == APPROX(g.estimates.at("sd")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("exp") {
  auto g = load("exp");
  auto r = cppunivariate::exp_mle(mltest::as_span(g.data));
  CHECK(r.rate == APPROX(g.estimates.at("rate")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("laplace") {
  auto g = load("laplace");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == APPROX(g.estimates.at("mu")));
  CHECK(r.sigma == APPROX(g.estimates.at("sigma")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("rayleigh") {
  auto g = load("rayleigh");
  auto r = cppunivariate::rayleigh_mle(mltest::as_span(g.data));
  CHECK(r.sigma == APPROX(g.estimates.at("sigma")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("unif") {
  auto g = load("unif");
  auto r = cppunivariate::unif_mle(mltest::as_span(g.data));
  CHECK(r.min == APPROX(g.estimates.at("min")));
  CHECK(r.max == APPROX(g.estimates.at("max")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("pareto") {
  auto g = load("pareto");
  auto r = cppunivariate::pareto_mle(mltest::as_span(g.data));
  // golden "a" → ParetoResult.shape, "b" → .min
  CHECK(r.shape == APPROX(g.estimates.at("a")));
  CHECK(r.min == APPROX(g.estimates.at("b")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("lnorm") {
  auto g = load("lnorm");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == APPROX(g.estimates.at("meanlog")));
  CHECK(r.sdlog == APPROX(g.estimates.at("sdlog")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("logitnorm") {
  auto g = load("logitnorm");
  auto r = cppunivariate::logitnorm_mle(mltest::as_span(g.data));
  CHECK(r.mu == APPROX(g.estimates.at("mu")));
  CHECK(r.sigma == APPROX(g.estimates.at("sigma")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("invgauss") {
  auto g = load("invgauss");
  auto r = cppunivariate::invgauss_mle(mltest::as_span(g.data));
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.lambda == APPROX(g.estimates.at("shape")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("power") {
  auto g = load("power");
  auto r = cppunivariate::power_mle(mltest::as_span(g.data), kFitTolTight);
  CHECK(r.alpha == APPROX(g.estimates.at("alpha")));
  CHECK(r.beta == APPROX(g.estimates.at("beta")));
  CHECK(r.logLik == APPROX(g.logLik));
}

// -- iterative continuous ----------------------------------------------------

TEST_CASE("cauchy") {
  auto g = load("cauchy");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == APPROX(g.estimates.at("location")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("gumbel") {
  auto g = load("gumbel");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == APPROX(g.estimates.at("mu")));
  CHECK(r.sigma == APPROX(g.estimates.at("sigma")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("logis") {
  auto g = load("logis");
  auto r = cppunivariate::logistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == APPROX(g.estimates.at("location")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("llogis") {
  auto g = load("llogis");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.rate == APPROX(g.estimates.at("rate")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("weibull") {
  auto g = load("weibull");
  auto r = cppunivariate::weibull_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("invweibull") {
  auto g = load("invweibull");
  auto r =
      cppunivariate::invweibull_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.rate == APPROX(g.estimates.at("rate")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("gamma") {
  auto g = load("gamma");
  auto r = cppunivariate::gamma_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.rate == APPROX(g.estimates.at("rate")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("invgamma") {
  auto g = load("invgamma");
  auto r = cppunivariate::invgamma_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("alpha")));
  CHECK(r.scale == APPROX(g.estimates.at("beta")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("naka") {
  auto g = load("naka");
  auto r = cppunivariate::naka_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("beta") {
  auto g = load("beta");
  auto r = cppunivariate::beta_mle(mltest::as_span(g.data), kFitTolTight, kFitMaxit);
  CHECK(r.alpha == APPROX(g.estimates.at("shape1")));
  CHECK(r.beta == APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("betapr") {
  auto g = load("betapr");
  auto r = cppunivariate::betapr_mle(mltest::as_span(g.data), kFitTolTight, kFitMaxit);
  CHECK(r.alpha == APPROX(g.estimates.at("shape1")));
  CHECK(r.beta == APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("kumar") {
  auto g = load("kumar");
  auto r = cppunivariate::kumar_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.a == APPROX(g.estimates.at("a")));
  CHECK(r.b == APPROX(g.estimates.at("b")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("burr") {
  auto g = load("burr");
  // Wrapper seeds c via burr_lmom from log moments.
  const std::size_t n = g.data.size();
  double sum_lx = 0.0, sum_lx_sq = 0.0;
  for (double v : g.data) {
    const double lx = std::log(v);
    sum_lx += lx;
    sum_lx_sq += lx * lx;
  }
  const double m1 = sum_lx / static_cast<double>(n);
  const double m2 = sum_lx_sq / static_cast<double>(n) - m1 * m1;
  const double c0 = cppunivariate::burr_lmom_core(m1, m2, true).c;
  auto r = cppunivariate::burr_mle_core(mltest::as_span(g.data), c0, kFitTol,
                                 kFitMaxit);
  CHECK(r.shape1 == APPROX(g.estimates.at("shape1")));
  CHECK(r.shape2 == APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("invburr") {
  auto g = load("invburr");
  auto r = cppunivariate::invburr_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape1 == APPROX(g.estimates.at("shape1")));
  CHECK(r.shape2 == APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("fatigue") {
  auto g = load("fatigue");
  auto r = cppunivariate::fatigue_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.alpha == APPROX(g.estimates.at("alpha")));
  CHECK(r.beta == APPROX(g.estimates.at("beta")));
  // Wrapper hardcodes mu=0; golden also stores 0.
  CHECK(g.estimates.at("mu") == 0.0);
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("sged (skew GED)") {
  auto g = load("sged");
  auto r = cppunivariate::sged_mle(mltest::as_span(g.data), kFitTol, 200);
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX(g.estimates.at("sd")));
  CHECK(r.nu == APPROX(g.estimates.at("nu")));
  CHECK(r.xi == APPROX(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX(g.logLik));

  cppunivariate::SgedCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("sstd (skew Student-t)") {
  auto g = load("sstd");
  auto r = cppunivariate::sstd_mle(mltest::as_span(g.data), kFitTol, 200);
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX(g.estimates.at("sd")));
  CHECK(r.nu == APPROX(g.estimates.at("nu")));
  CHECK(r.xi == APPROX(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX(g.logLik));

  cppunivariate::SstdCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("snorm (skew-normal)") {
  auto g = load("snorm");
  auto r = cppunivariate::snorm_mle(mltest::as_span(g.data), kFitTol, 200);
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX(g.estimates.at("sd")));
  CHECK(r.xi == APPROX(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX(g.logLik));

  cppunivariate::SnormCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.xi)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("ged (Generalized Error Distribution)") {
  auto g = load("ged");
  auto r = cppunivariate::ged_mle(mltest::as_span(g.data), kFitTol, 200);
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX(g.estimates.at("sd")));
  CHECK(r.nu == APPROX(g.estimates.at("nu")));
  CHECK(r.logLik == APPROX(g.logLik));

  cppunivariate::GedCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("std (Student-t with location/scale)") {
  auto g = load("std");
  auto r = cppunivariate::student_t_mle(mltest::as_span(g.data), kFitTol, 200);
  CHECK(r.mu == APPROX(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX(g.estimates.at("sd")));
  CHECK(r.nu == APPROX(g.estimates.at("nu")));
  CHECK(r.logLik == APPROX(g.logLik));

  cppunivariate::StudentTCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("paralogis") {
  auto g = load("paralogis");
  auto r = cppunivariate::paralogis_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));

  // Score-residual guard. The previous 1-param paralogis fit silently fixed
  // scale at 1; under that fit the scale-direction component of the score
  // is non-zero, so ||g||_2 > 1e-3. The 2-param fit must drive both
  // components below tolerance.
  cppunivariate::ParalogisCost cost{mltest::as_span(g.data)};
  const double phi_hat[2] = {std::log(r.shape), std::log(r.scale)};
  double grad[2];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("lomax") {
  auto g = load("lomax");
  auto r = cppunivariate::lomax_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.lambda == APPROX(g.estimates.at("lambda")));
  CHECK(r.kappa == APPROX(g.estimates.at("kappa")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("gompertz") {
  auto g = load("gompertz");
  auto r = cppunivariate::gompertz_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.a == APPROX(g.estimates.at("a")));
  CHECK(r.b == APPROX(g.estimates.at("b")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("lgamma (wrapper-only: gamma on log(x))") {
  auto g = load("lgamma");
  // Replicate mllgamma_ wrapper: y = log(x); shapelog/ratelog from gamma_mle;
  // logLik recomputed with the log-gamma density.
  std::vector<double> y;
  y.reserve(g.data.size());
  for (double v : g.data) y.push_back(std::log(v));
  auto r = cppunivariate::gamma_mle(mltest::as_span(y), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shapelog")));
  CHECK(r.rate == APPROX(g.estimates.at("ratelog")));
  // logLik for log-gamma is recomputed by the wrapper; match here.
  const double dn = static_cast<double>(g.data.size());
  double sum_y = 0.0, sum_log_y = 0.0;
  for (double v : g.data) {
    const double ly = std::log(v);
    sum_y += ly;
    sum_log_y += std::log(ly);
  }
  const double L = sum_y / dn;
  const double K = sum_log_y / dn;
  const double logLik =
      dn * (r.shape * std::log(r.rate) - std::lgamma(r.shape) +
            (r.shape - 1.0) * K - (r.rate + 1.0) * L);
  CHECK(logLik == APPROX(g.logLik));
}

// -- discrete ---------------------------------------------------------------

TEST_CASE("pois") {
  auto g = load("pois");
  auto v = as_ints(g.data);
  auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == APPROX(g.estimates.at("lambda")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("geom") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("dunif") {
  auto g = load("dunif");
  auto v = as_ints(g.data);
  auto r = cppunivariate::dunif_mle(as_ispan(v));
  CHECK(r.min == APPROX(g.estimates.at("min")));
  CHECK(r.max == APPROX(g.estimates.at("max")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("binom") {
  auto g = load("binom");
  auto v = as_ints(g.data);
  // size_or_nan: estimate size when NaN. Wrapper passes NaN by default.
  auto r = cppunivariate::binom_mle(as_ispan(v),
                             std::numeric_limits<double>::quiet_NaN(),
                             kFitTol, kFitMaxit);
  CHECK(r.size == APPROX(g.estimates.at("size")));
  CHECK(r.prob == APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("nbinom") {
  auto g = load("nbinom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::nbinom_mle(as_ispan(v), kFitTol, kFitMaxit);
  CHECK(r.size == APPROX(g.estimates.at("size")));
  CHECK(r.prob == APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("zip") {
  auto g = load("zip");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zip_mle(as_ispan(v));
  CHECK(r.lambda == APPROX(g.estimates.at("lambda")));
  CHECK(r.pi == APPROX(g.estimates.at("pi")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("lgser") {
  auto g = load("lgser");
  auto v = as_ints(g.data);
  auto r = cppunivariate::lgser_mle(as_ispan(v));
  CHECK(r.theta == APPROX(g.estimates.at("theta")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("zipf") {
  auto g = load("zipf");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zipf_mle(as_ispan(v), kFitTol, kFitMaxit);
  CHECK(r.N == APPROX(g.estimates.at("N")));
  CHECK(r.shape == APPROX(g.estimates.at("s")));
  CHECK(r.logLik == APPROX(g.logLik));
}
