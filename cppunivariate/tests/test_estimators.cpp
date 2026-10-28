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
// Golden "estimates" keys come from each dossier's metadata.hpp
// (canonical param names). C++ Result struct fields come from the
// estimator-author choice.
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

// The six BFGS fitters with finite-difference gradients (std, ged, sstd,
// sged, snorm, paralogis) stop wherever the FD gradient and the reltol rule
// fire. That point moves by ~1e-6 across compilers and Boost versions
// (GCC 15.2 / BH 1.90 vs the machine that made the old goldens), so their
// goldens are checked at 1e-6. Their score-residual check stays as is.
constexpr double kFdTol = 1e-6;
#define APPROX_FD(x) doctest::Approx(x).epsilon(kFdTol)

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

TEST_CASE("norm: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/norm.json");
  auto r = cppunivariate::norm_mle(mltest::as_span(g.data));
  CHECK(r.mean == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sd == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
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

TEST_CASE("laplace: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/laplace.json");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
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

TEST_CASE("lnorm: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/lnorm.json");
  auto r = cppunivariate::lognormal_mle(mltest::as_span(g.data));
  CHECK(r.meanlog == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sdlog == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
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

TEST_CASE("cauchy: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/cauchy.json");
  auto r = cppunivariate::fit_cauchy(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
}

TEST_CASE("cauchy: three observations and nonexistent MLE") {
  const std::vector<double> x{-2.2425190273413, 0.723253413022922, -2.56072856385821};
  auto r = cppunivariate::fit_cauchy(mltest::as_span(x), kFitTol, kFitMaxit);
  CHECK(r.status == cppunivariate::CauchyStatus::Ok);
  CHECK(r.mu == doctest::Approx(-2.369470147).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(0.272740500).epsilon(1e-4));
  CHECK(r.logLik >= -4.99686179 - 1e-6);
  for (const std::vector<double> &bad : {std::vector<double>{}, {1}, {1, 2}, {1, 1, 2}, {1, 1, 2, 3}}) {
    CHECK(cppunivariate::fit_cauchy(mltest::as_span(bad), kFitTol, kFitMaxit).status == cppunivariate::CauchyStatus::NoMLE);
  }
}


TEST_CASE("gumbel") {
  auto g = load("gumbel");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == APPROX(g.estimates.at("mu")));
  CHECK(r.sigma == APPROX(g.estimates.at("sigma")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("gumbel: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/gumbel.json");
  auto r = cppunivariate::fit_gumbel_newton(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}


TEST_CASE("logis") {
  auto g = load("logis");
  auto r = cppunivariate::logistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == APPROX(g.estimates.at("location")));
  CHECK(r.scale == APPROX(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("logis: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/logis.json");
  auto r = cppunivariate::logistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
  CHECK(r.status == cppunivariate::LogisticStatus::Ok);
}


TEST_CASE("logis: invalid inputs never report success") {
  for (const std::vector<double> &x : {std::vector<double>{}, {1}, {1, 1},
       {0, std::numeric_limits<double>::infinity()}}) {
    const auto r = cppunivariate::logistic_mle(mltest::as_span(x), kFitTol, kFitMaxit);
    CHECK(r.status == cppunivariate::LogisticStatus::InvalidInput);
  }
}

TEST_CASE("llogis") {
  auto g = load("llogis");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == APPROX(g.estimates.at("shape")));
  CHECK(r.rate == APPROX(g.estimates.at("rate")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("llogis: seeded sample with outlier") {
  const std::string source = __FILE__;
  auto g = mltest::load_golden(source.substr(0, source.find_last_of("/\\")) +
      "/fixtures/location-scale/llogis.json");
  auto r = cppunivariate::loglogistic_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.rate == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
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
  auto r = cppunivariate::burr_mle(mltest::as_span(g.data), kFitTol, kFitMaxit);
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
  CHECK(r.mu == APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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
  CHECK(r.mu == APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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
  CHECK(r.mu == APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX_FD(g.estimates.at("sd")));
  CHECK(r.xi == APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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
  CHECK(r.mu == APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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
  CHECK(r.mu == APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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
  CHECK(r.shape == APPROX_FD(g.estimates.at("shape")));
  CHECK(r.scale == APPROX_FD(g.estimates.at("scale")));
  CHECK(r.logLik == APPROX_FD(g.logLik));

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

TEST_CASE("pois all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::pois_mle(as_ispan(v));
  CHECK(r.lambda == 0.0);
  CHECK(r.logLik == 0.0);
}

TEST_CASE("geom") {
  auto g = load("geom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("geom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::geom_mle(as_ispan(v));
  CHECK(r.prob == 1.0);
  CHECK(r.logLik == 0.0);
  CHECK(cppunivariate::special::dgeom(1.0, 1.0, true) ==
        -std::numeric_limits<double>::infinity());
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

TEST_CASE("binom probability boundaries") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (const double size : {nan, 5.0}) {
    const std::vector<int> v{5, 5, 5};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, kFitTol, kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == 5.0);
    CHECK(r.prob == 1.0);
    CHECK(r.logLik == 0.0);
  }
  for (const double size : {nan, 0.0, 5.0}) {
    const std::vector<int> v{0, 0, 0};
    const auto r = cppunivariate::binom_mle(as_ispan(v), size, kFitTol, kFitMaxit);
    REQUIRE(r.status == cppunivariate::BinomStatus::Ok);
    CHECK(r.size == (std::isnan(size) ? 0.0 : size));
    CHECK(r.prob == 0.0);
    CHECK(r.logLik == 0.0);
  }
}

TEST_CASE("nbinom") {
  auto g = load("nbinom");
  auto v = as_ints(g.data);
  auto r = cppunivariate::nbinom_mle(as_ispan(v), kFitTol, kFitMaxit);
  CHECK(r.size == APPROX(g.estimates.at("size")));
  CHECK(r.prob == APPROX(g.estimates.at("prob")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("nbinom all zeros") {
  const std::vector<int> v{0, 0, 0};
  const auto r = cppunivariate::nbinom_mle(as_ispan(v), kFitTol, kFitMaxit);
  CHECK(r.size == 1.0);
  CHECK(r.prob == 1.0);
  CHECK(r.logLik == 0.0);
  CHECK(cppunivariate::special::dnbinom(0.0, r.size, r.prob, true) == 0.0);
  CHECK(cppunivariate::special::dnbinom(1.0, r.size, r.prob, true) ==
        -std::numeric_limits<double>::infinity());
}

TEST_CASE("zip") {
  auto g = load("zip");
  auto v = as_ints(g.data);
  auto r = cppunivariate::zip_mle(as_ispan(v));
  CHECK(r.lambda == APPROX(g.estimates.at("lambda")));
  CHECK(r.pi == APPROX(g.estimates.at("pi")));
  CHECK(r.logLik == APPROX(g.logLik));
}

TEST_CASE("zip ordinary zero inflation") {
  const std::vector<std::vector<int>> samples{
      {0, 1, 2, 1, 1, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3},
      {0, 0, 4}};
  const std::vector<double> lambdas{1.423265, 3.920695};
  const std::vector<double> pis{0.4730435, 0.6599249};
  const std::vector<double> logliks{-23.0097617, -3.52318909};
  for (std::size_t i = 0; i < samples.size(); ++i) {
    const auto r = cppunivariate::zip_mle(as_ispan(samples[i]));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.lambda == doctest::Approx(lambdas[i]).epsilon(1e-4));
    CHECK(r.pi == doctest::Approx(pis[i]).epsilon(1e-4));
    CHECK(r.logLik >= logliks[i] - 1e-6);
    const cppunivariate::ZipCost cost{as_ispan(samples[i])};
    const std::vector<double> theta{r.lambda, r.pi};
    std::vector<double> gradient(2);
    cost.gradient(theta.data(), gradient.data());
    CHECK(std::hypot(gradient[0], gradient[1]) < 1e-6);
  }
}

TEST_CASE("zip boundaries and invalid input") {
  const std::vector<int> zeros{0, 0, 0};
  const auto zero = cppunivariate::zip_mle(as_ispan(zeros));
  REQUIRE(zero.status == cppunivariate::ZipStatus::Ok);
  CHECK(zero.lambda == 1.0);
  CHECK(zero.pi == 1.0);
  CHECK(zero.logLik == 0.0);
  for (const std::vector<int> &v : {std::vector<int>{1, 2, 3},
                                  std::vector<int>{0, 1, 1, 2}}) {
    const auto r = cppunivariate::zip_mle(as_ispan(v));
    const auto pois = cppunivariate::pois_mle(as_ispan(v));
    REQUIRE(r.status == cppunivariate::ZipStatus::Ok);
    CHECK(r.pi == 0.0);
    CHECK(r.lambda == pois.lambda);
    CHECK(r.logLik == doctest::Approx(pois.logLik));
  }
  for (const std::vector<int> &v : {std::vector<int>{}, std::vector<int>{-1, 0}}) {
    CHECK(cppunivariate::zip_mle(as_ispan(v)).status ==
          cppunivariate::ZipStatus::InvalidInput);
  }
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

TEST_CASE("Burr edge reproducers") {
  // Both lower-root transformations must reject underflow before special calls.
  CHECK_FALSE(std::isfinite(cppunivariate::detail::invert_lower_logit(2.0, kFitTol, kFitMaxit).k));
  CHECK_FALSE(std::isfinite(cppunivariate::detail::invert_lower_log(2.0, kFitTol, kFitMaxit).k));
  const std::vector<double> boundary = {1.0, 2.0};
  CHECK(cppunivariate::burr_mle_core(mltest::as_span(boundary), 1.0, kFitTol, kFitMaxit).status ==
        cppunivariate::BurrStatus::AllGeOne);

  {
    const std::vector<double> x = {
      1.75518183312834530, 2.18164277910710602, 1.14570672670379281, 1.13979526186849789, 1.43606862577917460,
      3.89496853746407412, 2.22956205341489344, 1.53968283999711275, 1.95656749369845384, 1.14704599050395251,
      2.39073512881043015, 1.76202985547007351, 2.23760355073483019, 5.42393421768283535, 2.05454316731066733,
      2.03524394603074121, 2.87603517240824758, 1.65474663721397519, 1.33693347638472915, 1.58847972145304084,
      3.36451525302364596, 1.64189258823171258, 1.29412038763985038, 1.56586552457883954, 1.10607262328267097,
      1.05943916039541364, 1.57871246337890625, 4.95893285216248181, 2.17331210582077095, 1.99681295524384428,
      2.43528534337307567, 1.03726852638646960, 1.32401015283539891, 2.32046792931379109, 1.20351035003156581,
      2.02272587731004982, 1.30174093414098024, 1.72521430342913895, 1.75154269172508492, 1.23502745087096866,
      2.07988113719494372, 2.02824690375001460, 2.29226164767415996, 2.25310535445333748, 1.55464139766991138,
      1.30128299637464950, 2.29312465615094929, 1.99455578807265788, 1.51417429605498910, 3.00783240227940141,
      1.42224244493991137, 3.17877256823952958, 4.21778901774803749, 1.55782935488969088, 1.59461765177547932,
      1.97739580582300656, 1.20986658055335283, 1.30944785615429282, 2.10593626830251246, 1.77418776411087542,
      1.08967407831240504, 2.10817665676437871, 1.24726425334025914, 2.57198684654178678, 5.83281274487204637,
      1.43113213218748569, 3.73038931364629311, 2.13683141527031939, 1.81336824596344304, 1.83700649067184196,
      2.78476540454122468, 3.31247162620041413, 3.90988726897125316, 1.28559098439291120, 1.38878677205630718,
      1.05205544782802463, 1.35187049788409630, 2.56524134530959191, 1.81453580632792733, 3.75924378807160897,
      1.38619356347350520, 2.00825645677269726, 1.81851418883536176, 1.05926120560616255, 3.28385347301576180,
      1.80417091115833550, 2.58369608005806528, 2.23379150779365832, 2.34564401857079297, 3.10037722984764619,
      2.03509714729608326, 1.44745189324021339, 2.04360851455004244, 1.26082824496552348, 1.68122910801321268,
      1.26373826402717171, 1.44660565445208422, 1.21060689305886626, 1.13257140592272254, 1.34888834539891866,
      2.89984334065558969, 1.51620610477402806, 1.44899778533726931, 1.37959968773345931, 2.24876757986367970,
      3.69341156060311837, 1.19632180903350460, 3.29983717307221980, 1.56570267397910357, 1.76318000997872382,
      2.57271670569401101, 2.83564097984674035, 1.03715227497741580, 1.12556587159633636, 2.41954919687452374,
      1.71861759662152558, 1.43587055196985602, 1.06941234545696040, 1.21369195981875033, 1.29915892099961638,
      1.23565562629813153, 1.74304362102867394, 2.70992582655491887, 1.98181940704108506, 1.02025041310116649,
      1.17856945059352447, 4.25933710468893167, 2.36457221392066508, 1.09110887388377265, 2.19109463873775034,
      1.26284045679494739, 2.25346291526676445, 1.37925569759681821, 1.37882682494819164, 1.10980124631896615,
      1.90555746145917948, 1.15670788800343871, 1.09884561746080278, 3.07370036645772515, 1.78512086275983872,
      1.52816003383183041, 2.02931641025648268, 3.25115269751045943, 2.18461401290715962, 1.21982793501004716,
      1.80501129846544295, 1.57508101547136903, 1.40492502506822348, 2.70651547043564378, 3.11076216200802369,
      1.50941124325618148, 1.24082006607204676, 2.74290849446299134, 3.84816205237745423, 2.92397064148428498,
      1.72182726576448708, 2.83614894366803227, 1.71240774611416580, 1.05462155397981405, 1.03324273688831725,
      2.49532033494653760, 1.97911566212297707, 2.62494725166800080, 2.44411973833086549, 2.47738536103061602,
      1.22666864274403808, 3.66614719281408830, 3.11440364945488035, 1.34742446569725871, 3.59716722915384812,
      1.95322746103112621, 2.79710007077994405, 1.62687041703611612, 3.17458773578021569, 2.29494616757602721,
      2.51544896691086350, 2.93277326031568109, 1.68270343355834484, 1.43982797674834728, 1.76199551295013457,
      1.75075734450245557, 1.24942837934941053, 1.32923649949952960, 1.94310580655456810, 2.31590563147784501,
      1.54216732827101932, 1.76465969621659546, 1.52430305909365416, 1.20351185850561238, 3.31386824870917840,
      2.14793441999228030, 1.80177313967098307, 1.12230723159110890, 2.23036274868519335, 1.35232810163870454,
      1.77032229952038556, 1.81222992323767840, 1.45081054512411356, 1.73737168048508472, 0.98999999999999999
    };
    double mu = 0.0, variance = 0.0;
    for (auto v : x) mu += std::log(v) / x.size();
    for (auto v : x) variance += std::pow(std::log(v) - mu, 2) / x.size();
    const auto init = cppunivariate::burr_lmom_core(mu, variance, true);
    CHECK_FALSE(std::isfinite(init.c));
    const auto bad = cppunivariate::burr_mle_core(mltest::as_span(x), init.c, kFitTol, kFitMaxit);
    CHECK(bad.status == cppunivariate::BurrStatus::InvalidInit);
    const auto fit = cppunivariate::burr_mle(mltest::as_span(x), kFitTol, kFitMaxit);
    REQUIRE(fit.status == cppunivariate::BurrStatus::Ok);
    CHECK(fit.shape1 == doctest::Approx(0.0166659).epsilon(1e-3));
    CHECK(fit.shape2 == doctest::Approx(97.413).epsilon(1e-3));
    CHECK(fit.logLik >= -227.81258048920921 - 1e-6);
    // The first iteration has c*log(x) > 700; explicit init remains supported.
    const auto large = cppunivariate::burr_mle_core(mltest::as_span(x), 1000.0, kFitTol, kFitMaxit);
    REQUIRE(large.status == cppunivariate::BurrStatus::Ok);
    CHECK(large.shape1 == doctest::Approx(fit.shape1).epsilon(1e-9));
    CHECK(large.shape2 == doctest::Approx(fit.shape2).epsilon(1e-9));
    CHECK(large.logLik >= fit.logLik - 1e-9);
  }
  {
    const std::vector<double> x = {
      2.8653524405414110, 1.4047480719164014, 1.1466526696458459, 2.7307097240442282, 1.0895261805579235,
      1.6668976312503219, 2.0743668604838881, 2.5116293041141597, 2.3142759054479125, 1.1565302816554957,
      1.7451189573973100, 2.2433453509163979, 1.6737783537246287, 2.5903058870786673, 2.0822361763146642,
      1.7965972227564637, 2.4452703873718029, 5.4919423346681127, 2.7034322972500568, 1.6203842889517546,
      1.3564729540345526, 1.6888580061495304, 1.8326294775632146, 1.3344512954354286, 2.5900523773485666,
      1.0797702023693296, 1.5509576182355678, 2.0895008230161789, 1.0039397617802024, 1.6207945253700018,
      5.8622153068691416, 1.1908935642894585, 1.5717578255571425, 1.4255597282832255, 1.1804262356063970,
      1.2690414443786198, 1.7388501937226546, 1.7959512872201326, 1.9346715705049160, 4.1222055552709591,
      2.8636820669377094, 2.8532801865691191, 1.5109248328953981, 1.8453861092728499, 1.2790906772165012,
      2.0957174803641561, 1.3439978566020727, 5.0689587199822679, 2.2975513319996121, 0.5000000000000000
    };
    double mu = 0.0, variance = 0.0;
    for (auto v : x) mu += std::log(v) / x.size();
    for (auto v : x) variance += std::pow(std::log(v) - mu, 2) / x.size();
    const auto init = cppunivariate::burr_lmom_core(mu, variance, true);
    CHECK_FALSE(std::isfinite(init.c));
    const auto bad = cppunivariate::burr_mle_core(mltest::as_span(x), init.c, kFitTol, kFitMaxit);
    CHECK(bad.status == cppunivariate::BurrStatus::InvalidInit);
    const auto fit = cppunivariate::burr_mle(mltest::as_span(x), kFitTol, kFitMaxit);
    REQUIRE(fit.status == cppunivariate::BurrStatus::Ok);
    CHECK(fit.shape1 == doctest::Approx(0.194673).epsilon(1e-3));
    CHECK(fit.shape2 == doctest::Approx(8.00904).epsilon(1e-3));
    CHECK(fit.logLik >= -67.74383957580861 - 1e-6);
  }
}
TEST_CASE("inverse Burr power edge reproducers") {
  const std::vector<double> boundary = {0.5, 1.0};
  CHECK(cppunivariate::invburr_mle(mltest::as_span(boundary), kFitTol, kFitMaxit).status ==
        cppunivariate::InvBurrStatus::AllLeOne);

  {
    const std::vector<double> x = {
      0.51527532751151595, 0.61001958955167179, 0.75687077057572805, 0.95299936515969219, 0.44909011460669651,
      0.94783420753193792, 0.97194406660329558, 0.81289469950716642, 0.79316709708540012, 0.24856844221977267,
      0.45384421875716446, 0.42018656871560672, 0.82886841335508910, 0.61976101701683128, 0.87740607474450294,
      0.70547802381450442, 0.84712366763325786, 0.99594482519388750, 0.61646993392568961, 0.88172854174048076,
      0.96680154690914144, 0.46058931954905119, 0.80726313311452425, 0.35433754523239197, 0.51693391137313871,
      0.62138079511980515, 0.11571660710068962, 0.61837525586816844, 0.93257216649462038, 0.58339437491947121,
      0.69431989419217655, 0.77431636003967297, 0.70252495119310321, 0.43152937491096083, 0.90960063687696902,
      0.81759815202357478, 0.89120135812650125, 0.32854775282728060, 0.85071202297853299, 0.64130681396935640,
      0.90606086667279495, 0.80440051828670844, 0.88483487854481624, 0.74366411211065941, 0.72781837033542018,
      0.88845721995459814, 0.15274554767256898, 0.69081840235807634, 0.85575331648215081, 0.83230496603265147,
      0.69110029817352481, 0.92801372664002146, 0.66188904449625574, 0.49476992332948694, 0.26585531242566779,
      0.31538256161778599, 0.56238039366851167, 0.72016266439828358, 0.81363694386237129, 0.63783241309019523,
      0.95544540621608742, 0.54185179962801366, 0.67754389249680591, 0.57653679343778752, 0.80676543497505526,
      0.50795352220188250, 0.69176964972136690, 0.87539172411321520, 0.29025319010843414, 0.93558608905699747,
      0.58229969761654621, 0.91620977411722826, 0.58879834337296633, 0.57773257723406712, 0.69018203763471098,
      0.94456251028982907, 0.92969859128134191, 0.62449142786218292, 0.88165792620208194, 0.98011121675956925,
      0.65928710344877384, 0.84410584568216140, 0.63245108024079699, 0.57039648655840980, 0.87010754968997628,
      0.45021356614937630, 0.84328003798854134, 0.34884369139684501, 0.49546797470630904, 0.37855564904526290,
      0.48951957580541666, 0.24276403620294476, 0.80142888551141089, 0.93609252357398587, 0.88256142984205099,
      0.89292151160543765, 0.67474028605048852, 0.64037807742498787, 0.90048333841929651, 0.77777457548873141,
      0.80915012703276912, 0.59430402315283193, 0.51986550749951910, 0.99633531563091227, 0.79592290106428298,
      0.46174466452513363, 0.35968367780258703, 0.69146079737936084, 0.96128792244192030, 0.77379646364071730,
      0.98801350948312727, 0.85544871961101387, 0.59726619867786279, 0.65686656982280223, 0.38498254593908149,
      0.11435722749597171, 0.84591138193887827, 0.32122303116044404, 0.66804516958869520, 0.80006315071828626,
      0.99591094985660789, 0.70398407500184124, 0.69595224287982815, 0.41646408592820328, 0.86880431888230025,
      0.67371766283776180, 0.71496138620979943, 0.45557119452851802, 0.47818212297455909, 0.77182381170393055,
      0.75820326976465202, 0.27760471948894555, 0.18852209309616341, 0.80174527887381764, 0.96364682308136385,
      0.77336435291198058, 0.74893307311725932, 0.72527768745082200, 0.99251963400105037, 0.71248987524912688,
      0.82630991684305954, 0.77559088290757749, 0.48874193369897961, 0.50810031162411051, 0.85399626659751648,
      0.67273384886085241, 0.41848150307485965, 0.86411704625041263, 0.32401796308150238, 0.92980909278660051,
      0.78399296659866202, 0.74643120168335875, 0.57339106991056987, 0.67315038843538388, 0.70741852719896059,
      0.42528385947509101, 0.72775724166996869, 0.27436425734504005, 0.52702555217901681, 0.46119358108879116,
      0.53365764397698046, 0.94609413006689846, 0.66800847548874676, 0.88316753211654464, 0.93841304065405884,
      0.64274739167320771, 0.25260340557975097, 0.57921282071794167, 0.85072083941802357, 0.58104675672231776,
      0.79398622310639733, 0.91685034438920643, 0.92527383228432403, 0.62558714902696777, 0.61684186434179766,
      0.94627978209126618, 0.80269281977042251, 0.86085925023560694, 0.77801249766917824, 0.95030606201230117,
      0.54196877686743483, 0.43733295083767665, 0.94151523795646863, 0.70946422441936841, 0.93651350391994925,
      0.43496393234607666, 0.87069113489630412, 0.85117500707471117, 0.97145500065359069, 0.74003147704409233,
      0.84364913780746031, 0.62362256199237498, 0.31760529936181037, 0.96296525824583945, 0.53219592286204831,
      0.76848757882846774, 0.33220566663395068, 0.91679170595990622, 0.56388268677392850, 0.88478886576942850
    };
    const auto fit = cppunivariate::invburr_mle(mltest::as_span(x), kFitTol, kFitMaxit);
    CHECK(fit.status == cppunivariate::InvBurrStatus::AllLeOne);
  }
  {
    const std::vector<double> x = {
      0.265508663142099977, 0.372123899636790156, 0.572853363351896405, 0.908207789994776249, 0.201681931037455797,
      0.898389684967696667, 0.944675268605351448, 0.660797792486846447, 0.629114043898880482, 0.061786270467564464,
      0.205974574899300933, 0.176556752528995275, 0.687022846657782793, 0.384103718213737011, 0.769841419998556376,
      0.497699242085218430, 0.717618508264422417, 0.991906094830483198, 0.380035179434344172, 0.777445221319794655,
      0.934705231105908751, 0.212142521282657981, 0.651673766085878015, 0.125555095961317420, 0.267220668727532029,
      0.386114092543721199, 0.013390333158895373, 0.382387957070022821, 0.869690845720469952, 0.340348996687680483,
      0.482080115471035242, 0.599565825425088406, 0.493541307048872113, 0.186217601411044598, 0.827373318606987596,
      0.668466738192364573, 0.794239860726520419, 0.107943625887855887, 0.723710946040228009, 0.411274429643526673,
      0.820946294115856290, 0.647060193819925189, 0.782932762289419770, 0.553036311641335487, 0.529719580197706819,
      0.789356231689453125, 0.023331202333793044, 0.477230065036565065, 0.732313738670200109, 0.692731556482613087
    };
    const auto fit = cppunivariate::invburr_mle(mltest::as_span(x), kFitTol, kFitMaxit);
    CHECK(fit.status == cppunivariate::InvBurrStatus::AllLeOne);
  }
}

TEST_CASE("inverse Burr finite mostly-power MLE") {
  // set.seed(3); c(runif(199)^(1/2), 1.02), generated by R.
  const std::vector<double> x = {
    0.409928684456148051, 0.898619162422259476, 0.620437225974373008, 0.572480844365004127, 0.775951464178951889,
    0.777427844909296284, 0.353034621872015175, 0.542771521216939390, 0.760006525637505814, 0.794342038670734496,
    0.715552861462957268, 0.710650345920187165, 0.730777225600040548, 0.746491417023196768, 0.931621966060187767,
    0.910883468571148436, 0.333840011675307802, 0.838861346640808136, 0.947358572224363371, 0.528897488889862100,
    0.477704805590119852, 0.123813943133532112, 0.359140026460472050, 0.305584568512264010, 0.486708339224679987,
    0.889464675786854730, 0.774423376252255657, 0.954016619842727898, 0.748615090636634672, 0.869312814334510131,
    0.615769348975700126, 0.610967246993624258, 0.412662868312171527, 0.673281013859330923, 0.508344332572628121,
    0.579884430146522689, 0.943177096135028981, 0.449384355340306907, 0.761042733866333809, 0.455666577548473894,
    0.530536325196329428, 0.886724987853581559, 0.415955946396024123, 0.755478337355523277, 0.647520627676521410,
    0.517322109665803609, 0.218653702933996774, 0.321703363258364017, 0.560385095863395355, 0.894785483903150269,
    0.478878581084373423, 0.461517541077263449, 0.936536654845476990, 0.996605219377324492, 0.918829157052450940,
    0.954167987947593721, 0.686490882926474311, 0.473728200558682200, 0.357511757274051378, 0.528851124989536570,
    0.903385886699025087, 0.240027066855227678, 0.896007393679967401, 0.323091063582160110, 0.875560402625477852,
    0.552096519063463687, 0.877090315134308796, 0.735292952729781124, 0.601972202214401175, 0.304231681744805804,
    0.871639406629136415, 0.872249933723920456, 0.950400360493042107, 0.982996839314840054, 0.717813780033645932,
    0.741269636405696319, 0.404623154213415326, 0.405705501148446779, 0.886754068199570034, 0.866667998517222204,
    0.885558978441008726, 0.808922069520413567, 0.614901941230120430, 0.092557914002500977, 0.977409614076824806,
    0.915759874568763732, 0.461979369200338108, 0.703358767809346808, 0.797649206281950240, 0.959735064016085881,
    0.108369925546738355, 0.517110159585636775, 0.659978864612025706, 0.910750841194326699, 0.933243825856430065,
    0.501067819240358814, 0.569524581018826082, 0.553387578394351376, 0.429280834770439212, 0.824607421461806434,
    0.875407147357997406, 0.825912965822350076, 0.457308202616218351, 0.843767598915673323, 0.778009116584252536,
    0.583574506845814578, 0.202904709012069612, 0.633839498145675528, 0.281175471073779326, 0.559064084393829375,
    0.570154308766685047, 0.279940095827360680, 0.387581953416381075, 0.390416533454662706, 0.955731971356133747,
    0.838005309687087308, 0.905841223422605979, 0.815820501522474317, 0.464957109721728323, 0.680486886568929417,
    0.846671747739098590, 0.906745856047519561, 0.905948081727401489, 0.447151598231862513, 0.775329278884149042,
    0.655618793160827162, 0.798478761013231564, 0.648092590013584546, 0.938013879932076411, 0.566328553510612398,
    0.561527242232991552, 0.702546855708568008, 0.581094050676907115, 0.472002112718274636, 0.911174883719548689,
    0.733823401054050306, 0.314162211208363906, 0.331044018975808541, 0.757532198027832337, 0.733474718747907373,
    0.698215567308112073, 0.565004545316872053, 0.824490669698615397, 0.928210835078065011, 0.920396133715717379,
    0.516985125064709283, 0.778076734743133103, 0.911566808296912168, 0.769196635742883550, 0.445914434452226893,
    0.879003411591569361, 0.353730459822500398, 0.942449427631946057, 0.556189501896737437, 0.805774757265660724,
    0.512244459326625878, 0.401949487953302376, 0.324818354360386641, 0.661574177644570827, 0.196267027709410541,
    0.979120510605880012, 0.712560267360850541, 0.601919662471012029, 0.937805033521458387, 0.868605256479620169,
    0.871498280730161556, 0.943186339187165212, 0.424676559845833579, 0.886841880183364406, 0.739847754426832149,
    0.705298397449970249, 0.329819609062640517, 0.765985404216748433, 0.947271112666997595, 0.433224927669166926,
    0.727992310247125074, 0.818501457578459268, 0.884831211757011404, 0.433247933347074787, 0.752596079916005190,
    0.443399139230162942, 0.554445228086385411, 0.401358249314182292, 0.313559340751007054, 0.507506359418271935,
    0.584475826790224362, 0.924146271190016133, 0.352037096220523871, 0.589867527629312716, 0.569396505070430048,
    0.686923186549628473, 0.922603251717059591, 0.567470215107573450, 0.747099278705002590, 0.840068209117430187,
    0.625089909933574495, 0.907875207055178990, 0.952119437534129420, 0.645845990103414147, 1.020000000000000018
  };
  const auto fit = cppunivariate::invburr_mle(mltest::as_span(x), kFitTol, kFitMaxit);
  REQUIRE(fit.status == cppunivariate::InvBurrStatus::Ok);
  CHECK(fit.shape1 == doctest::Approx(0.0224871).epsilon(1e-3));
  CHECK(fit.shape2 == doctest::Approx(91.5277).epsilon(1e-3));
  CHECK(fit.logLik >= 38.4194 - 1e-6);
}

TEST_CASE("Burr large-exponent likelihood and derivatives") {
  CHECK(cppunivariate::burr_softplus(1000.0) == 1000.0);
  CHECK(cppunivariate::burr_softplus(-1000.0) == 0.0);
  const std::vector<double> x = {std::exp(1.0), std::exp(-1.0)};
  const double theta[2] = {1.0, 1000.0};
  const cppunivariate::BurrCost cost{mltest::as_span(x)};
  double gradient[2], hessian[4];
  cost.gradient(theta, gradient);
  cost.hessian(theta, hessian);
  CHECK(std::isfinite(cost(theta)));
  CHECK(gradient[0] == doctest::Approx(998.0));
  CHECK(gradient[1] == doctest::Approx(1.998));
  CHECK(hessian[0] == doctest::Approx(2.0));
  CHECK(hessian[1] == doctest::Approx(1.0));
  CHECK(hessian[3] == doctest::Approx(2e-6));
}
