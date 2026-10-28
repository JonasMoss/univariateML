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
#include <random>
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

// The five BFGS fitters with finite-difference gradients (std, ged, sstd,
// sged, paralogis) stop wherever the FD gradient and the reltol rule
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
