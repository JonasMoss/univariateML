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

// -- discrete ---------------------------------------------------------------

