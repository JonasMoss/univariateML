// C++ regression tests for the standalone ML core.
//
// Each test case:
//  1. Loads tests/golden/<id>.json (data + recorded estimates + logLik).
//  2. Runs the corresponding mlcore::*_mle() function.
//  3. Asserts params and logLik to 1e-10 vs the recorded values.
//
// The R-side test_golden_regression.R provides the equivalent check
// from the wrapper layer at 1e-12. This harness is the analog
// guarantee from the C++ side: refactors that don't change algorithms
// must reproduce the goldens, even with no R install on PATH (once
// WS3's standalone-Rmath build lands).
//
// New cases will be added as remaining estimators are migrated and
// the harness expands (see WS3 task #3).

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

#include "../../src/ml/ml.hpp"
#include "json_loader.hpp"

#include <cstdlib>
#include <string>

namespace {

constexpr double kTol = 1e-10;

std::string golden_path(const std::string &id) {
  // CMake passes UMVL_GOLDEN_DIR; fall back to a relative path for
  // editor-driven runs (run from the package root).
  const char *dir = std::getenv("UMVL_GOLDEN_DIR");
  std::string base = dir ? dir : "tests/golden";
  return base + "/" + id + ".json";
}

mltest::Golden load(const std::string &id) {
  return mltest::load_golden(golden_path(id));
}

}  // namespace

TEST_CASE("normal MLE matches golden") {
  const auto g = load("norm");
  const auto fit = mlcore::norm_mle(mltest::as_span(g.data));
  CHECK(fit.mean == doctest::Approx(g.estimates.at("mean")).epsilon(kTol));
  CHECK(fit.sd == doctest::Approx(g.estimates.at("sd")).epsilon(kTol));
  CHECK(fit.logLik == doctest::Approx(g.logLik).epsilon(kTol));
}

TEST_CASE("exponential MLE matches golden") {
  const auto g = load("exp");
  const auto fit = mlcore::exp_mle(mltest::as_span(g.data));
  CHECK(fit.rate == doctest::Approx(g.estimates.at("rate")).epsilon(kTol));
  CHECK(fit.logLik == doctest::Approx(g.logLik).epsilon(kTol));
}

TEST_CASE("laplace MLE matches golden") {
  const auto g = load("laplace");
  const auto fit = mlcore::laplace_mle(mltest::as_span(g.data));
  CHECK(fit.mu == doctest::Approx(g.estimates.at("mu")).epsilon(kTol));
  CHECK(fit.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(kTol));
  CHECK(fit.logLik == doctest::Approx(g.logLik).epsilon(kTol));
}
