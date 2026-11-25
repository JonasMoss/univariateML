// Comparison outcomes (core/stop_detail.hpp; dev/design/consolidation.md,
// "Outcome model"): the better candidate, and how it renders.
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/core/stop_detail.hpp"
#include "../../bindings/common/fit.hpp"
#include <cmath>
#include <limits>
#include <vector>

namespace cu = cppunivariate;
namespace {
typedef cu::FitCandidate FitCandidate;
typedef cu::StopModel StopModel;
cppunivariate::StopDetail detail(std::vector<FitCandidate> c) {
  cppunivariate::StopDetail d; d.comparison_tie = true; d.candidates = c; return d;
}
const auto family = [](const FitCandidate& c) { return c.model == StopModel::None; };
const auto never = [](const FitCandidate&) { return false; };
const auto exact = [](const FitCandidate& c) { return cppunivariate::point_value(c); };
}

TEST_CASE("the better candidate has the larger lower bound at rounding resolution") {
  const double a = -3.5, b = std::nextafter(a, 0.);
  // One ulp apart counts as equal: the first listed wins without a preference.
  CHECK(cppunivariate::better_candidate(detail({{{1, 1}, a, a, StopModel::Norm}, {{1, 1}, b, b}}), never) == 0);
  // An accurate family point is preferred among equal bounds.
  CHECK(cppunivariate::better_candidate(detail({{{1, 1}, a, a, StopModel::Norm}, {{1, 1}, b, b}}), family) == 1);
  // A clearly larger bound wins regardless of preference; NaN bounds lose.
  CHECK(cppunivariate::better_candidate(detail({{{1, 1}, a, a}, {{1, 1}, a + 1e-6, a + 1, StopModel::Norm}}), family) == 1);
  CHECK(cppunivariate::better_candidate(detail({{{1, 1}, NAN, NAN}, {{1, 1}, a, a, StopModel::Norm}}), family) == 1);
}

TEST_CASE("comparison outcomes: estimate, limit or unresolved by the better candidate") {
  // An accurate family point: an uncertified estimate.
  auto o = cppunivariate::comparison_outcome(detail({{{1, 1}, -2, -2}, {{1, 1}, -3, -3, StopModel::Norm}}), family, exact);
  CHECK(o.kind == cu::OutcomeKind::Estimate); CHECK_FALSE(o.certified); CHECK_FALSE(o.uniqueness_known);
  // A family point without a point log likelihood is not returned.
  o = cppunivariate::comparison_outcome(detail({{{1, 1}, -2, -1.9}, {{1, 1}, -3, -3, StopModel::Norm}}), family, exact);
  CHECK(o.kind == cu::OutcomeKind::Failure); CHECK(o.reason == cppunivariate::OutcomeReason::Unresolved);
  // A better face: an uncertified limit; the family's own model is not a face.
  o = cppunivariate::comparison_outcome(detail({{{1, 1}, -3, -3}, {{0, 1}, -2, -2, StopModel::Norm}}), family, exact);
  CHECK(o.kind == cu::OutcomeKind::Limit); CHECK_FALSE(o.certified);
  o = cppunivariate::comparison_outcome(detail({{{1, 1}, -3, -3}, {{0, 1, 3}, -2, -2, StopModel::Lst}}), family, exact, StopModel::Lst);
  CHECK(o.kind == cu::OutcomeKind::Failure);
  CHECK(cppunivariate::comparison_outcome(detail({}), family, exact).kind == cu::OutcomeKind::Failure);
}

TEST_CASE("equal limiting suprema require at least two valid faces") {
  const auto o = cppunivariate::equal_faces_outcome(
      detail({{{0, 1}, -1, -1, StopModel::Hnorm}, {{0, 1}, -1, -1, StopModel::ReflectedHnorm}}), true);
  CHECK(o.kind == cu::OutcomeKind::Limit);
  CHECK(o.certified);
  for (const auto& d : {detail({{{0, 1}, 0, 0}, {{1, 1}, 0, 0}}),
                        detail({{{0, 1}, -1.1, -0.9}, {{1, 1}, -1.1, -0.9}}),
                        detail({})}) {
    const auto stopped = cppunivariate::equal_faces_outcome(d, true);
    CHECK(stopped.kind == cu::OutcomeKind::Failure);
    CHECK(stopped.reason == cu::OutcomeReason::Unresolved);
  }
}

TEST_CASE("rendering refuses non-unique estimates without a typed set") {
  umvl_binding::FitResult fit;
  fit.estimates = {{"location", 0.}};
  fit.logLik = -2.;
  fit.candidates = {{fit.estimates, -2., -2., ""}, {{{"location", 1.}}, -2., -2., ""}};
  fit.outcome = cu::FitOutcome::estimate(true, false);
  const auto rejected = umvl_binding::render_fit(fit);
  CHECK(rejected.error_kind == "numerical");
  CHECK(rejected.error == "The fit returned an invalid maximizer set.");
  CHECK(rejected.outcome.kind == cu::OutcomeKind::Failure);
  CHECK(rejected.outcome.reason == cu::OutcomeReason::Evaluation);
  CHECK(rejected.outcome_override);
  CHECK(rejected.maximizers.empty());
  CHECK(rejected.candidates.size() == 2);

  fit.outcome = cu::FitOutcome::undetermined();
  const auto comparison = umvl_binding::render_fit(fit);
  CHECK(comparison.error_kind.empty());
  CHECK(comparison.estimates == fit.estimates);
  CHECK(comparison.candidates.size() == 2);
  CHECK(comparison.maximizers.empty());

  cu::MaximizerSet set;
  set.dimension = 1;
  REQUIRE(set.add_box({{cu::interval(0., 1., true, true)}}) == cu::MaxsetStatus::Ok);
  fit.outcome = cu::FitOutcome::estimate(true, false);
  umvl_binding::attach_maximizer_set(fit, set, "example", [&](const auto&) { return fit.logLik; });
  const auto rendered = umvl_binding::render_fit(fit);
  REQUIRE(rendered.error_kind.empty());
  REQUIRE(rendered.maximizer_set);
  REQUIRE(rendered.maximizers.size() == 2);
  CHECK(rendered.maximizers.front().estimates == fit.estimates);
  CHECK(rendered.candidates.size() == 2);
}

TEST_CASE("certification and cardinality are independent at the fit boundary") {
  umvl_binding::FitResult fit;
  fit.estimates = {{"scale", 1.}};
  fit.logLik = -2.;
  fit.outcome = cu::FitOutcome::unknown_cardinality(true);
  const auto rendered = umvl_binding::render_fit(fit);
  CHECK(rendered.error_kind.empty());
  CHECK(rendered.outcome.kind == cu::OutcomeKind::Estimate);
  CHECK(rendered.outcome.certified);
  CHECK_FALSE(rendered.outcome.uniqueness_known);
  CHECK_FALSE(rendered.maximizer_set);
  CHECK_FALSE(cu::FitOutcome::undetermined().certified);
  CHECK(cu::FitOutcome::singleton(true).uniqueness_known);
}
