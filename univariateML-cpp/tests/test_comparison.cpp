// Comparison outcomes (core/stop_detail.hpp; dev/design/consolidation.md,
// "Outcome model"): the better candidate, and how it renders.
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/core/stop_detail.hpp"
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
  CHECK(o.kind == cu::OutcomeKind::Estimate); CHECK_FALSE(o.certified); CHECK(o.unique);
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

TEST_CASE("proved non-uniqueness: estimate only for accurate family points") {
  auto o = cppunivariate::nonunique_outcome(detail({{{0, 1}, 0, 0}, {{1, 1}, 0, 0}}), true, family, exact);
  CHECK(o.kind == cu::OutcomeKind::Estimate); CHECK(o.certified); CHECK_FALSE(o.unique);
  o = cppunivariate::nonunique_outcome(detail({{{0, 1}, -1, -1, StopModel::Hnorm}, {{0, 1}, -1, -1, StopModel::ReflectedHnorm}}), true, family, exact);
  CHECK(o.kind == cu::OutcomeKind::Limit); CHECK(o.certified);
  o = cppunivariate::nonunique_outcome(detail({{{0, 1}, -1.1, -0.9}, {{1, 1}, -1.1, -0.9}}), true, family, exact);
  CHECK(o.kind == cu::OutcomeKind::Failure); CHECK(o.reason == cppunivariate::OutcomeReason::Unresolved);
  CHECK(cppunivariate::nonunique_outcome(detail({}), true, family, exact).kind == cu::OutcomeKind::Failure);
}
