#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"
#include "cppunivariate/core/maximizer_set.hpp"

TEST_CASE("maximizer boxes propagate integer order and slice holds") {
  cppunivariate::MaximizerSet set;
  set.dimension = 2;
  cppunivariate::Box box;
  box.axes[0] = cppunivariate::interval(0, 5, false, true, true);
  box.axes[1] = cppunivariate::interval(0, 9, true, true, true);
  box.order[0] = {0, 1};
  box.order[1] = {0, 1};
  box.order_size = 2;
  REQUIRE(set.add_box(box) == cppunivariate::MaxsetStatus::Ok);
  const auto& normalized = std::get<cppunivariate::Box>(set.pieces[0].value);
  CHECK(normalized.order_size == 1);
  CHECK(normalized.axes[0].range.lower == 1);
  CHECK(normalized.axes[1].range.lower == 1);
  REQUIRE(set.slice({std::nullopt, 3.}) == cppunivariate::MaxsetStatus::Ok);
  const auto& sliced = std::get<cppunivariate::Box>(set.pieces[0].value);
  CHECK(sliced.axes[0].range.upper == 3);
  CHECK(sliced.order_size == 0);
  CHECK(set.slice({5., 3.}) == cppunivariate::MaxsetStatus::Empty);
}

TEST_CASE("maximizer builders reject invalid encodings and normalize points") {
  cppunivariate::MaximizerSet set;
  set.dimension = 2;
  cppunivariate::Box wrong;
  wrong.axes[0] = cppunivariate::interval(1, -1, true, true);
  CHECK(set.add_box(wrong) == cppunivariate::MaxsetStatus::Invalid);
  wrong.axes[0] = cppunivariate::interval(1, 2, false, false, true);
  CHECK(set.add_box(wrong) == cppunivariate::MaxsetStatus::Empty);
  const std::array<cppunivariate::Point, 3> values{{
      {{{2, 0}}, {}}, {{{1, -0.}}, {}}, {{{1, 0}}, {}}}};
  REQUIRE(set.add_points({values.data(), values.size()}) == cppunivariate::MaxsetStatus::Ok);
  const auto& points = std::get<cppunivariate::Points>(set.pieces[0].value);
  REQUIRE(points.size == 2);
  CHECK(points.values[0].estimate.values[0] == 1);
  CHECK_FALSE(std::signbit(points.values[0].estimate.values[1]));
  auto overlapping = values;
  overlapping[0].enclosure = cppunivariate::Enclosure{{{1, 0}}, {{3, 0}}};
  CHECK(set.add_points({overlapping.data(), overlapping.size()}) == cppunivariate::MaxsetStatus::Invalid);
}

TEST_CASE("maximizer relations solve each named form") {
  cppunivariate::MaximizerSet set;
  set.dimension = 2;
  cppunivariate::Relation relation;
  relation.axes[0] = cppunivariate::interval(.5, 1, true, true);
  relation.axes[1] = cppunivariate::interval(0, .5, true, true);
  relation.level = {.5};
  relation.complement_dependent = true;
  REQUIRE(set.add_relation(relation) == cppunivariate::MaxsetStatus::Ok);
  CHECK(cppunivariate::relation_solve(relation, 1) == .5);

  relation.form = cppunivariate::RelationForm::PartOverSum;
  relation.complement_dependent = false;
  relation.axes[0] = relation.axes[1] = cppunivariate::interval(0, cppunivariate::maxset_inf, false, false);
  relation.level = {0, 2, 3};
  REQUIRE(set.add_relation(relation) == cppunivariate::MaxsetStatus::Ok);
  CHECK(cppunivariate::relation_solve(relation, 4) == doctest::Approx(2));

  relation.form = cppunivariate::RelationForm::CountRatio;
  const double cap = cppunivariate::maxset_count;
  relation.axes[0] = relation.axes[1] = cppunivariate::interval(1, cap, true, true, true);
  relation.level = {0, 2, static_cast<std::uint64_t>(cap)};
  REQUIRE(set.add_relation(relation) == cppunivariate::MaxsetStatus::Ok);
  CHECK(cppunivariate::relation_solve(relation, 2) == cap);
  CHECK(std::isnan(cppunivariate::relation_solve(relation, 4)));
  CHECK(set.add_relation(relation) == cppunivariate::MaxsetStatus::Capacity);
}

TEST_CASE("anchored semicircle intersects a held scale on both branches") {
  cppunivariate::MaximizerSet set;
  set.dimension = 2;
  cppunivariate::Relation relation;
  relation.form = cppunivariate::RelationForm::Semicircle;
  relation.anchors = {1, 5};
  relation.axes[0] = cppunivariate::interval(1, 5, false, false);
  relation.axes[1] = cppunivariate::interval(0, cppunivariate::maxset_inf, false, false);
  REQUIRE(set.add_relation(relation) == cppunivariate::MaxsetStatus::Ok);
  CHECK(cppunivariate::relation_solve(relation, 2) == doctest::Approx(std::sqrt(3.)));
  CHECK(cppunivariate::relation_solve(relation, 3) == 2);
  REQUIRE(set.add_relation(relation) == cppunivariate::MaxsetStatus::Ok);
  REQUIRE(set.seal() == cppunivariate::MaxsetStatus::Ok);
  CHECK(set.size == 1);
  auto vertex = set;
  REQUIRE(vertex.slice({3., 2.}) == cppunivariate::MaxsetStatus::Ok);
  CHECK(vertex.contains({{{3, 2}}}));
  std::array<std::optional<double>, cppunivariate::maxset_dim> holds{};
  holds[1] = 1;
  REQUIRE(set.slice(holds) == cppunivariate::MaxsetStatus::Ok);
  const auto& points = std::get<cppunivariate::Points>(set.pieces[0].value);
  REQUIRE(points.size == 2);
  CHECK(points.values[0].estimate.values[0] == doctest::Approx(3 - std::sqrt(3.)));
  CHECK(points.values[1].estimate.values[0] == doctest::Approx(3 + std::sqrt(3.)));
  CHECK(points.values[0].estimate.values[1] == 1);
}

TEST_CASE("point unions deduplicate exact values and reject overlapping isolated roots") {
  cppunivariate::MaximizerSet set;
  set.dimension = 2;
  const cppunivariate::Point root{{{1, 2}}, {}};
  REQUIRE(set.add_points({&root, 1}) == cppunivariate::MaxsetStatus::Ok);
  REQUIRE(set.add_points({&root, 1}) == cppunivariate::MaxsetStatus::Ok);
  CHECK(set.size == 1);
  auto overlapping = root;
  overlapping.enclosure = cppunivariate::Enclosure{{{.9, 2}}, {{1.1, 2}}};
  CHECK(set.add_points({&overlapping, 1}) == cppunivariate::MaxsetStatus::Invalid);
}
