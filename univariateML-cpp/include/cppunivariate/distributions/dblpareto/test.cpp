#include "test_support.hpp"
#include "cppunivariate/distributions/dblpareto/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

TEST_CASE("dblpareto: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("dblpareto",
      [](double x, const auto& p, bool log) {
        return cppunivariate::distributions::ddblpareto(x, p[0], p[1], p[2], log);
      },
      [](double x, const auto& p, bool lower, bool log) {
        return cppunivariate::distributions::pdblpareto(x, p[0], p[1], p[2], lower, log);
      },
      [](double x, const auto& p, bool lower, bool log) {
        return cppunivariate::distributions::qdblpareto(x, p[0], p[1], p[2], lower, log);
      },
      [](auto& rng, const auto& p) {
        return cppunivariate::distributions::rdblpareto(rng, p[0], p[1], p[2]);
      });
}

TEST_CASE("dblpareto: golden and all held subsets") {
  const auto g = mltest::load_distribution_golden("dblpareto");
  const double scale = g.estimates.at("scale"), left = g.estimates.at("left_rate"),
               right = g.estimates.at("right_rate");
  for (unsigned mask = 0; mask < 8; ++mask) {
    CAPTURE(mask);
    const auto r = cppunivariate::dblpareto_mle(mltest::as_span(g.data),
        mask & 1 ? scale : NAN, mask & 2 ? left : NAN, mask & 4 ? right : NAN);
    CHECK(cppunivariate::outcome(r).certified);
    CHECK(r.status == cppunivariate::DblparetoStatus::Ok);
    CHECK(r.scale == scale);
    CHECK(r.left_rate == doctest::Approx(left).epsilon(mltest::kGolden_dblpareto));
    CHECK(r.right_rate == doctest::Approx(right).epsilon(mltest::kGolden_dblpareto));
    CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_dblpareto));
  }
}

TEST_CASE("dblpareto: one-sided limits, collapse and fixed-rate interval") {
  const std::vector<double> v{1, 2, 2, 4};
  const auto x = mltest::as_span(v);
  const auto pr = cppunivariate::dblpareto_mle(x, 1);
  CHECK(pr.status == cppunivariate::DblparetoStatus::ParetoLimit);
  CHECK(pr.stop_detail.limit_coef[0] == doctest::Approx(1 / std::log(2)));
  const auto pw = cppunivariate::dblpareto_mle(x, 4, 3);
  CHECK(pw.status == cppunivariate::DblparetoStatus::PowerLimit);
  CHECK(pw.stop_detail.limit_coef == std::vector<double>{4, 3});
  const auto held = cppunivariate::dblpareto_mle(x, 1, 3);
  CHECK(held.status == cppunivariate::DblparetoStatus::Ok);
  const std::vector<double> single{5};
  const auto z = mltest::as_span(single);
  CHECK(cppunivariate::dblpareto_mle(z).status == cppunivariate::DblparetoStatus::Unbounded);
  CHECK(cppunivariate::dblpareto_mle(z, 5).status == cppunivariate::DblparetoStatus::Unbounded);
  CHECK(cppunivariate::dblpareto_mle(z, 5, 3).status == cppunivariate::DblparetoStatus::PowerLimit);
  CHECK(cppunivariate::dblpareto_mle(z, NAN, 3).status == cppunivariate::DblparetoStatus::PowerLimit);
  CHECK(cppunivariate::dblpareto_mle(z, 2).status == cppunivariate::DblparetoStatus::ParetoLimit);
  CHECK(cppunivariate::dblpareto_mle(z, NAN, 1, 1).status == cppunivariate::DblparetoStatus::Ok);

  const auto flat = cppunivariate::dblpareto_mle(x, NAN, 3, 1);
  CHECK_FALSE(cppunivariate::outcome(flat).unique);
  CHECK(flat.maximizers.contains({{1, 3, 1}}));
  CHECK(flat.maximizers.contains({{1.5, 3, 1}}));
  CHECK(flat.maximizers.contains({{2, 3, 1}}));
  CHECK_FALSE(flat.maximizers.contains({{3, 3, 1}}));
  const auto tied = cppunivariate::dblpareto_mle(x, NAN, 1, 1);
  CHECK(cppunivariate::outcome(tied).unique);
  CHECK(tied.scale == 2);
  const auto tilted = cppunivariate::dblpareto_mle(x, NAN, std::nextafter(3., 0.), 1);
  CHECK(cppunivariate::outcome(tilted).unique);
  CHECK(tilted.scale == 2);
}

TEST_CASE("dblpareto: exactly reflected samples can have two maximizing knots") {
  const std::vector<double> v{1./1024, .5, .5, 2, 2, 1024};
  const auto r = cppunivariate::dblpareto_mle(mltest::as_span(v));
  const auto o = cppunivariate::outcome(r);
  CHECK(o.certified);
  CHECK_FALSE(o.unique);
  REQUIRE(r.maximizers.size == 1);
  const auto& points = std::get<cppunivariate::Points>(r.maximizers.pieces[0].value);
  REQUIRE(points.size == 2);
  CHECK(points.values[0].estimate.values[0] == .5);
  CHECK(points.values[1].estimate.values[0] == 2);
  CHECK(points.values[0].estimate.values[1] == points.values[1].estimate.values[2]);

  const std::vector<double> two{1, 2};
  const auto endpoint = cppunivariate::dblpareto_mle(mltest::as_span(two));
  CHECK(cppunivariate::outcome(endpoint).kind == cppunivariate::OutcomeKind::Limit);
  CHECK(cppunivariate::outcome(endpoint).certified);
}

TEST_CASE("dblpareto: wide rate ratios and concentrated samples") {
  CHECK(cppunivariate::distributions::pdblpareto(1, 1, 1e300, 1e-300, true, true)
        == doctest::Approx(-600 * std::log(10.)));
  CHECK(cppunivariate::distributions::pdblpareto(
        std::nextafter(1., 0.), 1, 1e-300, 1e300, false, true) < -700);
  CHECK(cppunivariate::distributions::qdblpareto(-1e-300, 1, 1, 1e300, false, true)
        == doctest::Approx(1e-300).epsilon(1e-10).scale(0));

  const double b = 1e300;
  const std::vector<double> x{std::nextafter(b, 0.), b, std::nextafter(b, INFINITY)};
  const auto r = cppunivariate::dblpareto_mle(mltest::as_span(x), b);
  CHECK(r.status == cppunivariate::DblparetoStatus::Ok);
  CHECK(r.scale == b);
  CHECK(r.left_rate > 1e15);
  CHECK(r.right_rate > 1e15);
}

TEST_CASE("dblpareto: unseparated knot and face retain a finite witness") {
  const std::vector<double> data{.5, 1, 1, 1, 1, 16};
  // At scale 1, L = log(2), R = 4*log(2). The squared sum of their
  // roots equals the total right gap at scale .5, namely 9*log(2).
  const auto r = cppunivariate::dblpareto_mle(mltest::as_span(data));
  CHECK(r.status == cppunivariate::DblparetoStatus::Ok);
  CHECK(r.scale == 1);
  const auto o = cppunivariate::outcome(r);
  CHECK_FALSE(o.certified);
  CHECK_FALSE(o.uniqueness_known);
}
