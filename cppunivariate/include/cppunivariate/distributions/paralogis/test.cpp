// paralogis: golden regression, score residual, start value.

#include "test_support.hpp"

#include "cppunivariate/core/finite_diff.hpp"
#include "cppunivariate/distributions/paralogis/mle.hpp"

#include <cmath>

TEST_CASE("paralogis: golden and score residual") {
  auto g = mltest::load_distribution_golden("paralogis");
  auto r = cppunivariate::paralogis_mle(mltest::as_span(g.data), mltest::kFitTol,
                                        mltest::kFitMaxit);
  CHECK(r.status == cppunivariate::ParalogisStatus::Ok);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(mltest::kFdTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kFdTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kFdTol));

  // A parameter silently held fixed (the old 1-D fit held scale at 1)
  // leaves a non-zero score component; both components must vanish.
  cppunivariate::ParalogisCost cost{mltest::as_span(g.data)};
  const double phi_hat[2] = {std::log(r.shape), std::log(r.scale)};
  double grad[2];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  CHECK(std::sqrt(grad[0] * grad[0] + grad[1] * grad[1]) < 1e-3);
}

TEST_CASE("paralogis: an explicit shape start reaches the same fit") {
  auto g = mltest::load_distribution_golden("paralogis");
  const auto x = mltest::as_span(g.data);
  auto a = cppunivariate::paralogis_mle(x, 1e-8, 500);
  auto b = cppunivariate::paralogis_mle(x, 1e-8, 500, 2.0 * a.shape);
  CHECK(b.status == cppunivariate::ParalogisStatus::Ok);
  CHECK(b.logLik == doctest::Approx(a.logLik).epsilon(1e-9));
  CHECK(b.shape == doctest::Approx(a.shape).epsilon(1e-4));
}
