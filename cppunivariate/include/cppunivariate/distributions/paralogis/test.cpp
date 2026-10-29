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

TEST_CASE("paralogis: historical fixed scale golden and equivariance") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/paralogis/fixed-scale-golden.json");
  auto r = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), 1);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shape")).epsilon(1e-9));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-10));
  CHECK(r.scale == 1);
  for (auto &v : g.data) v *= 7;
  auto moved = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), 7);
  CHECK(moved.shape == doctest::Approx(r.shape).epsilon(1e-10));
  CHECK(moved.logLik == doctest::Approx(r.logLik-g.data.size()*std::log(7.0)).epsilon(1e-10));
  for (double bad : {0.0, -1.0, double(INFINITY), double(NAN)})
    CHECK(cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(g.data), bad).status == cppunivariate::ParalogisStatus::InvalidStart);
}

TEST_CASE("paralogis: fixed-scale independent scipy oracle") {
  // scipy minimization of the reference log density, in log parameters.
  std::vector<double> x{.03,.1,.4,.7,1,2,4,12};
  auto r = cppunivariate::paralogis_mle_fixed_scale(mltest::as_span(x), 1);
  CHECK(r.logLik == doctest::Approx(-13.915168948543954).epsilon(1e-10));
}

TEST_CASE("P2 paralogis golden holds") {
 auto g=mltest::load_distribution_golden("paralogis");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<2;++mask){auto r=cppunivariate::paralogis_mle_holds(mltest::as_span(g.data),g.estimates.at("shape"),mask?g.estimates.at("scale"):nan);
 CHECK(r.shape==g.estimates.at("shape"));
 CHECK(r.logLik>=g.logLik-1e-5);}
}
