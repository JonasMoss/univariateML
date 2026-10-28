#include "test_support.hpp"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/lgamma/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("lgamma (wrapper-only: gamma on log(x))") {
  auto g = mltest::load_distribution_golden("lgamma");
  // Replicate mllgamma_ wrapper: y = log(x); shapelog/ratelog from gamma_mle;
  // logLik recomputed with the log-gamma density.
  std::vector<double> y;
  y.reserve(g.data.size());
  for (double v : g.data) y.push_back(std::log(v));
  auto r = cppunivariate::gamma_mle(mltest::as_span(y), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.shape == doctest::Approx(g.estimates.at("shapelog")).epsilon(mltest::kTol));
  CHECK(r.rate == doctest::Approx(g.estimates.at("ratelog")).epsilon(mltest::kTol));
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
  CHECK(logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

