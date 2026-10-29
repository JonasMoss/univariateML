#include "test_support.hpp"
#include "cppunivariate/distributions/unif/mle.hpp"
#include "cppunivariate/distributions/unif/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("unif") {
  auto g = mltest::load_distribution_golden("unif");
  auto r = cppunivariate::unif_mle(mltest::as_span(g.data));
  CHECK(r.min == doctest::Approx(g.estimates.at("min")).epsilon(mltest::kTol));
  CHECK(r.max == doctest::Approx(g.estimates.at("max")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("unif dpqr: density, CDF, quantile") {
  CHECK(cppunivariate::distributions::dunif(0.5, 0.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::punif(0.3, 0.0, 1.0, true, false) == doctest::Approx(0.3));
  CHECK(cppunivariate::distributions::qunif(0.8, 0.0, 1.0, true, false) == doctest::Approx(0.8));
}

TEST_CASE("unif P2 exact held endpoints and feasibility") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.2,.2,.8};
  for(int mask=1;mask<4;++mask) {
    const auto r=cppunivariate::unif_mle(mltest::as_span(x),mask&1?-1:nan,mask&2?2:nan);
    CHECK(r.status==cppunivariate::UnifStatus::Ok);
    CHECK(r.min==(mask&1?-1:.2));
    CHECK(r.max==(mask&2?2:.8));
    CHECK(r.logLik==doctest::Approx(-3*std::log(r.max-r.min)));
  }
  CHECK(cppunivariate::unif_mle(mltest::as_span(x),.3).status==cppunivariate::UnifStatus::Incompatible);
  CHECK(cppunivariate::unif_mle(mltest::as_span(x),nan,.7).status==cppunivariate::UnifStatus::Incompatible);
  CHECK(cppunivariate::unif_mle(mltest::as_span(x),2,1).status==cppunivariate::UnifStatus::InvalidInput);
  const std::vector<double> z(5,.4);
  CHECK(cppunivariate::unif_mle(mltest::as_span(z),.4).status==cppunivariate::UnifStatus::Unbounded);
  CHECK(cppunivariate::unif_mle(mltest::as_span(z),0).status==cppunivariate::UnifStatus::Ok);
}
