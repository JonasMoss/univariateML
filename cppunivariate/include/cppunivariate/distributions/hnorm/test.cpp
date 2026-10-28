#include "test_support.hpp"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include "cppunivariate/distributions/hnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>

TEST_CASE("hnorm: closed form and golden") {
  const auto g = mltest::load_distribution_golden("hnorm");
  const auto r = cppunivariate::hnorm_mle(mltest::as_span(g.data));
  CHECK(r.status == cppunivariate::HnormStatus::Ok);
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(1e-12));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(1e-12));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(1e-12));
  const std::vector<double> x = {-2, -2, 1, 2};
  const auto fit = cppunivariate::hnorm_mle(mltest::as_span(x));
  CHECK(fit.mu == -2);
  CHECK(fit.sigma == doctest::Approx(2.5));
  double ll = 0;
  for (double v : x) ll += cppunivariate::distributions::dhalfnorm(v, fit.mu, fit.sigma, true);
  CHECK(fit.logLik == doctest::Approx(ll));
}
TEST_CASE("hnorm: degeneracy and nonfinite inputs") {
  for (const std::vector<double> &x : {std::vector<double>{}, {1}, {2,2}})
    CHECK(cppunivariate::hnorm_mle(mltest::as_span(x)).status == cppunivariate::HnormStatus::DegenerateData);
  const std::vector<double> bad = {0, cppunivariate::special::kPosInf};
  CHECK(cppunivariate::hnorm_mle(mltest::as_span(bad)).status == cppunivariate::HnormStatus::InvalidData);
}
TEST_CASE("hnorm: d/p/q/r") {
  for (bool lower : {false,true}) for (bool logp : {false,true})
    for (double u : {0.0, 0.001, 0.2, 0.8, 1.0}) {
      const double p = logp ? std::log(u) : u;
      const double q = cppunivariate::distributions::qhalfnorm(p,-2,3,lower,logp);
      CHECK(cppunivariate::distributions::phalfnorm(q,-2,3,lower,false) == doctest::Approx(u).epsilon(1e-12));
    }
  CHECK(cppunivariate::distributions::dhalfnorm(-3,-2,3,false) == 0);
  CHECK(cppunivariate::distributions::dhalfnorm(-2,-2,3,false) > 0);
  double integral = 0;
  for (int i=0; i<10000; ++i)
    integral += 0.003 * cppunivariate::distributions::dhalfnorm(-2+(i+0.5)*0.003,-2,3,false);
  CHECK(integral == doctest::Approx(1.0).epsilon(1e-8));
  cppunivariate::distributions::StdRng<> rng(37);
  for (int i=0;i<100;++i) CHECK(cppunivariate::distributions::rhalfnorm(rng,-2,3) >= -2);
}
