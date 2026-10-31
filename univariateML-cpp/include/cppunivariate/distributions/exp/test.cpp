#include <algorithm>
#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/exp/mle.hpp"
#include "cppunivariate/distributions/exp/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("exp") {
  auto g = mltest::load_distribution_golden("exp");
  auto r = cppunivariate::exp_mle(mltest::as_span(g.data));
  CHECK(r.rate == doctest::Approx(g.estimates.at("rate")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("exp: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("exp",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dexp(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pexp(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qexp(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rexp(rng, p[0]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// S=lambda x:m.exp(-x); Q=lambda l:-l
TEST_CASE("exp: extreme tail regressions") {
  CHECK(cppunivariate::distributions::pexp(1000,1,false,true) == doctest::Approx(-1000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qexp(-10000,1,false,true) == doctest::Approx(10000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::pexp(1e-300,1,true,true) == doctest::Approx(std::log(1e-300)).epsilon(1e-12).scale(1e-300));
}


TEST_CASE("exp P2 every held subset") {
  const auto g=mltest::load_distribution_golden("exp");
  std::vector<double> x=g.data;
  const std::vector<double> held{g.estimates.at("rate")};
  { const auto r=cppunivariate::exp_mle(mltest::as_span(x),held[0]);
    CHECK(r.rate==held[0]);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*x.size());
    auto ll=[&](const std::vector<double> &p){double s=0;for(double v:x)s+=cppunivariate::distributions::dexp(v,p[0],true);return s;};
    std::vector<double> p{r.rate};
    CHECK(ll(p)==doctest::Approx(r.logLik).epsilon(1e-8));
  }
}

TEST_CASE("exp: all-zero free rate is unbounded, held rate is finite") {
  const std::vector<double> x(4, 0.0);
  const auto r = cppunivariate::exp_mle(mltest::as_span(x));
  CHECK(r.status == cppunivariate::ExpStatus::Unbounded);
  CHECK(r.rate == std::numeric_limits<double>::infinity());
  CHECK(r.logLik == std::numeric_limits<double>::infinity());
  const auto delegated = cppunivariate::exp_mle(mltest::as_span(x),
      std::numeric_limits<double>::quiet_NaN());
  CHECK(delegated.status == cppunivariate::ExpStatus::Unbounded);
  const auto held = cppunivariate::exp_mle(mltest::as_span(x), 2.0);
  CHECK(held.status == cppunivariate::ExpStatus::Ok);
  CHECK(held.logLik == doctest::Approx(4 * std::log(2.0)));
}

TEST_CASE("exp: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0},std::vector<double>{-1.0,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::exp_mle(x).status != cppunivariate::ExpStatus::Ok);
  }
}

TEST_CASE("exp: TASK-132 a zero mixed with positive data is valid") {
  const std::vector<double> data{0,2};
  CHECK(cppunivariate::exp_mle(cppunivariate::span{data.data(),data.size()}).status == cppunivariate::ExpStatus::Ok);
}

#include "cppunivariate/distributions/exp/inference.hpp"

TEST_CASE("exp: TASK-102.5 inference analytic and invalid-input checks") {

  const std::vector<double> x{2};
  const auto ci=cppunivariate::exp_rate_exact_interval(mltest::as_span(x));
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  CHECK(ci.lower==doctest::Approx(-std::log(.975)/2));
  CHECK(ci.upper==doctest::Approx(-std::log(.025)/2));
  const std::vector<double> zero{0},negative{-1};
  CHECK(cppunivariate::exp_rate_exact_interval(mltest::as_span(zero)).status==cppunivariate::InferenceStatus::NumericalFailure);
  CHECK(cppunivariate::exp_rate_exact_interval(mltest::as_span(negative)).status==cppunivariate::InferenceStatus::InvalidObservation);

  const std::vector<double> empty;
  const cppunivariate::span data{empty.data(),empty.size()};
  CHECK(cppunivariate::exp_rate_exact_interval(data).status==cppunivariate::InferenceStatus::EmptySample);
  const std::vector<double> invalid{NAN,1,2};
  const cppunivariate::span bad{invalid.data(),invalid.size()};
  CHECK(cppunivariate::exp_rate_exact_interval(bad).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("exp: TASK-102.5 invalid confidence levels") {
  const std::vector<double> x{.2,.4,.6,.8};
  const auto data=mltest::as_span(x);
  for(double alpha:std::vector<double>{-1.,0.,1.,2.,NAN,INFINITY})
    CHECK(cppunivariate::exp_rate_exact_interval(data,alpha).status==cppunivariate::InferenceStatus::InvalidParameter);
}
