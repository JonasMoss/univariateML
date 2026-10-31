#include <limits>
#include "test_support.hpp"
#include "cppunivariate/distributions/laplace/mle.hpp"
#include "cppunivariate/distributions/laplace/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rlaplace samples are symmetric around mu, var = 2*sigma^2") {
  cppunivariate::distributions::StdRng<> rng(101);
  const int n = 20000;
  double sum = 0.0, sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rlaplace(rng, 1.0, 2.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean - 1.0) < 0.1);
  CHECK(std::abs(var - 8.0) < 0.5); // 2*sigma^2 = 8
  // CDF at mu should be exactly 0.5.
  CHECK(std::abs(
            cppunivariate::distributions::plaplace(1.0, 1.0, 2.0, true, false) -
            0.5) < 1e-12);
}

TEST_CASE("laplace") {
  auto g = mltest::load_distribution_golden("laplace");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("laplace: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) + "/laplace/fixture-outlier.json");
  auto r = cppunivariate::laplace_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("laplace: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("laplace",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlaplace(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plaplace(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlaplace(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlaplace(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:-x-m.log(2); Q=lambda l:l+m.log(2)
TEST_CASE("laplace: extreme tail regressions") {
  CHECK(cppunivariate::distributions::plaplace(1000,0,1,false,true) == doctest::Approx(-1000-std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qlaplace(-10000,0,1,true,true) == doctest::Approx(-10000+std::log(2.0)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qlaplace(-10000,0,1,false,true) == doctest::Approx(10000-std::log(2.0)).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(2))))
// Invert the family's CDF at this p, retaining p-m.mpf('.5').
TEST_CASE("laplace: rounded log half retains median offset") {
  CHECK(cppunivariate::distributions::qlaplace(-std::log(2.0),0,1,true,true) == doctest::Approx(2.3190468138462997e-17).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qlaplace(-std::log(2.0),0,1,false,true) == doctest::Approx(-2.3190468138462997e-17).epsilon(1e-12));
}


TEST_CASE("laplace: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("laplace");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::laplace_mle(mltest::as_span(g.data),((mask & 1) ? g.estimates.at("mu") : nan),((mask & 2) ? g.estimates.at("sigma") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mu"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sigma"));
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("laplace: held scale removes constant-sample collapse") {
  const std::vector<double> x={2,2,2};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  CHECK(cppunivariate::laplace_mle(mltest::as_span(x),2,nan).status==cppunivariate::LaplaceStatus::Unbounded);
  CHECK(cppunivariate::laplace_mle(mltest::as_span(x),nan,1).status==cppunivariate::LaplaceStatus::Ok);
}

TEST_CASE("laplace: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::laplace_mle(x).status != cppunivariate::LaplaceStatus::Ok);
  }
}

TEST_CASE("laplace: TASK-132 unresolved likelihood has numerical failure status") {
  const std::vector<double> data{1e308,1e308};
  const cppunivariate::span x{data.data(),data.size()};
  CHECK(cppunivariate::laplace_mle(x,0.0,1e-300).status == cppunivariate::LaplaceStatus::IterationFailure);
}

#include "cppunivariate/distributions/laplace/inference.hpp"

TEST_CASE("laplace: TASK-102.5 inference analytic and invalid-input checks") {

  const std::vector<double> x{1,2,3,4,5,6,7,8,9,10};
  const auto sign=cppunivariate::median_sign_interval(mltest::as_span(x));
  REQUIRE(sign.status==cppunivariate::InferenceStatus::Ok);
  CHECK(sign.lower==2);CHECK(sign.upper==9); // Coverage = 1-22/1024.
  const std::vector<double> one{2};
  const auto unbounded=cppunivariate::median_sign_interval(mltest::as_span(one));
  CHECK(unbounded.status==cppunivariate::InferenceStatus::Ok);
  CHECK(unbounded.lower==-INFINITY);CHECK(unbounded.upper==INFINITY);
  const auto s=cppunivariate::median_sandwich(mltest::as_span(x));
  REQUIRE(s.status==cppunivariate::InferenceStatus::Ok);
  CHECK(s.median==6);CHECK(s.bandwidth==doctest::Approx(1.06*3/std::log(2.)*std::pow(10.,-.2)));
  CHECK(s.uncertainty.variance==doctest::Approx(1/(40*s.density*s.density)));
  std::vector<double> y;for(double v:x)y.push_back(7+3*v);
  const auto t=cppunivariate::median_sandwich(mltest::as_span(y));
  CHECK(t.bandwidth==doctest::Approx(3*s.bandwidth));CHECK(t.density==doctest::Approx(s.density/3));
  CHECK(t.uncertainty.variance==doctest::Approx(9*s.uncertainty.variance));
  const auto ci=cppunivariate::laplace_location_wald_interval(mltest::as_span(x));
  CHECK((ci.lower+ci.upper)/2==doctest::Approx(6));
  CHECK((ci.upper-ci.lower)/2==doctest::Approx(1.959963984540054*2.5/std::sqrt(10.)));
  const std::vector<double> flat{2,2,2};
  CHECK(cppunivariate::median_sandwich(mltest::as_span(flat)).status==cppunivariate::InferenceStatus::NumericalFailure);

  const std::vector<double> empty;
  const cppunivariate::span data{empty.data(),empty.size()};
  CHECK(cppunivariate::median_sign_interval(data).status==cppunivariate::InferenceStatus::EmptySample);
  const std::vector<double> invalid{NAN,1,2};
  const cppunivariate::span bad{invalid.data(),invalid.size()};
  CHECK(cppunivariate::median_sign_interval(bad).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("laplace: TASK-102.5 invalid confidence levels") {
  const std::vector<double> x{.2,.4,.6,.8};
  const auto data=mltest::as_span(x);
  for(double alpha:std::vector<double>{-1.,0.,1.,2.,NAN,INFINITY})
    CHECK(cppunivariate::median_sign_interval(data,alpha).status==cppunivariate::InferenceStatus::InvalidParameter);
}

TEST_CASE("laplace: TASK-102.5 underflowed uncertainty fails") {
  const std::vector<double> x{-2e-200,-1e-200,1e-200,2e-200};
  CHECK(cppunivariate::laplace_location_wald_interval(mltest::as_span(x)).status==cppunivariate::InferenceStatus::NumericalFailure);
  CHECK(cppunivariate::median_sandwich(mltest::as_span(x)).status==cppunivariate::InferenceStatus::NumericalFailure);
}
