#include "test_support.hpp"
#include "cppunivariate/distributions/logis/mle.hpp"
#include "cppunivariate/distributions/logis/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <string>
#include <vector>


TEST_CASE("logis") {
  auto g = mltest::load_distribution_golden("logis");
  auto r = cppunivariate::logistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("location")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("logis: seeded sample with outlier") {
  auto g = mltest::load_golden(std::string(CPPUNIV_DISTRIBUTIONS_DIR) +
      "/logis/fixture-outlier.json");
  auto r = cppunivariate::logistic_mle(mltest::as_span(g.data), mltest::kFitTol, mltest::kFitMaxit);
  CHECK(r.mu == doctest::Approx(g.estimates.at("first")).epsilon(1e-4));
  CHECK(r.scale == doctest::Approx(g.estimates.at("second")).epsilon(1e-4));
  CHECK(r.logLik >= g.logLik - 1e-6);
  CHECK(r.status == cppunivariate::LogisticStatus::Ok);
}

TEST_CASE("logis: invalid inputs never report success") {
  for (const std::vector<double> &x : {std::vector<double>{}, {1}, {1, 1},
       {0, std::numeric_limits<double>::infinity()}}) {
    const auto r = cppunivariate::logistic_mle(mltest::as_span(x), mltest::kFitTol, mltest::kFitMaxit);
    CHECK(r.status == cppunivariate::LogisticStatus::InvalidInput);
  }
}

TEST_CASE("logis dpqr: density at location = 1/(4*scale)") {
  CHECK(cppunivariate::distributions::dlogis(0.0, 0.0, 1.0, false) == doctest::Approx(0.25));
  CHECK(cppunivariate::distributions::plogis(0.0, 0.0, 1.0, true, false) == doctest::Approx(0.5));
  const double q = cppunivariate::distributions::qlogis(0.7, 0.0, 1.0, true, false);
  CHECK(cppunivariate::distributions::plogis(q, 0.0, 1.0, true, false) == doctest::Approx(0.7));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("logis: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("logis",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlogis(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plogis(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlogis(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlogis(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logS=lambda x:-x-m.log1p(m.exp(-x)); Q=lambda l:l-m.log1p(-m.exp(l))
TEST_CASE("logis: extreme tail regressions") {
  CHECK(cppunivariate::distributions::plogis(1000,0,1,false,true) == doctest::Approx(-1000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qlogis(-10000,0,1,true,true) == doctest::Approx(-10000).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::dlogis(-1e300,0,1,true) == doctest::Approx(-1e300).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=100; p=m.exp(m.mpf(float(-math.log(2))))
// Invert the family's CDF at this p, retaining p-m.mpf('.5').
TEST_CASE("logis: rounded log half retains median offset") {
  CHECK(cppunivariate::distributions::qlogis(-std::log(2.0),0,1,true,true) == doctest::Approx(4.6380936276925993e-17).epsilon(1e-12));
  CHECK(cppunivariate::distributions::qlogis(-std::log(2.0),0,1,false,true) == doctest::Approx(-4.6380936276925993e-17).epsilon(1e-12));
}


TEST_CASE("logis: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("logis");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<4;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::logistic_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("location") : nan),((mask & 2) ? g.estimates.at("scale") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("location"));
    if(mask & 2)CHECK(r.scale==g.estimates.at("scale"));
    CHECK(r.status==cppunivariate::LogisticStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("logis: held profiles preserve extreme units") {
  const std::vector<double> x={-2,-.5,0,.7,3};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::logistic_mle(mltest::as_span(x),1e-8,200,0,nan);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::logistic_mle(mltest::as_span(y),1e-8,200,0,nan);
    CHECK(fit.scale/unit==doctest::Approx(base.scale).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
}
