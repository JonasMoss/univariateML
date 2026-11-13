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
  CHECK(r.mu == doctest::Approx(g.estimates.at("location")).epsilon(mltest::kGolden_logis).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_logis).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_logis).scale(0));
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
  for (const std::vector<double> &x : {std::vector<double>{},
       {0, std::numeric_limits<double>::infinity()}}) {
    const auto r = cppunivariate::logistic_mle(mltest::as_span(x), mltest::kFitTol, mltest::kFitMaxit);
    CHECK(r.status == cppunivariate::LogisticStatus::InvalidInput);
  }
  // Constant samples are a scale collapse, not invalid input (TASK-214.3).
  for (const std::vector<double> &x : {std::vector<double>{1}, {1, 1}}) {
    const auto r = cppunivariate::logistic_mle(mltest::as_span(x), mltest::kFitTol, mltest::kFitMaxit);
    CHECK(r.status == cppunivariate::LogisticStatus::Unbounded);
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

TEST_CASE("logis: an absolute tol below the location's resolution; constant data collapse") {
  // TASK-214.3. (1e-8, 1, 1e8) at tol 1e-10 used to end in IterationFailure
  // after 100 iterations: the absolute location step cannot fall below 1e-10
  // near 2.6e7. Reference: 50-digit score equations (fix-nonfinite data).
  const std::vector<double> x{1e-8,1,1e8};
  const auto r=cppunivariate::logistic_mle(mltest::as_span(x),1e-10,100);
  REQUIRE(r.status==cppunivariate::LogisticStatus::Ok);
  CHECK(r.mu==doctest::Approx(26275335.36836199439710978).epsilon(1e-12));
  CHECK(r.scale==doctest::Approx(28619993.76342037092873051).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(-57.41148262342058850475964).epsilon(1e-13));
  // Constant data, and a held location every observation equals: the scale
  // collapses (used to be InvalidInput).
  const std::vector<double> c{2,2,2};
  const auto rc=cppunivariate::logistic_mle(mltest::as_span(c),1e-10,100);
  CHECK(rc.status==cppunivariate::LogisticStatus::Unbounded);
  CHECK(cppunivariate::outcome(rc).kind==cppunivariate::OutcomeKind::Nonexistence);
  CHECK(cppunivariate::outcome(rc).reason==cppunivariate::OutcomeReason::Unbounded);
  const auto rh=cppunivariate::logistic_mle(mltest::as_span(c),1e-10,100,2.0,NAN);
  CHECK(rh.status==cppunivariate::LogisticStatus::Unbounded);
  CHECK(cppunivariate::logistic_mle(mltest::as_span(c),1e-10,100,1.0,NAN).status==cppunivariate::LogisticStatus::Ok);
  CHECK(cppunivariate::logistic_mle(mltest::as_span(c),1e-10,100,NAN,1.0).status==cppunivariate::LogisticStatus::Ok);
}

TEST_CASE("logis: held scale, location bracket at adjacent doubles") {
  // Archive case 4719 (offset 1e8, scale held at 1.008): the width 1e-12*s
  // is below the spacing near 1e8, so the bisection stalled into
  // IterationFailure. Signs certified at adjacent doubles now end it.
  const auto read=[](std::initializer_list<const char*> words) {
    std::vector<double> v;for(const char* w:words)v.push_back(std::strtod(w,nullptr));return v;
  };
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto x=read({"0x1.7d783f609d127p+26","0x1.7d7840110d012p+26","0x1.7d7840000db82p+26",
      "0x1.7d783fc2b056ap+26","0x1.7d7840351a593p+26","0x1.7d783f61e5240p+26","0x1.7d7840acdca3bp+26",
      "0x1.7d78407da7befp+26","0x1.7d783fce71df4p+26","0x1.7d783fdce8cc5p+26","0x1.7d78402f7065cp+26",
      "0x1.7d78407729152p+26"});
  const auto r=cppunivariate::logistic_mle(mltest::as_span(x),mltest::kFitTol,mltest::kFitMaxit,nan,1.0081430589945095);
  REQUIRE(r.status==cppunivariate::LogisticStatus::Ok);
  CHECK(std::abs(r.mu-100000000.116207034405182)<=4*(std::nextafter(1e8,2e8)-1e8));
  CHECK(r.logLik==doctest::Approx(-22.93818887915537710812935).epsilon(1e-13));
  // Archive case 4718 (spread 1e150, scale 1): the computed score is exactly
  // zero between the middle order statistics, so no bracket certifies.
  const auto y=read({"-0x1.8588474a3c164p+499","0x1.4d5ec9a539d1fp+496","0x1.0c3bdee89a5a3p+488",
      "-0x1.2baecb9b165f4p+498","0x1.038fca903d996p+498","-0x1.82667ef7aeb8ap+499","0x1.a677329ce2dc6p+499",
      "0x1.3318554c44384p+499","-0x1.e47100bd6e2b8p+497","-0x1.5709f3c33f45cp+497","0x1.cfc127e5d264cp+497",
      "0x1.2338de5d01383p+499"});
  CHECK(cppunivariate::logistic_mle(mltest::as_span(y),mltest::kFitTol,mltest::kFitMaxit,nan,1.0081430589945095).status!=
        cppunivariate::LogisticStatus::Ok);
}
