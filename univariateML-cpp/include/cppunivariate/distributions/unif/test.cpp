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
  CHECK(r.min == doctest::Approx(g.estimates.at("min")).epsilon(mltest::kGolden_unif).scale(0));
  CHECK(r.max == doctest::Approx(g.estimates.at("max")).epsilon(mltest::kGolden_unif).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_unif).scale(0));
}

TEST_CASE("unif dpqr: density, CDF, quantile") {
  CHECK(cppunivariate::distributions::dunif(0.5, 0.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::punif(0.3, 0.0, 1.0, true, false) == doctest::Approx(0.3));
  CHECK(cppunivariate::distributions::qunif(0.8, 0.0, 1.0, true, false) == doctest::Approx(0.8));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("unif: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("unif",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dunif(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::punif(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qunif(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::runif(rng, p[0], p[1]); });
}

// mpmath reproduction: import mpmath as m; m.mp.dps=100
// logF=lambda x:m.log(x); Q=lambda l,w:w*m.exp(l)
TEST_CASE("unif: extreme tail regressions") {
  CHECK(cppunivariate::distributions::punif(1e-300,0,1,true,true) == doctest::Approx(std::log(1e-300)).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qunif(-800,0,1e300,true,true) == doctest::Approx(std::exp(-800+std::log(1e300))).epsilon(1e-12).scale(1e-300));
  CHECK(cppunivariate::distributions::qunif(-INFINITY,0,1,true,true) == doctest::Approx(0).epsilon(1e-12).scale(1e-300));
}

// mpmath: m.mp.dps=100; a=-m.mpf('1e308'); b=-a
// log(1/(b-a))=-log(2)-log(1e308); F(0)=1/2; Q(1/2)=0.
TEST_CASE("unif: finite endpoints with overflowing width") {
  CHECK(cppunivariate::distributions::dunif(0,-1e308,1e308,true) == doctest::Approx(-std::log(2.0)-std::log(1e308)).epsilon(1e-12));
  CHECK(cppunivariate::distributions::punif(0,-1e308,1e308,true,false) == doctest::Approx(.5));
  CHECK(cppunivariate::distributions::qunif(.5,-1e308,1e308,true,false) == 0);
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

TEST_CASE("unif: TASK-132 invalid data never report success") {
  for (const auto& data : {std::vector<double>{},std::vector<double>{NAN,2.0},std::vector<double>{INFINITY,2.0},std::vector<double>{-INFINITY,2.0}}) {
    const cppunivariate::span x{data.data(),data.size()};
    CHECK(cppunivariate::unif_mle(x).status != cppunivariate::UnifStatus::Ok);
  }
}

#include "cppunivariate/distributions/unif/inference.hpp"

TEST_CASE("unif: TASK-102.5 inference analytic and invalid-input checks") {

  const std::vector<double> x{.2,.8};
  const auto ci=cppunivariate::unif_exact_interval(mltest::as_span(x),cppunivariate::UniformTarget::Width);
  REQUIRE(ci.status==cppunivariate::InferenceStatus::Ok);
  CHECK(ci.lower==doctest::Approx(.6/(1-std::sqrt(.025))));
  CHECK(ci.upper==doctest::Approx(.6/(1-std::sqrt(.975))));
  const auto left=cppunivariate::unif_exact_interval(mltest::as_span(x),cppunivariate::UniformTarget::Minimum);
  const auto right=cppunivariate::unif_exact_interval(mltest::as_span(x),cppunivariate::UniformTarget::Maximum);
  CHECK(left.lower==doctest::Approx(.2-.6*(1/.025-1)));
  CHECK(left.upper==doctest::Approx(.2-.6*(1/.975-1)));
  CHECK(left.lower==doctest::Approx(1-right.upper));CHECK(left.upper==doctest::Approx(1-right.lower));
  const auto known=cppunivariate::unif_endpoint_known_opposite_interval(mltest::as_span(x),true,0);
  CHECK(known.lower==doctest::Approx(.8/std::sqrt(.975)));
  CHECK(known.upper==doctest::Approx(.8/std::sqrt(.025)));
  CHECK(cppunivariate::unif_endpoint_known_opposite_interval(mltest::as_span(x),true,.3).status==cppunivariate::InferenceStatus::InvalidObservation);

  const std::vector<double> empty;
  const cppunivariate::span data{empty.data(),empty.size()};
  CHECK(cppunivariate::unif_exact_interval(data,cppunivariate::UniformTarget::Width).status==cppunivariate::InferenceStatus::EmptySample);
  const std::vector<double> invalid{NAN,1,2};
  const cppunivariate::span bad{invalid.data(),invalid.size()};
  CHECK(cppunivariate::unif_exact_interval(bad,cppunivariate::UniformTarget::Width).status==cppunivariate::InferenceStatus::InvalidObservation);
}

TEST_CASE("unif: TASK-102.5 invalid confidence levels") {
  const std::vector<double> x{.2,.4,.6,.8};
  const auto data=mltest::as_span(x);
  for(double alpha:std::vector<double>{-1.,0.,1.,2.,NAN,INFINITY})
    CHECK(cppunivariate::unif_exact_interval(data,cppunivariate::UniformTarget::Width,alpha).status==cppunivariate::InferenceStatus::InvalidParameter);
}

// Independent central log-probability expansion; exact binary64 input.
TEST_CASE("unif: central inverse retains a translated residual") {
  CHECK(cppunivariate::distributions::qunif(-0.6931471805599453,-1,1,true,true) ==
        doctest::Approx(2.3190468138462996e-17).epsilon(1e-10).scale(0));
}
