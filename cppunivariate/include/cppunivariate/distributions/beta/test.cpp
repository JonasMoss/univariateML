#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/beta/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

TEST_CASE("beta") {
  auto g = mltest::load_distribution_golden("beta");
  auto r = cppunivariate::beta_mle(mltest::as_span(g.data), 1.490116e-08, mltest::kFitMaxit);
  CHECK(r.alpha == C_APPROX(g.estimates.at("shape1")));
  CHECK(r.beta == C_APPROX(g.estimates.at("shape2")));
  CHECK(r.logLik == C_APPROX(g.logLik));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("beta dpqr: U(0,1) special case (shape1=shape2=1)") {
  CHECK(cppunivariate::distributions::dbeta(0.3, 1.0, 1.0, false) == doctest::Approx(1.0));
  CHECK(cppunivariate::distributions::pbeta(0.4, 1.0, 1.0, true, false) == doctest::Approx(0.4));
  CHECK(cppunivariate::distributions::qbeta(0.7, 1.0, 1.0, true, false) == doctest::Approx(0.7));
  // Asymmetric round-trip.
  const double q = cppunivariate::distributions::qbeta(0.6, 2.0, 5.0, true, false);
  CHECK(cppunivariate::distributions::pbeta(q, 2.0, 5.0, true, false) == doctest::Approx(0.6));
}

TEST_CASE("beta P2 held subsets and score roots") {
  const std::vector<double> x{.2,.2,.6,.8};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for (int mask=1;mask<4;++mask) {
    const auto r=cppunivariate::beta_mle(mltest::as_span(x),1e-12,100,
      mask&1?2:nan,mask&2?3:nan);
    REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
    if(mask&1) CHECK(r.alpha==2);
    else CHECK(std::abs(cppunivariate::mean_log(mltest::as_span(x))-
      cppunivariate::special::digamma(r.alpha)+cppunivariate::special::digamma(r.alpha+r.beta))<1e-11);
    if(mask&2) CHECK(r.beta==3);
    else {
      double score=0;
      for(double v:x) score+=std::log1p(-v);
      CHECK(std::abs(score/x.size()-cppunivariate::special::digamma(r.beta)+
        cppunivariate::special::digamma(r.alpha+r.beta))<1e-11);
    }
    double ll=0;
    for(double v:x) ll+=cppunivariate::distributions::dbeta(v,r.alpha,r.beta,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  }
  CHECK(cppunivariate::beta_mle(mltest::as_span(x),1e-12,1,2).status==cppunivariate::BetaStatus::IterationFailure);
  const std::vector<double> z(5,.4);
  CHECK(cppunivariate::beta_mle(mltest::as_span(z),1e-12,100,2).status==cppunivariate::BetaStatus::Ok);
}

TEST_CASE("beta P2 held shape retains tiny log complements") {
  const std::vector<double> x(5,1e-100);
  const auto r=cppunivariate::beta_mle(mltest::as_span(x),1e-12,100,1);
  REQUIRE(r.status==cppunivariate::BetaStatus::Ok);
  CHECK(r.beta==doctest::Approx(1e100).epsilon(1e-12));
  CHECK(r.logLik==doctest::Approx(5*(100*std::log(10.0)-1)).epsilon(1e-12));
}
