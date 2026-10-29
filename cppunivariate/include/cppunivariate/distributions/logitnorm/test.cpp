#include "test_support.hpp"
#include "cppunivariate/distributions/logitnorm/mle.hpp"
#include "cppunivariate/distributions/logitnorm/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <string>
#include <vector>

TEST_CASE("rlogitnorm samples lie in (0, 1) and match the CDF at median") {
  cppunivariate::distributions::StdRng<> rng(7);
  const int n = 5000;
  double sum = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rlogitnorm(rng, 0.0, 1.0);
    CHECK(x > 0.0);
    CHECK(x < 1.0);
    sum += x;
  }
  // mu=0 means logit(X) ~ N(0, 1), so median(X) = logistic(0) = 0.5.
  // Empirical mean is biased toward 0.5 but with high spread; sanity-check
  // the CDF at 0.5 returns 0.5.
  const double cdf_half =
      cppunivariate::distributions::plogitnorm(0.5, 0.0, 1.0, true, false);
  CHECK(std::abs(cdf_half - 0.5) < 1e-12);
  CHECK(std::abs(sum / n - 0.5) < 0.05);
}

TEST_CASE("logitnorm") {
  auto g = mltest::load_distribution_golden("logitnorm");
  auto r = cppunivariate::logitnorm_mle(mltest::as_span(g.data));
  CHECK(r.mu == doctest::Approx(g.estimates.at("mu")).epsilon(mltest::kTol));
  CHECK(r.sigma == doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("logitnorm dpqr support and extreme log probabilities") {
  CHECK(cppunivariate::distributions::plogitnorm(-1,0,1)==0);
  CHECK(cppunivariate::distributions::plogitnorm(2,0,1)==1);
  CHECK(cppunivariate::distributions::dlogitnorm(-1,0,1)==0);
  CHECK(cppunivariate::distributions::qlogitnorm(0,0,1)==0);
  CHECK(cppunivariate::distributions::qlogitnorm(1,0,1)==1);
  CHECK(std::isnan(cppunivariate::distributions::dlogitnorm(.5,0,-1)));
  for (bool lower : {true,false}) {
    for (double lp : {-1000.0,-10000.0,-1e-20}) {
      const double x=cppunivariate::distributions::qlogitnorm(lp,0,.01,lower,true);
      CHECK(cppunivariate::distributions::plogitnorm(x,0,.01,lower,true)==doctest::Approx(lp).epsilon(1e-10));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("logitnorm: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("logitnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dlogitnorm(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::plogitnorm(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qlogitnorm(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rlogitnorm(rng, p[0], p[1]); });
}

TEST_CASE("logitnorm P2 exact held normal fits") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{.2,.5,.8};
  for(int mask=1;mask<4;++mask) {
    const auto r=cppunivariate::logitnorm_mle(mltest::as_span(x),mask&1?.3:nan,mask&2?1.2:nan);
    if(mask&1) CHECK(r.mu==.3);
    else CHECK(std::abs(r.mu)<1e-15);
    if(mask&2) CHECK(r.sigma==1.2);
    double ll=0;
    for(double v:x) ll+=cppunivariate::distributions::dlogitnorm(v,r.mu,r.sigma,true);
    CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-12));
  }
  const std::vector<double> z(5,.5);
  CHECK(std::isnan(cppunivariate::logitnorm_mle(mltest::as_span(z),0).sigma));
  CHECK(cppunivariate::logitnorm_mle(mltest::as_span(z),1).sigma==1);
  CHECK(cppunivariate::logitnorm_mle(mltest::as_span(z),nan,2).mu==0);
}
