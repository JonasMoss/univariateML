#include "test_support.hpp"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include "cppunivariate/distributions/hcauchy/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("hcauchy: golden and exact pair") {
  const auto g=mltest::load_distribution_golden("hcauchy");
  const auto r=cppunivariate::hcauchy_mle(mltest::as_span(g.data));
  CHECK(r.status==cppunivariate::HcauchyStatus::Ok);
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(1e-10));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(1e-10));
  for(double factor:{1e-200,1.0,1e200}) {
    const std::vector<double> x={factor,4*factor};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).sigma/factor==doctest::Approx(2).epsilon(1e-11));
  }
}
TEST_CASE("hcauchy: zero multiplicities and invalid data") {
  const std::vector<double> a={0,1,2}, b={0,1}, c={0,0,1}, d={0}, e={-1,1}, f={};
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(a)).status==cppunivariate::HcauchyStatus::Ok);
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(b)).status==cppunivariate::HcauchyStatus::ScaleBoundary);
  for(const auto &x:{c,d}) CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::Unbounded);
  for(const auto &x:{e,f}) CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::InvalidData);
}
TEST_CASE("hcauchy: reference identities, tails and sampling") {
  CHECK(cppunivariate::distributions::dhcauchy(0,2,false)==doctest::Approx(1/std::acos(-1.0)));
  CHECK(std::isfinite(cppunivariate::distributions::dhcauchy(1e200,1,true)));
  CHECK(cppunivariate::distributions::phcauchy(-1,2,true,false)==0);
  for(bool lower:{false,true}) for(bool logs:{false,true}) for(double p:{0.,.01,.5,.99,1.}) {
    const double q=cppunivariate::distributions::qhcauchy(logs?std::log(p):p,2,lower,logs);
    CHECK(cppunivariate::distributions::phcauchy(q,2,lower,false)==doctest::Approx(p).epsilon(1e-11));
  }
  cppunivariate::distributions::StdRng<> rng(23);
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rhcauchy(rng,2)>=0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("hcauchy: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("hcauchy",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dhcauchy(x, p[0], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::phcauchy(x, p[0], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qhcauchy(x, p[0], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rhcauchy(rng, p[0]); });
}

TEST_CASE("hcauchy: held scale evaluates zeros") {
  const std::vector<double> x{0,0};
  auto r=cppunivariate::hcauchy_mle(mltest::as_span(x),2);
  CHECK(r.status==cppunivariate::HcauchyStatus::Ok);CHECK(r.sigma==2);
  CHECK(r.logLik==doctest::Approx(-2*std::log(std::acos(-1.0))));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// x=mp.mpf(1e100)
// print(mp.log(2*mp.atan(1/x)/mp.pi))
// print(mp.mpf(0))  # inverse upper tail 1
TEST_CASE("hcauchy: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qhcauchy(1.0, 1.0, false, false) == doctest::Approx(0.0).epsilon(1e-10));
  CHECK(cppunivariate::distributions::phcauchy(1e+100, 1.0, false, true) == doctest::Approx(-230.71009200469402).epsilon(1e-10));
}
