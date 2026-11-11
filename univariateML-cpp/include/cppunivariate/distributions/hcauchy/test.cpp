#include "test_support.hpp"
#include "cppunivariate/distributions/hcauchy/mle.hpp"
#include "cppunivariate/distributions/hcauchy/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <vector>
TEST_CASE("hcauchy: golden and exact pair (mu held at 0)") {
  const auto g=mltest::load_distribution_golden("hcauchy");
  const auto r=cppunivariate::hcauchy_mle(mltest::as_span(g.data),0.0);
  CHECK(r.status==cppunivariate::HcauchyStatus::Ok);
  CHECK(r.mu==0.0);
  CHECK(r.sigma==doctest::Approx(g.estimates.at("sigma")).epsilon(mltest::kGolden_hcauchy).scale(0));
  CHECK(r.logLik==doctest::Approx(g.logLik).epsilon(mltest::kGolden_hcauchy).scale(0));
  for(double factor:{1e-200,1.0,1e200}) {
    const std::vector<double> x={factor,4*factor};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x),0.0).sigma/factor==doctest::Approx(2).epsilon(1e-11));
  }
}
TEST_CASE("hcauchy: zero multiplicities and invalid data (mu held at 0)") {
  const std::vector<double> a={0,1,2}, b={0,1}, c={0,0,1}, d={0}, e={-1,1}, f={};
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(a),0.0).status==cppunivariate::HcauchyStatus::Ok);
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(b),0.0).status==cppunivariate::HcauchyStatus::ScaleBoundary);
  for(const auto &x:{c,d}) CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x),0.0).status==cppunivariate::HcauchyStatus::Unbounded);
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(e),0.0).status==cppunivariate::HcauchyStatus::InvalidData);
  CHECK(cppunivariate::hcauchy_mle(mltest::as_span(f),0.0).status==cppunivariate::HcauchyStatus::InvalidData);
}
TEST_CASE("hcauchy: reference identities, tails and sampling") {
  CHECK(cppunivariate::distributions::dhcauchy(0,0,2,false)==doctest::Approx(1/std::acos(-1.0)));
  CHECK(std::isfinite(cppunivariate::distributions::dhcauchy(1e200,0,1,true)));
  CHECK(cppunivariate::distributions::phcauchy(-1,0,2,true,false)==0);
  for(bool lower:{false,true}) for(bool logs:{false,true}) for(double p:{0.,.01,.5,.99,1.}) {
    const double q=cppunivariate::distributions::qhcauchy(logs?std::log(p):p,0,2,lower,logs);
    CHECK(cppunivariate::distributions::phcauchy(q,0,2,lower,false)==doctest::Approx(p).epsilon(1e-11));
  }
  cppunivariate::distributions::StdRng<> rng(23);
  for(int i=0;i<20;++i) CHECK(cppunivariate::distributions::rhcauchy(rng,0,2)>=0);
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("hcauchy: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("hcauchy",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dhcauchy(x, p[0], p[1], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::phcauchy(x, p[0], p[1], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qhcauchy(x, p[0], p[1], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rhcauchy(rng, p[0], p[1]); });
}

TEST_CASE("hcauchy: held scale evaluates zeros (mu held at 0)") {
  const std::vector<double> x{0,0};
  auto r=cppunivariate::hcauchy_mle(mltest::as_span(x),0.0,2.0);
  CHECK(r.status==cppunivariate::HcauchyStatus::Ok);CHECK(r.sigma==2);
  CHECK(r.logLik==doctest::Approx(-2*std::log(std::acos(-1.0))));
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp; mp.mp.dps=100
// x=mp.mpf(1e100)
// print(mp.log(2*mp.atan(1/x)/mp.pi))
// print(mp.mpf(0))  # inverse upper tail 1
TEST_CASE("hcauchy: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qhcauchy(1.0,0, 1.0, false, false) == doctest::Approx(0.0).epsilon(1e-10));
  CHECK(cppunivariate::distributions::phcauchy(1e+100,0, 1.0, false, true) == doctest::Approx(-230.71009200469402).epsilon(1e-10));
}

// P4 (C5): free threshold, held threshold and their held-scale variants.
TEST_CASE("hcauchy: free threshold is the sample minimum, tie multiplicity vs n") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  { // 2k < n: an ordinary attained fit at mu = min(x).
    const std::vector<double> x={5,5,7,9,20};
    const auto r=cppunivariate::hcauchy_mle(mltest::as_span(x));
    CHECK(r.status==cppunivariate::HcauchyStatus::Ok);
    CHECK(r.mu==5.0);
    CHECK(std::isfinite(r.sigma));
    auto ll=[&](double s){double a=0;for(double v:x)a+=cppunivariate::distributions::dhcauchy(v,r.mu,s,true);return a;};
    CHECK(ll(r.sigma)==doctest::Approx(r.logLik).epsilon(1e-8));
    {const double h=1e-5*r.sigma;CHECK(std::abs((ll(r.sigma+h)-ll(r.sigma-h))/(2*h))<1e-3);}
  }
  { // 2k == n: a finite unattained scale-zero supremum.
    const std::vector<double> x={3,3,8,20};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::ScaleBoundary);
  }
  { // 2k > n: unbounded.
    const std::vector<double> x={3,3,3,8};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x)).status==cppunivariate::HcauchyStatus::Unbounded);
  }
  { // Held sigma removes the collapse at any tie multiplicity.
    const std::vector<double> x={3,3,3,8};
    const auto r=cppunivariate::hcauchy_mle(mltest::as_span(x),nan,2.0);
    CHECK(r.status==cppunivariate::HcauchyStatus::Ok);
    CHECK(r.mu==3.0);
    CHECK(r.sigma==2.0);
  }
  { // Held mu above an observation is invalid; below or at the minimum is fine.
    const std::vector<double> x={1,2,3};
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x),1.5).status==cppunivariate::HcauchyStatus::InvalidData);
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x),0.0).status==cppunivariate::HcauchyStatus::Ok);
    CHECK(cppunivariate::hcauchy_mle(mltest::as_span(x),1.0).status==cppunivariate::HcauchyStatus::Ok);
  }
}

TEST_CASE("hcauchy: README threshold shifts all four operations") {
  const double mu = 3.0;
  CHECK(cppunivariate::distributions::dhcauchy(2.0, mu, 2.0, false) == 0.0);
  CHECK(cppunivariate::distributions::qhcauchy(0.0, mu, 2.0, true, false) == mu);
  for (bool lower : {true, false}) for (bool logs : {true, false}) {
    CHECK(cppunivariate::distributions::dhcauchy(4.0, mu, 2.0, logs) == doctest::Approx(cppunivariate::distributions::dhcauchy(1.0, 0.0, 2.0, logs)));
    CHECK(cppunivariate::distributions::phcauchy(4.0, mu, 2.0, lower, logs) == doctest::Approx(cppunivariate::distributions::phcauchy(1.0, 0.0, 2.0, lower, logs)));
    CHECK(cppunivariate::distributions::qhcauchy((logs ? std::log(0.25) : 0.25), mu, 2.0, lower, logs) == doctest::Approx(mu + cppunivariate::distributions::qhcauchy((logs ? std::log(0.25) : 0.25), 0.0, 2.0, lower, logs)));
  }
  cppunivariate::distributions::StdRng<> a(94), b(94);
  for (int i = 0; i < 32; ++i)
    CHECK(cppunivariate::distributions::rhcauchy(a, mu, 2.0) == mu + cppunivariate::distributions::rhcauchy(b, 0.0, 2.0));
  CHECK(std::isnan(cppunivariate::distributions::dhcauchy(4.0, INFINITY, 2.0, true)));
}

TEST_CASE("hcauchy: exact-input extreme arithmetic regression") {
  CHECK(cppunivariate::distributions::qhcauchy(-0.6931471805599453,-1.0,1.0,true,true) == doctest::Approx(3.642750216855176e-17).epsilon(1e-10).scale(0));
}
