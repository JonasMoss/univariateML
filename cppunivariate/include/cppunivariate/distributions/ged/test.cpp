#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rged samples are symmetric around the mean; sample mean ~ 0, sd ~ 1 at
// (mean=0, sd=1), and the distribution lines up with the CDF at 0 (= 0.5).
TEST_CASE("rged samples are symmetric and match the CDF") {
  cppunivariate::distributions::StdRng<> rng(17);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rged(rng, 0.0, 1.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  // CDF at 0 should be exactly 0.5 (symmetric distribution).
  const double cdf0 =
      cppunivariate::distributions::pged(0.0, 0.0, 1.0, 1.5, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("ged (Generalized Error Distribution)") {
  auto g = mltest::load_distribution_golden("ged");
  auto r = cppunivariate::ged_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == C_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == C_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == C_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == C_APPROX_FD(g.logLik));

  cppunivariate::GedCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  if (r.nu > 1.0 && r.status == cppunivariate::GedStatus::Ok) CHECK(gnorm < 1e-3);
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("ged constrained faces and shapes") {
  std::vector<double> uniform, peaked;
  for (int i = 0; i < 40; ++i) uniform.push_back(-1.0 + 2.0*i/39.0);
  for (int i = 0; i < 18; ++i) peaked.push_back(-0.02 + 0.04*i/17.0);
  peaked.push_back(-10.0); peaked.push_back(10.0);
  auto u = cppunivariate::ged_mle(mltest::as_span(uniform), 1e-6, 500);
  CHECK(u.status == cppunivariate::GedStatus::UniformBoundary);
  auto l = cppunivariate::ged_mle(mltest::as_span(peaked), 1e-6, 500);
  CHECK(l.status == cppunivariate::GedStatus::Ok);
  CHECK(l.nu == 1.0);
  CHECK(l.nu >= 1.0);
  cppunivariate::distributions::StdRng<> rng(812);
  for (double shape : {1.5, 3.0}) {
    std::vector<double> x;
    for (int i = 0; i < 500; ++i)
      x.push_back(cppunivariate::distributions::rged(rng, 0.0, 1.0, shape));
    auto r = cppunivariate::ged_mle(mltest::as_span(x), 1e-6, 500);
    CHECK(r.status == cppunivariate::GedStatus::Ok);
    CHECK(r.nu > 1.0);
  }
}

TEST_CASE("ged package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qged(lp, 0.3, 1.7, 1.4, lower, true);
      CHECK(cppunivariate::distributions::pged(x, 0.3, 1.7, 1.4, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("ged: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("ged",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dged(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pged(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qged(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rged(rng, p[0], p[1], p[2]); });
}

TEST_CASE("ged: every held golden subset preserves coefficients and likelihood") {
  const auto g=mltest::load_distribution_golden("ged");
  const double nan=std::numeric_limits<double>::quiet_NaN();
  for(int mask=1;mask<8;++mask) {
    CAPTURE(mask);
    const auto r=cppunivariate::ged_mle(mltest::as_span(g.data),1e-8,1000,((mask & 1) ? g.estimates.at("mean") : nan),((mask & 2) ? g.estimates.at("sd") : nan),((mask & 4) ? g.estimates.at("nu") : nan));
    if(mask & 1)CHECK(r.mu==g.estimates.at("mean"));
    if(mask & 2)CHECK(r.sigma==g.estimates.at("sd"));
    if(mask & 4)CHECK(r.nu==g.estimates.at("nu"));
    CHECK(r.status==cppunivariate::GedStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    CHECK(r.logLik>=g.logLik-1e-6*g.data.size());
  }
}

TEST_CASE("ged: held variance uniform endpoint comparison") {
  const double endpoint=std::sqrt(3.0),nan=std::numeric_limits<double>::quiet_NaN();
  std::vector<double> x;for(int i=0;i<10;++i){x.push_back(-endpoint);x.push_back(endpoint);}
  CHECK(cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,0,1,nan).status==cppunivariate::GedStatus::UniformBoundary);
  CHECK(cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,nan,1,nan).status==cppunivariate::GedStatus::UniformBoundary);
}

TEST_CASE("ged: held profiles preserve extreme units") {
  const std::vector<double> x={-.02,-.005,0,.007,.03};
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const auto base=cppunivariate::ged_mle(mltest::as_span(x),1e-8,200,nan,nan,10000);
  for(double unit:{1e-300,1e300}) {
    auto y=x;for(double& v:y)v*=unit;
    const auto fit=cppunivariate::ged_mle(mltest::as_span(y),1e-8,200,nan,nan,10000);
    CHECK(fit.sigma/unit==doctest::Approx(base.sigma).epsilon(1e-8));
    CHECK(fit.logLik+x.size()*std::log(unit)==doctest::Approx(base.logLik).epsilon(1e-8));
  }
}


// Generating mpmath snippet (binary64 inputs retained exactly):
// import mpmath as mp, math; mp.mp.dps=100
// n=mp.mpf(3); s=mp.sqrt(mp.gamma(1/n)/mp.gamma(3/n))
// p=mp.mpf(float(-math.log(2))); mass=mp.expm1(p+mp.log(2))
// f=lambda y: mp.log(mp.gammainc(1/n,0,mp.exp(y))/mp.gamma(1/n))-mp.log(mass)
// y=mp.findroot(f,-110); print(s*mp.exp(y/n))
// g=lambda a: mp.log(mp.gammainc(1/n,a,mp.inf)/mp.gamma(1/n))+800-mp.log(2)
// print(s*mp.findroot(g,800)**(1/n))
TEST_CASE("ged: targeted tail and endpoint regressions") {
  CHECK(cppunivariate::distributions::qged(-0.6931471805599453, 0.0, 1.0, 3.0, true, true) == doctest::Approx(3.389474609308743e-17).epsilon(1e-10));
  CHECK(cppunivariate::distributions::qged(-800.0, 0.0, 1.0, 3.0, false, true) == doctest::Approx(15.155294491505197).epsilon(1e-10));
}
