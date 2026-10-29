#include "test_support.hpp"
#include "cppunivariate/distributions/sged/mle.hpp"
#include "cppunivariate/distributions/sged/dpqr.hpp"
#include "cppunivariate/distributions/ged/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
TEST_CASE("sged (skew GED)") {
  auto g = mltest::load_distribution_golden("sged");
  auto r = cppunivariate::sged_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == D_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == D_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == D_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == D_APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == D_APPROX_FD(g.logLik));

  cppunivariate::SgedCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  if (r.nu > 1.0 && r.status == cppunivariate::SgedStatus::Ok) CHECK(gnorm < 1e-3);
}

TEST_CASE("rsged samples are unit-variance and recurse cleanly into rged") {
  cppunivariate::distributions::StdRng<> rng(29);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsged(rng, 0.0, 1.0, 1.5, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);
}

TEST_CASE("sged constrained faces and shapes") {
  std::vector<double> uniform, peaked;
  for (int i = 0; i < 40; ++i) uniform.push_back(-1.0 + 2.0*i/39.0);
  for (int i = 0; i < 18; ++i) peaked.push_back(-0.02 + 0.04*i/17.0);
  peaked.push_back(-10.0); peaked.push_back(10.0);
  auto u = cppunivariate::sged_mle(mltest::as_span(uniform), 1e-6, 500);
  CHECK(u.status == cppunivariate::SgedStatus::UniformBoundary);
  auto l = cppunivariate::sged_mle(mltest::as_span(peaked), 1e-6, 500);
  CHECK(l.status == cppunivariate::SgedStatus::Ok);
  CHECK(l.nu == 1.0);
  CHECK(l.nu >= 1.0);
  cppunivariate::distributions::StdRng<> rng(812);
  for (double shape : {1.5, 3.0}) {
    std::vector<double> x;
    for (int i = 0; i < 500; ++i)
      x.push_back(cppunivariate::distributions::rged(rng, 0.0, 1.0, shape));
    auto r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
    CHECK(r.status == cppunivariate::SgedStatus::Ok);
    CHECK(r.nu > 1.0);
  }
}

TEST_CASE("sged translated exponential faces") {
  std::vector<double> x;
  for (int i = 0; i < 100; ++i) x.push_back(-std::log((i+0.5)/100.0));
  auto r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
  CHECK(r.status == cppunivariate::SgedStatus::RightHalfGedBoundary);
  for (double &v : x) v = -v;
  r = cppunivariate::sged_mle(mltest::as_span(x), 1e-6, 500);
  CHECK(r.status == cppunivariate::SgedStatus::LeftHalfGedBoundary);
}

TEST_CASE("sged package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsged(lp, 0.3, 1.7, 1.4, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psged(x, 0.3, 1.7, 1.4, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sged: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sged",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsged(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psged(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsged(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsged(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("sged: classified holds preserve values and handle tied/singleton data") {
  const double nan=std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{-2,-1,-.4,0,.2,.8,1,1.3,2,3};
  const auto data=mltest::as_span(x);
  for(bool mean : {false,true}) for(bool sd : {false,true}) {
    if(!mean && !sd) continue;
    const auto r=cppunivariate::sged_mle(data,1e-9,300,mean ? .2 : nan,sd ? 1.7 : nan,4.2,1.3);
    REQUIRE(r.status==cppunivariate::SgedStatus::Ok);
    CHECK(std::isfinite(r.logLik));
    if(mean) CHECK(r.mu==.2);
    if(sd) CHECK(r.sigma==1.7);
    CHECK(r.xi==1.3);
    CHECK(r.nu==4.2);
  }
  const std::vector<double> one{.2};
  const auto location=cppunivariate::sged_mle(mltest::as_span(one),1e-9,300,nan,1.7,4.2,1.3);
  CHECK(location.status==cppunivariate::SgedStatus::Ok);
  const auto collapse=cppunivariate::sged_mle(mltest::as_span(one),1e-9,300,.2,nan,4.2,1.3);
  CHECK(collapse.status==cppunivariate::SgedStatus::Unbounded);
}

TEST_CASE("sged: P3 restrictions and closed faces") {
 const std::vector<double> x{-100,-2,-.01,0,.01,2,100};const auto data=mltest::as_span(x);
 const auto f=cppunivariate::sged_mle(data,1e-8,1000); REQUIRE(f.status==cppunivariate::SgedStatus::Ok);
 CHECK(f.nu==1);CHECK(f.restricted==16u);
 const auto h=cppunivariate::sged_mle(data,1e-8,1000,0,1,1,1);CHECK(h.restricted==0u);
}
