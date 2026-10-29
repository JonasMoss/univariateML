#include "test_support.hpp"
#include "cppunivariate/cppunivariate.hpp"
#include "cppunivariate/distributions/std/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <random>
#include <vector>

#define C_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define C_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)

// rstd at (mean=0, sd=1, nu=8) is symmetric with sd 1; CDF at 0 = 0.5.
TEST_CASE("rstd samples have unit variance and a symmetric CDF") {
  cppunivariate::distributions::StdRng<> rng(19);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x = cppunivariate::distributions::rstd(rng, 0.0, 1.0, 8.0);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.05);
  CHECK(std::abs(var - 1.0) < 0.1);

  const double cdf0 =
      cppunivariate::distributions::pstd(0.0, 0.0, 1.0, 8.0, true, false);
  CHECK(std::abs(cdf0 - 0.5) < 1e-12);
}

TEST_CASE("std (Student-t with location/scale)") {
  auto g = mltest::load_distribution_golden("std");
  auto r = cppunivariate::student_t_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == C_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == C_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == C_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.logLik == C_APPROX_FD(g.logLik));

  cppunivariate::StudentTCost cost{mltest::as_span(g.data)};
  const double phi_hat[3] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0)};
  double grad[3];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm =
      std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] + grad[2] * grad[2]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("std compares normal and df-two faces") {
  std::vector<double> uniform;
  for (int i = 0; i < 200; ++i) uniform.push_back((i - 99.5) / 100.0);
  auto normal = cppunivariate::student_t_mle(mltest::as_span(uniform), mltest::kFitTol, 200);
  CHECK(normal.status == cppunivariate::StudentTStatus::NormalBoundary);
  std::vector<double> cluster;
  for (int i = 0; i < 30; ++i) cluster.push_back(-1e-4 + 2e-4 * i / 29.0);
  for (int i = 0; i < 10; ++i) cluster.push_back(0.9999 + 2e-4 * i / 9.0);
  auto two = cppunivariate::student_t_mle(mltest::as_span(cluster), mltest::kFitTol, 200);
  CHECK(two.status == cppunivariate::StudentTStatus::DfTwoBoundary);
  CHECK(two.logLik == doctest::Approx(54.07217335).epsilon(1e-7));
}

TEST_CASE("std large nu density retains precision") {
  // Independent high precision log-density values at z=1.25.
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1e8) ==
        doctest::Approx(-1.7001885430386572).epsilon(1e-12));
}

#undef C_APPROX
#undef C_APPROX_FD

TEST_CASE("std density across the gamma-ratio transition") {
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10.0) ==
        doctest::Approx(-1.813567692594348).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 100.0) ==
        doctest::Approx(-1.710151642368861).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 343.0) ==
        doctest::Approx(-1.7030664401294549).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 344.0) ==
        doctest::Approx(-1.7030580425140323).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1000.0) ==
        doctest::Approx(-1.7011732051102342).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 9999.0) ==
        doctest::Approx(-1.7002868956031254).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10000.0) ==
        doctest::Approx(-1.7002868857656135).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 100000.0) ==
        doctest::Approx(-1.7001983673162022).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 1000000.0) ==
        doctest::Approx(-1.7001895166043817).epsilon(2e-11));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 10000000.0) ==
        doctest::Approx(-1.7001886315445291).epsilon(2e-11));
}

TEST_CASE("std asymptotic cutoff is accurate") {
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 127.0) ==
        doctest::Approx(-1.7080116221314585).epsilon(1e-13));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 128.0) ==
        doctest::Approx(-1.7079498782526117).epsilon(1e-13));
  CHECK(cppunivariate::dens::log_dstd_unit(1.25, 129.0) ==
        doctest::Approx(-1.7078891013889226).epsilon(1e-13));
}

TEST_CASE("std: fixed nu maps ordinary t scale exactly") {
  const std::vector<double> x = {-5,-2,-1,0,0,1,2,9};
  auto r = cppunivariate::student_t_mle_fixed_nu(mltest::as_span(x),4,1e-8,5000);
  auto t = cppunivariate::lst_mle_fixed_df(mltest::as_span(x),4,1e-8,5000);
  REQUIRE(r.status == cppunivariate::StudentTStatus::Ok);
  CHECK(r.nu == 4);
  CHECK(r.mu == t.mu);
  CHECK(r.sigma == t.sigma*std::sqrt(2.0));
  CHECK(r.logLik == t.logLik);
  for (double nu : {-1.0,0.0,2.0,std::numeric_limits<double>::infinity()})
    CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(x),nu,1e-8,100).status == cppunivariate::StudentTStatus::InvalidNu);
  const std::vector<double> tied = {0,0,0,1};
  CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(tied),2.5,1e-8,100).status == cppunivariate::StudentTStatus::Unbounded);
  CHECK(cppunivariate::student_t_mle_fixed_nu(mltest::as_span(tied),3,1e-8,100).status == cppunivariate::StudentTStatus::ScaleBoundary);
}

TEST_CASE("std: free df tie collapse and equality") {
  for (int k : {5,6,7,8}) {
    std::vector<double> x(k,0.0); x.insert(x.end(),{1,2,3});
    const auto f=cppunivariate::student_t_mle(mltest::as_span(x),1e-8,200);
    if(k>6) CHECK(f.status==cppunivariate::StudentTStatus::Unbounded);
    if(k==6) CHECK(f.status==cppunivariate::StudentTStatus::ScaleBoundary);
    if(k==5) {
      CHECK(f.status!=cppunivariate::StudentTStatus::Unbounded);
      CHECK(f.status!=cppunivariate::StudentTStatus::ScaleBoundary);
    }
  }
}

TEST_CASE("std package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qstd(lp, 0.3, 1.7, 5.0, lower, true);
      CHECK(cppunivariate::distributions::pstd(x, 0.3, 1.7, 5.0, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("std: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("std",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dstd(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pstd(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qstd(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rstd(rng, p[0], p[1], p[2]); });
}

TEST_CASE("std fits are equivariant on small-scale samples") {
  const auto g = mltest::load_distribution_golden("std");
  const auto base = cppunivariate::student_t_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  for (double scale : {1e-6, 1e-2, 1e6}) {
    std::vector<double> x;
    for (double v : g.data) x.push_back(scale * (v + 3.0));
    const auto r = cppunivariate::student_t_mle(mltest::as_span(x), mltest::kFitTol, 200);
    CHECK(r.status == base.status);
    CHECK(r.mu / scale - 3.0 == doctest::Approx(base.mu).epsilon(1e-5));
    CHECK(r.sigma / scale == doctest::Approx(base.sigma).epsilon(1e-5));
    CHECK(r.nu == doctest::Approx(base.nu).epsilon(1e-5));
    CHECK(r.logLik + x.size() * std::log(scale) == doctest::Approx(base.logLik).epsilon(1e-9));
  }
}
