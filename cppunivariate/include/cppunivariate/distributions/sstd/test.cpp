#include "test_support.hpp"
#include "cppunivariate/distributions/sstd/mle.hpp"
#include "cppunivariate/distributions/sstd/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include "cppunivariate/special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>
namespace dist = cppunivariate::distributions;
#define D_APPROX(x) doctest::Approx(x).epsilon(mltest::kTol)
#define D_APPROX_FD(x) doctest::Approx(x).epsilon(mltest::kFdTol)
TEST_CASE("sstd (skew Student-t)") {
  auto g = mltest::load_distribution_golden("sstd");
  auto r = cppunivariate::sstd_mle(mltest::as_span(g.data), mltest::kFitTol, 200);
  CHECK(r.mu == D_APPROX_FD(g.estimates.at("mean")));
  CHECK(r.sigma == D_APPROX_FD(g.estimates.at("sd")));
  CHECK(r.nu == D_APPROX_FD(g.estimates.at("nu")));
  CHECK(r.xi == D_APPROX_FD(g.estimates.at("xi")));
  CHECK(r.logLik == D_APPROX_FD(g.logLik));

  cppunivariate::SstdCost cost{mltest::as_span(g.data)};
  const double phi_hat[4] = {r.mu, std::log(r.sigma), std::log(r.nu - 2.0),
                             std::log(r.xi)};
  double grad[4];
  cppunivariate::finite_diff_gradient(cost, phi_hat, grad);
  const double gnorm = std::sqrt(grad[0] * grad[0] + grad[1] * grad[1] +
                                 grad[2] * grad[2] + grad[3] * grad[3]);
  CHECK(gnorm < 1e-3);
}

TEST_CASE("rsstd samples are unit-variance and recurse cleanly into rstd") {
  cppunivariate::distributions::StdRng<> rng(31);
  const int n = 20000;
  double sum = 0.0;
  double sum_sq = 0.0;
  for (int i = 0; i < n; ++i) {
    const double x =
        cppunivariate::distributions::rsstd(rng, 0.0, 1.0, 8.0, 1.5);
    sum += x;
    sum_sq += x * x;
  }
  const double mean = sum / n;
  const double var = sum_sq / n - mean * mean;
  CHECK(std::abs(mean) < 0.15);
  CHECK(std::abs(var - 1.0) < 0.15);
}

namespace {
// Deterministic samples: quantiles at (i + 1/2) / n.
template <class Q> std::vector<double> quantile_sample(int n, Q q) {
  std::vector<double> x;
  for (int i = 0; i < n; ++i) x.push_back(q((i + 0.5) / n));
  return x;
}
cppunivariate::SstdResult fit(const std::vector<double> &x) {
  return cppunivariate::sstd_mle(mltest::as_span(x), mltest::kFitTol, 200);
}
// Translated half-t log-likelihood at endpoint e, scale s, df nu, via Boost.
double half_t_loglik(const std::vector<double> &x, double e, double s, double nu) {
  double ll = 0.0;
  for (double v : x) {
    ll += std::log(2.0 / s) + cppunivariate::special::dt(std::abs(v - e) / s, nu, true);
  }
  return ll;
}
}  // namespace

TEST_CASE("sstd: normal data escape to the skew-normal face") {
  const auto x = quantile_sample(200, [](double p) {
    return cppunivariate::special::qnorm(p, 0.0, 1.0, true, false);
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::SnormBoundary);
  CHECK(std::isinf(r.nu));
  const auto sn = cppunivariate::snorm_mle(mltest::as_span(x), mltest::kFitTol, 200);
  CHECK(r.logLik == D_APPROX(sn.logLik));
  CHECK(r.xi == D_APPROX(sn.xi));
}

TEST_CASE("sstd: half-normal data escape to the half-normal corner") {
  const auto x = quantile_sample(200, [](double p) {
    return cppunivariate::special::qnorm(0.5 + 0.5 * p, 0.0, 1.0, true, false);
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::RightHalfNormalBoundary);
  CHECK(r.mu == *std::min_element(x.begin(), x.end()));
  CHECK(std::isinf(r.xi));
}

TEST_CASE("sstd: exponential data escape to a translated half-t") {
  const auto x = quantile_sample(200, [](double p) { return -std::log1p(-p); });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::RightHalfTBoundary);
  CHECK(r.mu == *std::min_element(x.begin(), x.end()));
  CHECK(std::isinf(r.xi));
  CHECK(r.nu > 2.0);
  CHECK(r.nu < 10.0);
  // Independent log-likelihood; the profile is maximal at (sigma, nu).
  CHECK(r.logLik == D_APPROX(half_t_loglik(x, r.mu, r.sigma, r.nu)));
  for (double fs : {0.99, 1.01}) {
    for (double fn : {0.95, 1.0, 1.05}) {
      if (fs == 1.0 && fn == 1.0) continue;
      CHECK(half_t_loglik(x, r.mu, fs * r.sigma, fn * r.nu) < r.logLik);
    }
  }
  CHECK(half_t_loglik(x, r.mu, r.sigma, 1e6 * r.nu) < r.logLik);

  // Reflected data: the left face with the same df and log-likelihood.
  std::vector<double> y;
  for (double v : x) y.push_back(-v);
  const auto l = fit(y);
  CHECK(l.status == cppunivariate::SstdStatus::LeftHalfTBoundary);
  CHECK(l.xi == 0.0);
  CHECK(l.logLik == D_APPROX(r.logLik));
  CHECK(l.nu == doctest::Approx(r.nu).epsilon(1e-5));
}

TEST_CASE("sstd: skewed Cauchy data escape to the df-2 face") {
  const auto x = quantile_sample(200, [](double p) {
    const double c = std::tan(3.14159265358979323846 * (p - 0.5));
    return c > 0.0 ? 2.0 * c : c;
  });
  const auto r = fit(x);
  CHECK(r.status == cppunivariate::SstdStatus::DfTwoBoundary);
  CHECK(r.nu == 2.0);
  CHECK(std::isinf(r.sigma));
  CHECK(r.xi > 1.0);
}

TEST_CASE("sstd: large-df interior fits do not overflow") {
  // tgamma in sstd_sigma used to overflow above nu = 343.
  double mu_u = 0.0;
  const double sigma_u = cppunivariate::dens::sstd_sigma(1e4, 1.5, mu_u);
  CHECK(std::isfinite(sigma_u));
  CHECK(std::isfinite(mu_u));
  const double m1 = std::sqrt(2.0 / 3.14159265358979323846);  // normal limit
  CHECK(mu_u == doctest::Approx(m1 * (1.5 - 1.0 / 1.5)).epsilon(1e-3));
}

TEST_CASE("sstd: free df tie collapse and equality") {
  for (int k : {5,6,7,8}) {
    std::vector<double> x(k,0.0); x.insert(x.end(),{-1,1,2});
    const auto f=cppunivariate::sstd_mle(mltest::as_span(x),1e-8,200);
    if(k>6) CHECK(f.status==cppunivariate::SstdStatus::Unbounded);
    if(k==6) CHECK(f.status==cppunivariate::SstdStatus::ScaleBoundary);
    if(k==5) {
      CHECK(f.status!=cppunivariate::SstdStatus::Unbounded);
      CHECK(f.status!=cppunivariate::SstdStatus::ScaleBoundary);
    }
  }
}

TEST_CASE("sstd: half t scale root strict inequality and finite equality supremum") {
  double logscale=0;
  const std::vector<double> strict={0,0,0,0,0,0,0,1,2,3};
  CHECK(cppunivariate::sstd_detail::half_t_profile(strict,.5,logscale)==INFINITY);
  const std::vector<double> equality={0,0,0,0,0,0,1,2,3};
  const double ll=cppunivariate::sstd_detail::half_t_profile(equality,.5,logscale);
  CHECK(std::isfinite(ll));
  CHECK(logscale==-INFINITY);
  // At nu=2 the scale powers cancel, leaving the analytic limit.
  CHECK(ll==doctest::Approx(-3*std::log(6.0)).epsilon(1e-12));
  const std::vector<double> regular={0,0,0,0,0,1,2,3};
  CHECK(std::isfinite(cppunivariate::sstd_detail::half_t_profile(regular,.5,logscale)));
  CHECK(std::isfinite(logscale));
}

TEST_CASE("sstd package dpqr preserve logarithmic tails") {
  for (bool lower : {false, true}) {
    for (double lp : {-1000.0, -100.0, -2.0, -0.1}) {
      const double x = cppunivariate::distributions::qsstd(lp, 0.3, 1.7, 5.0, 1.8, lower, true);
      CHECK(cppunivariate::distributions::psstd(x, 0.3, 1.7, 5.0, 1.8, lower, true) == doctest::Approx(lp).epsilon(2e-12));
    }
  }
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("sstd: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("sstd",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dsstd(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::psstd(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qsstd(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rsstd(rng, p[0], p[1], p[2], p[3]); });
}
