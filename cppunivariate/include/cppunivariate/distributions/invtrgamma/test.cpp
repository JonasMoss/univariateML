// invtrgamma: golden, score gate, reciprocal relation, mirrored faces, fixed shapes.

#include "test_support.hpp"

#include "cppunivariate/distributions/invgamma/mle.hpp"
#include "cppunivariate/distributions/invtrgamma/dpqr.hpp"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"
#include "cppunivariate/distributions/invweibull/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/pareto/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <vector>

namespace {
double invtrgamma_loglik(const std::vector<double> &x, double a, double t, double s) {
  double ll = 0.0;
  for (double v : x) ll += cppunivariate::distributions::dinvtrgamma(v, a, t, s, true);
  return ll;
}
}  // namespace

TEST_CASE("invtrgamma: golden, score gate and reciprocal of trgamma") {
  auto g = mltest::load_distribution_golden("invtrgamma");
  auto r = cppunivariate::invtrgamma_mle(mltest::as_span(g.data));
  REQUIRE(r.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kTol));
  CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kTol));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
  CHECK(invtrgamma_loglik(g.data, r.shape1, r.shape2, r.scale) ==
        doctest::Approx(r.logLik).epsilon(1e-12));
  const double h = 1e-5;
  double score[3];
  for (int k = 0; k < 3; ++k) {
    double p[3] = {std::log(r.shape1), std::log(r.shape2), std::log(r.scale)};
    double m[3] = {p[0], p[1], p[2]};
    p[k] += h;
    m[k] -= h;
    score[k] = (invtrgamma_loglik(g.data, std::exp(p[0]), std::exp(p[1]), std::exp(p[2])) -
                invtrgamma_loglik(g.data, std::exp(m[0]), std::exp(m[1]), std::exp(m[2]))) / (2 * h);
  }
  CHECK(std::sqrt(score[0] * score[0] + score[1] * score[1] + score[2] * score[2]) < 1e-3);
  std::vector<double> y;
  double sum_log = 0;
  for (double v : g.data) {
    y.push_back(1 / v);
    sum_log += std::log(v);
  }
  auto f = cppunivariate::trgamma_mle(mltest::as_span(y));
  CHECK(f.shape1 == doctest::Approx(r.shape1).epsilon(1e-8));
  CHECK(f.shape2 == doctest::Approx(r.shape2).epsilon(1e-8));
  CHECK(f.scale == doctest::Approx(1 / r.scale).epsilon(1e-8));
  CHECK(r.logLik == doctest::Approx(f.logLik - 2 * sum_log).epsilon(1e-12));
}

TEST_CASE("invtrgamma: lognormal and Pareto faces") {
  const int n = 400;
  std::vector<double> lognormal, pareto;
  for (int i = 0; i < n; ++i) {
    const double p = (i + 0.5) / n;
    const double z = cppunivariate::special::qnorm(p, 0, 1, true, false);
    // Left-skewed log data: reciprocal of the trgamma lognormal-face sample.
    lognormal.push_back(std::exp(-(z + 0.05 * (z * z - 1))));
    pareto.push_back(2 * std::pow(1 - p, -1 / 1.7));
  }
  auto l = cppunivariate::invtrgamma_mle(mltest::as_span(lognormal));
  CHECK(l.status == cppunivariate::InvTrGammaStatus::LognormalBoundary);
  CHECK(l.logLik == doctest::Approx(cppunivariate::lognormal_mle(mltest::as_span(lognormal)).logLik)
                        .epsilon(1e-14));
  auto p = cppunivariate::invtrgamma_mle(mltest::as_span(pareto));
  const auto pm = cppunivariate::pareto_mle(mltest::as_span(pareto));
  CHECK(p.status == cppunivariate::InvTrGammaStatus::ParetoBoundary);
  CHECK(p.scale == doctest::Approx(pm.min).epsilon(1e-14));
  CHECK(p.logLik == doctest::Approx(pm.logLik).epsilon(1e-12));
}

TEST_CASE("invtrgamma: fixed shapes reduce to inverse gamma and inverse Weibull") {
  auto g = mltest::load_distribution_golden("invtrgamma");
  const auto x = mltest::as_span(g.data);
  auto fg = cppunivariate::invtrgamma_mle(x, 1e-10, 100, std::nan(""), 1.0);
  auto ig = cppunivariate::invgamma_mle(x, 1e-14, 100);
  REQUIRE(fg.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(fg.shape1 == doctest::Approx(ig.shape).epsilon(1e-10));
  CHECK(fg.scale == doctest::Approx(ig.scale).epsilon(1e-10));
  CHECK(fg.logLik == doctest::Approx(ig.logLik).epsilon(1e-12));
  auto fw = cppunivariate::invtrgamma_mle(x, 1e-12, 100, 1.0);
  auto iw = cppunivariate::invweibull_mle(x, 1e-12, 100);
  REQUIRE(fw.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(fw.shape2 == doctest::Approx(iw.shape).epsilon(1e-9));
  CHECK(fw.scale == doctest::Approx(1 / iw.rate).epsilon(1e-9));
  CHECK(fw.logLik == doctest::Approx(iw.logLik).epsilon(1e-12));
  auto both = cppunivariate::invtrgamma_mle(x, 1e-12, 100, 2.5, 0.8);
  CHECK(both.logLik == doctest::Approx(invtrgamma_loglik(g.data, 2.5, 0.8, both.scale)).epsilon(1e-12));
  CHECK(cppunivariate::invtrgamma_mle(x, 1e-12, 100, 0.0).status ==
        cppunivariate::InvTrGammaStatus::InvalidFixed);
}

TEST_CASE("invtrgamma: dpqr") {
  namespace d = cppunivariate::distributions;
  const double q = d::qinvtrgamma(0.3, 2, 1.5, 0.5, true, false);
  CHECK(d::pinvtrgamma(q, 2, 1.5, 0.5, true, false) == doctest::Approx(0.3).epsilon(1e-12));
  CHECK(d::dinvtrgamma(0.7, 2, 1.5, 0.5, true) ==
        doctest::Approx(d::dtrgamma(1 / 0.7, 2, 1.5, 2, true) - 2 * std::log(0.7)).epsilon(1e-13));
  cppunivariate::distributions::StdRng<> rng(9);
  for (int i = 0; i < 100; ++i) CHECK(d::rinvtrgamma(rng, 2, 1.5, 0.5) > 0);
}

TEST_CASE("invtrgamma: both shapes fixed on constant data and stable density") {
  const std::vector<double> x = {0.5, 0.5};
  const auto r = cppunivariate::invtrgamma_mle(mltest::as_span(x), 1e-10, 100, 3, 2);
  REQUIRE(r.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(r.scale == doctest::Approx(std::sqrt(3.0)/2));
  CHECK(r.logLik == doctest::Approx(invtrgamma_loglik(x, 3, 2, r.scale)).epsilon(1e-12));
  const double a = 1e10;
  CHECK(cppunivariate::distributions::dinvtrgamma(1, a, 1, a, true) ==
        doctest::Approx(0.5 * std::log(a) - 0.91893853320467274178 - 1/(12*a)).epsilon(1e-12));
  CHECK(std::isnan(cppunivariate::distributions::dinvtrgamma(std::nan(""), 2, 1, 1, false)));
}

TEST_CASE("invtrgamma: evaluation close to the Pareto face") {
  namespace d = cppunivariate::distributions;
  const double q = d::qinvtrgamma(.1, 1e-6, 1e6, 1, true, false);
  CHECK(q > 1.1);
  CHECK(q < 1.12);
  CHECK(d::pinvtrgamma(q, 1e-6, 1e6, 1, true, false) == doctest::Approx(.1).epsilon(1e-12));
  cppunivariate::distributions::StdRng<> rng(7);
  for (int i=0; i<20; ++i) CHECK(std::isfinite(d::rinvtrgamma(rng, 1e-6, 1e6, 1)));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("invtrgamma: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("invtrgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dinvtrgamma(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pinvtrgamma(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qinvtrgamma(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rinvtrgamma(rng, p[0], p[1], p[2]); });
}

TEST_CASE("P2 invtrgamma golden holds") {
 auto g=mltest::load_distribution_golden("invtrgamma");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<4;++mask){double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan;auto r=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(g.data),g.estimates.at("scale"),a,b);REQUIRE(r.status==cppunivariate::InvTrGammaStatus::Ok);
 CHECK(r.scale==g.estimates.at("scale"));
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 CHECK(r.logLik>=g.logLik-1e-5);}
}

TEST_CASE("invtrgamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("invtrgamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::invtrgamma_mle(x,1e-8,1).status==cppunivariate::InvTrGammaStatus::IterationFailure);
}
