// trgamma: golden, score gate, faces, fixed shapes, reciprocal fit and a
// dense-grid profile reference.

#include "test_support.hpp"

#include "cppunivariate/distributions/gamma/mle.hpp"
#include "cppunivariate/distributions/invtrgamma/mle.hpp"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include "cppunivariate/distributions/power/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include "cppunivariate/distributions/trgamma/dpqr.hpp"
#include "cppunivariate/distributions/trgamma/mle.hpp"
#include "cppunivariate/distributions/weibull/mle.hpp"

#include <cmath>
#include <random>
#include <vector>

namespace {

double trgamma_loglik(const std::vector<double> &x, double a, double t, double s) {
  double ll = 0.0;
  for (double v : x) ll += cppunivariate::distributions::dtrgamma(v, a, t, s, true);
  return ll;
}

// Central-difference score of the full log likelihood in log parameters.
double score_norm(const std::vector<double> &x, double a, double t, double s) {
  const double h = 1e-7;
  double g[3];
  for (int k = 0; k < 3; ++k) {
    double p[3] = {std::log(a), std::log(t), std::log(s)}, m[3] = {p[0], p[1], p[2]};
    p[k] += h;
    m[k] -= h;
    g[k] = (trgamma_loglik(x, std::exp(p[0]), std::exp(p[1]), std::exp(p[2])) -
            trgamma_loglik(x, std::exp(m[0]), std::exp(m[1]), std::exp(m[2]))) / (2 * h);
  }
  return std::sqrt(g[0] * g[0] + g[1] * g[1] + g[2] * g[2]);
}

std::vector<double> uniforms(std::mt19937_64 &engine, int n) {
  std::vector<double> u;
  for (int i = 0; i < n; ++i)
    u.push_back((static_cast<double>(engine() >> 11) + 0.5) / 9007199254740992.0);
  return u;
}

// Naive profile log likelihood at shape2 = tau (log-sum-exp, direct digamma).
double naive_profile(const std::vector<double> &x, double tau) {
  const double n = static_cast<double>(x.size());
  long double ybar = 0;
  for (double v : x) ybar += std::log(v) / n;
  long double m = -1e300L;
  for (double v : x) m = std::max(m, tau * (std::log(v) - ybar));
  long double s = 0;
  for (double v : x) s += std::exp(tau * (std::log(v) - ybar) - m);
  const double K = static_cast<double>(m + std::log(s / n));
  double b = std::log(0.5 / K);
  for (int i = 0; i < 200; ++i) {
    const double a = std::exp(b);
    const double f = std::log(a) - cppunivariate::special::digamma(a) - K;
    const double df = 1 - a * cppunivariate::special::trigamma(a);
    const double step = std::max(-2.0, std::min(2.0, f / df));
    b -= step;
    if (std::abs(step) < 1e-15) break;
  }
  const double a = std::exp(b);
  return n * (a * std::log(a) - a - std::lgamma(a) - a * K + std::log(tau) -
              static_cast<double>(ybar));
}

// Dense log grid plus golden-section refinement of the best cell.
double dense_reference(const std::vector<double> &x, double lo, double hi) {
  const int cells = 4000;
  int best = 0;
  std::vector<double> ll(cells + 1);
  for (int j = 0; j <= cells; ++j) {
    ll[j] = naive_profile(x, std::exp(lo + (hi - lo) * j / cells));
    if (ll[j] > ll[best]) best = j;
  }
  double a = lo + (hi - lo) * std::max(0, best - 1) / cells;
  double b = lo + (hi - lo) * std::min(cells, best + 1) / cells;
  for (int k = 0; k < 120; ++k) {
    const double c = b - (b - a) * 0.6180339887498949, d = a + (b - a) * 0.6180339887498949;
    if (naive_profile(x, std::exp(c)) > naive_profile(x, std::exp(d))) b = d; else a = c;
  }
  return std::max(ll[best], naive_profile(x, std::exp(0.5 * (a + b))));
}

}  // namespace

TEST_CASE("trgamma: golden and score gate") {
  auto g = mltest::load_distribution_golden("trgamma");
  auto r = cppunivariate::trgamma_mle(mltest::as_span(g.data));
  REQUIRE(r.status == cppunivariate::TrGammaStatus::Ok);
  CHECK(r.shape1 == doctest::Approx(g.estimates.at("shape1")).epsilon(mltest::kGolden_trgamma).scale(0));
  CHECK(r.shape2 == doctest::Approx(g.estimates.at("shape2")).epsilon(mltest::kGolden_trgamma).scale(0));
  CHECK(r.scale == doctest::Approx(g.estimates.at("scale")).epsilon(mltest::kGolden_trgamma).scale(0));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kGolden_trgamma).scale(0));
  CHECK(trgamma_loglik(g.data, r.shape1, r.shape2, r.scale) ==
        doctest::Approx(r.logLik).epsilon(1e-12));
  CHECK(score_norm(g.data, r.shape1, r.shape2, r.scale) < 1e-3);
}

TEST_CASE("trgamma: faces from limit-model data") {
  const int n = 400;
  std::vector<double> lognormal, power;
  for (int i = 0; i < n; ++i) {
    const double p = (i + 0.5) / n;
    // Right-skewed log data: lognormal quantiles with a positive cubic tilt.
    const double z = cppunivariate::special::qnorm(p, 0, 1, true, false);
    lognormal.push_back(std::exp(z + 0.05 * (z * z - 1)));
    power.push_back(3 * std::pow(p, 1 / 2.5));
  }
  auto l = cppunivariate::trgamma_mle(mltest::as_span(lognormal));
  CHECK(l.status == cppunivariate::TrGammaStatus::LognormalBoundary);
  CHECK(l.logLik == doctest::Approx(cppunivariate::lognormal_mle(mltest::as_span(lognormal)).logLik)
                        .epsilon(1e-14));
  auto p = cppunivariate::trgamma_mle(mltest::as_span(power));
  CHECK(p.status == cppunivariate::TrGammaStatus::PowerBoundary);
  CHECK(p.scale == doctest::Approx(power.back()).epsilon(1e-14));
  CHECK(p.logLik == doctest::Approx(cppunivariate::power_mle(mltest::as_span(power), 0.0).logLik)
                        .epsilon(1e-12));
  const std::vector<double> three = {1, 2, 3};
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(three)).status ==
        cppunivariate::TrGammaStatus::PowerBoundary);
}

TEST_CASE("trgamma: interior close to the lognormal face and scale range") {
  // Left-skewed log data: an interior maximum with a large shape1.
  const int n = 4000;
  std::vector<double> x, tight;
  for (int i = 0; i < n; ++i) {
    const double z = cppunivariate::special::qnorm((i + 0.5) / n, 0, 1, true, false);
    x.push_back(std::exp(z - 0.001 * (z * z - 1)));
    tight.push_back(std::exp(1e-3 * (z - 0.001 * (z * z - 1))));
  }
  auto r = cppunivariate::trgamma_mle(mltest::as_span(x));
  const double ln = cppunivariate::lognormal_mle(mltest::as_span(x)).logLik;
  CHECK(r.status == cppunivariate::TrGammaStatus::ScaleOutOfRange);
  CHECK(r.logLik > ln + 1e-8 * n);
  CHECK(r.shape1 > 1e4);
  // Shrinking sd(log x) by 1e-3 keeps shape1 and makes the scale representable.
  auto s = cppunivariate::trgamma_mle(mltest::as_span(tight));
  REQUIRE(s.status == cppunivariate::TrGammaStatus::Ok);
  CHECK(s.shape1 == doctest::Approx(r.shape1).epsilon(1e-6));
  CHECK(s.shape2 == doctest::Approx(1e3 * r.shape2).epsilon(1e-6));
  CHECK(score_norm(tight, s.shape1, s.shape2, s.scale) < 1e-2);
}

TEST_CASE("trgamma: fixed shapes reduce to gamma and Weibull fits") {
  auto g = mltest::load_distribution_golden("trgamma");
  const auto x = mltest::as_span(g.data);
  auto fg = cppunivariate::trgamma_mle(x, 1e-10, 100, std::nan(""), 1.0);
  auto gm = cppunivariate::gamma_mle(x, 1e-14, 100);
  REQUIRE(fg.status == cppunivariate::TrGammaStatus::Ok);
  CHECK(fg.shape2 == 1.0);
  CHECK(fg.shape1 == doctest::Approx(gm.shape).epsilon(1e-10));
  CHECK(fg.scale == doctest::Approx(gm.scale).epsilon(1e-10));
  CHECK(fg.logLik == doctest::Approx(gm.logLik).epsilon(1e-12));
  auto fw = cppunivariate::trgamma_mle(x, 1e-12, 100, 1.0);
  auto wb = cppunivariate::weibull_mle(x, 1e-12, 100);
  REQUIRE(fw.status == cppunivariate::TrGammaStatus::Ok);
  CHECK(fw.shape1 == 1.0);
  CHECK(fw.shape2 == doctest::Approx(wb.shape).epsilon(1e-9));
  CHECK(fw.scale == doctest::Approx(wb.scale).epsilon(1e-9));
  CHECK(fw.logLik == doctest::Approx(wb.logLik).epsilon(1e-12));
  // Fixed shape2: x^shape2 is gamma, with the Jacobian n log(tau) + (tau-1) sum(log x).
  const double tau = 0.7;
  std::vector<double> z;
  double jac = g.data.size() * std::log(tau);
  for (double v : g.data) { z.push_back(std::pow(v, tau)); jac += (tau - 1) * std::log(v); }
  auto gz = cppunivariate::gamma_mle(mltest::as_span(z), 1e-14, 100);
  auto ft = cppunivariate::trgamma_mle(x, 1e-10, 100, std::nan(""), tau);
  CHECK(ft.shape1 == doctest::Approx(gz.shape).epsilon(1e-10));
  CHECK(ft.scale == doctest::Approx(std::pow(gz.scale, 1 / tau)).epsilon(1e-10));
  CHECK(ft.logLik == doctest::Approx(gz.logLik + jac).epsilon(1e-12));
  // Fixed shape1: score in shape2 and scale vanishes; both fixed: scale score.
  for (double a : {0.05, 0.6, 3.0, 40.0}) {
    auto fa = cppunivariate::trgamma_mle(x, 1e-12, 100, a);
    REQUIRE(fa.status == cppunivariate::TrGammaStatus::Ok);
    const double h = 1e-5;
    const double d2 = (trgamma_loglik(g.data, a, fa.shape2 * std::exp(h), fa.scale) -
                       trgamma_loglik(g.data, a, fa.shape2 * std::exp(-h), fa.scale)) / (2 * h);
    const double d3 = (trgamma_loglik(g.data, a, fa.shape2, fa.scale * std::exp(h)) -
                       trgamma_loglik(g.data, a, fa.shape2, fa.scale * std::exp(-h))) / (2 * h);
    CHECK(std::hypot(d2, d3) < 1e-3);
    CHECK(fa.logLik == doctest::Approx(trgamma_loglik(g.data, a, fa.shape2, fa.scale)).epsilon(1e-12));
    auto both = cppunivariate::trgamma_mle(x, 1e-12, 100, a, fa.shape2);
    CHECK(both.scale == doctest::Approx(fa.scale).epsilon(1e-10));
    CHECK(both.logLik == doctest::Approx(fa.logLik).epsilon(1e-12));
  }
}

TEST_CASE("trgamma: reciprocal, scale and power equivariance") {
  auto g = mltest::load_distribution_golden("trgamma");
  auto r = cppunivariate::trgamma_mle(mltest::as_span(g.data));
  std::vector<double> inv, moved;
  double sum_log = 0;
  for (double v : g.data) {
    inv.push_back(1 / v);
    moved.push_back(5 * std::pow(v, 0.25));
    sum_log += std::log(v);
  }
  auto i = cppunivariate::invtrgamma_mle(mltest::as_span(inv));
  REQUIRE(i.status == cppunivariate::InvTrGammaStatus::Ok);
  CHECK(i.shape1 == doctest::Approx(r.shape1).epsilon(1e-8));
  CHECK(i.shape2 == doctest::Approx(r.shape2).epsilon(1e-8));
  CHECK(i.scale == doctest::Approx(1 / r.scale).epsilon(1e-8));
  CHECK(i.logLik == doctest::Approx(r.logLik + 2 * sum_log).epsilon(1e-12));
  // 5 x^(1/4) is trgamma(shape1, 4 shape2, 5 scale^(1/4)).
  auto m = cppunivariate::trgamma_mle(mltest::as_span(moved));
  const double jac = g.data.size() * std::log(0.25 * 5) - 0.75 * sum_log;
  CHECK(m.shape1 == doctest::Approx(r.shape1).epsilon(1e-8));
  CHECK(m.shape2 == doctest::Approx(4 * r.shape2).epsilon(1e-8));
  CHECK(m.scale == doctest::Approx(5 * std::pow(r.scale, 0.25)).epsilon(1e-8));
  CHECK(m.logLik == doctest::Approx(r.logLik - jac).epsilon(1e-10));
}

TEST_CASE("trgamma: seeded samples agree with a dense-grid profile reference") {
  std::mt19937_64 engine(20261029);
  const double params[][3] = {{2, 1.5, 2}, {0.3, 4, 1}, {8, 0.4, 3}, {1, 1, 1}, {50, 0.2, 1e-3}};
  for (const auto &p : params)
    for (int n : {8, 30, 150})
      for (int rep = 0; rep < 4; ++rep) {
        std::vector<double> x;
        for (double u : uniforms(engine, n))
          x.push_back(cppunivariate::distributions::qtrgamma(u, p[0], p[1], p[2], true, false));
        CAPTURE(p[0]); CAPTURE(n); CAPTURE(rep);
        const auto fit = cppunivariate::trgamma_mle(mltest::as_span(x));
        double sd = 0, mean = 0;
        for (double v : x) mean += std::log(v) / n;
        for (double v : x) sd += (std::log(v) - mean) * (std::log(v) - mean) / n;
        sd = std::sqrt(sd);
        const double ref = dense_reference(x, std::log(1e-3 / sd), std::log(1e5 / sd));
        const double ln = cppunivariate::lognormal_mle(mltest::as_span(x)).logLik;
        const double pw = cppunivariate::power_mle(mltest::as_span(x), 0.0).logLik;
        const double face = std::max(ln, pw);
        CHECK(fit.logLik >= ref - 1e-8);
        if (ref > face + 1e-8 * n + 1e-6) {
          CHECK((fit.status == cppunivariate::TrGammaStatus::Ok ||
                 fit.status == cppunivariate::TrGammaStatus::ScaleOutOfRange));
          CHECK(fit.logLik == doctest::Approx(ref).epsilon(1e-9));
          if (fit.status == cppunivariate::TrGammaStatus::Ok)
            CHECK(score_norm(x, fit.shape1, fit.shape2, fit.scale) < 1e-3 * n);
        }
        if (fit.status == cppunivariate::TrGammaStatus::LognormalBoundary) CHECK(fit.logLik == doctest::Approx(ln).epsilon(1e-12));
        if (fit.status == cppunivariate::TrGammaStatus::PowerBoundary)
          CHECK(fit.logLik == doctest::Approx(pw).epsilon(1e-13));
      }
}

TEST_CASE("trgamma: invalid inputs and dpqr") {
  const std::vector<double> one = {2}, same = {2, 2, 2}, zero = {0, 1}, ok = {1, 2, 4};
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(one)).status == cppunivariate::TrGammaStatus::InvalidData);
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(same)).status == cppunivariate::TrGammaStatus::InvalidData);
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(zero)).status == cppunivariate::TrGammaStatus::InvalidData);
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(ok), 1e-10, 100, -1).status == cppunivariate::TrGammaStatus::InvalidFixed);
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(ok), 1e-10, 100, std::nan(""),
                                   INFINITY).status == cppunivariate::TrGammaStatus::InvalidFixed);
  namespace d = cppunivariate::distributions;
  const double q = d::qtrgamma(0.3, 2, 1.5, 2, true, false);
  CHECK(d::ptrgamma(q, 2, 1.5, 2, true, false) == doctest::Approx(0.3).epsilon(1e-12));
  CHECK(d::dtrgamma(1.3, 1, 2.5, 2, false) ==
        doctest::Approx(2.5 / 2 * std::pow(0.65, 1.5) * std::exp(-std::pow(0.65, 2.5))));
  // log-scale quantile: finite where scale * qgamma^(1/shape2) would overflow.
  CHECK(std::isfinite(d::qtrgamma(0.5, 1e4, 0.0125, std::exp(-700), true, false)));
  cppunivariate::distributions::StdRng<> rng(5);
  for (int i = 0; i < 100; ++i) CHECK(d::rtrgamma(rng, 2, 1.5, 2) > 0);
}

TEST_CASE("trgamma: public face helper and both shapes fixed on constant data") {
  const std::vector<double> y = {-1, 0, 2};
  const auto f = cppunivariate::trgamma_faces_log(mltest::as_span(y));
  REQUIRE(f.valid);
  CHECK(f.meanlog == doctest::Approx(1.0 / 3));
  CHECK(f.power_shape == doctest::Approx(0.6));
  CHECK(f.log_endpoint == 2);
  const auto r = cppunivariate::trgamma_fit_log(mltest::as_span(y), 1e-10, 100);
  CHECK(r.logLik >= std::max(f.lognormal_logLik, f.power_logLik) - 1e-10);
  const std::vector<double> x = {2, 2};
  const auto fixed = cppunivariate::trgamma_mle(mltest::as_span(x), 1e-10, 100, 3, 2);
  REQUIRE(fixed.status == cppunivariate::TrGammaStatus::Ok);
  CHECK(fixed.scale == doctest::Approx(2 / std::sqrt(3.0)));
  CHECK(fixed.logLik == doctest::Approx(trgamma_loglik(x, 3, 2, fixed.scale)).epsilon(1e-12));
}

TEST_CASE("trgamma: stable density at a large gamma shape and NaN propagation") {
  const double a = 1e10;
  const double ld = cppunivariate::distributions::dtrgamma(1, a, 1, 1/a, true);
  CHECK(ld == doctest::Approx(0.5 * std::log(a) - 0.91893853320467274178 - 1/(12*a)).epsilon(1e-12));
  CHECK(std::isnan(cppunivariate::distributions::dtrgamma(std::nan(""), 2, 1, 1, false)));
}

TEST_CASE("trgamma: evaluation close to the power face without latent gamma underflow") {
  namespace d = cppunivariate::distributions;
  const double q = d::qtrgamma(.9, 1e-6, 1e6, 1, true, false);
  CHECK(q > .89);
  CHECK(q < .91);
  CHECK(d::ptrgamma(q, 1e-6, 1e6, 1, true, false) == doctest::Approx(.9).epsilon(1e-12));
  cppunivariate::distributions::StdRng<> rng(7);
  for (int i=0; i<20; ++i) CHECK(d::rtrgamma(rng, 1e-6, 1e6, 1) > 0);
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("trgamma: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("trgamma",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtrgamma(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptrgamma(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtrgamma(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtrgamma(rng, p[0], p[1], p[2]); });
}

TEST_CASE("P2 trgamma golden holds") {
 auto g=mltest::load_distribution_golden("trgamma");
 const double nan=std::numeric_limits<double>::quiet_NaN();
for(int mask=0;mask<4;++mask){double a=mask&1?g.estimates.at("shape1"):nan,b=mask&2?g.estimates.at("shape2"):nan;auto r=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(g.data),g.estimates.at("scale"),a,b);REQUIRE(r.status==cppunivariate::TrGammaStatus::Ok);
 CHECK(r.scale==g.estimates.at("scale"));
 if(mask&1)CHECK(r.shape1==a);
 if(mask&2)CHECK(r.shape2==b);
 CHECK(r.logLik>=g.logLik-1e-5);}
}

TEST_CASE("P2 trgamma held-scale power root near its endpoint") {
 std::vector<double> x(5,1+1e-12);
 auto r=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),1,2);
 REQUIRE(r.status==cppunivariate::TrGammaStatus::Ok);
 CHECK(r.shape1==2);
 CHECK(r.scale==1);
 CHECK(r.shape2>1e8);
 const double u=r.shape2*std::log(x[0]);
 CHECK(std::abs(1+2*u-u*std::exp(u))<1e-8);
}


// mpmath generator: import mpmath as mp; mp.mp.dps=100
// P=lambda z:mp.gammainc(3,0,z)/mp.gamma(3)
// Q=lambda z:mp.gammainc(3,z,mp.inf)/mp.gamma(3)
// lo=mp.exp(mp.findroot(lambda y:mp.log(P(mp.exp(y)))+800,-mp.mpf(800)/3))
// hi=mp.findroot(lambda z:mp.log(Q(z))+800,800)
// [2*lo**(mp.mpf(2)/3),2*hi**(mp.mpf(2)/3)]
TEST_CASE("trgamma: logarithmic quantile regression") {
  CHECK(cppunivariate::distributions::qtrgamma(-800.0, 3.0, 1.5, 2.0, true, true) == doctest::Approx(1.8452049111202298e-77).epsilon(1e-11));
  CHECK(cppunivariate::distributions::qtrgamma(-800.0, 3.0, 1.5, 2.0, false, true) == doctest::Approx(174.17550845088326).epsilon(1e-11));
}

TEST_CASE("trgamma: support endpoints on log input") {
  CHECK(cppunivariate::distributions::qtrgamma(cppunivariate::special::kNegInf, 3.0, 1.5, 2.0, true, true) == 0.0);
  CHECK(cppunivariate::distributions::qtrgamma(0.0, 3.0, 1.5, 2.0, false, true) == 0.0);
  CHECK(cppunivariate::distributions::qtrgamma(cppunivariate::special::kNegInf, 3.0, 1.5, 2.0, false, true) == cppunivariate::special::kPosInf);
  CHECK(cppunivariate::distributions::qtrgamma(0.0, 3.0, 1.5, 2.0, true, true) == cppunivariate::special::kPosInf);
}

// At x=mp.mpf(float(1e100)), evaluate the same mpmath tails and
// the README log density; constants are also in the independent truth table.
TEST_CASE("trgamma: extreme observation log tails and density") {
  CHECK(cppunivariate::distributions::dtrgamma(1e100, 3.0, 1.5, 2.0, true) == doctest::Approx(-3.535533905932738e+149).epsilon(1e-11));
  CHECK(cppunivariate::distributions::ptrgamma(1e100, 3.0, 1.5, 2.0, true, true) == doctest::Approx(0.0).epsilon(1e-11));
  CHECK(cppunivariate::distributions::ptrgamma(1e100, 3.0, 1.5, 2.0, false, true) == doctest::Approx(-3.535533905932738e+149).epsilon(1e-11));
}

TEST_CASE("trgamma: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("trgamma");
  const auto x=mltest::as_span(g.data);
  CHECK(cppunivariate::trgamma_mle(x,1e-8,1).status==cppunivariate::TrGammaStatus::IterationFailure);
}

TEST_CASE("trgamma: structured face payloads") {
  std::vector<double> x,p;
  for(int i=0;i<400;++i) {
    const double u=(i+.5)/400,z=cppunivariate::special::qnorm(u,0,1,true,false);
    x.push_back(std::exp((z+.05*(z*z-1))));
    p.push_back(3*std::pow(u,1/2.5));
  }
  const auto n=cppunivariate::trgamma_mle(mltest::as_span(x));
  const auto ref=cppunivariate::lognormal_mle(mltest::as_span(x));
  REQUIRE(n.stop_detail.limit_coef.size()==2);
  CHECK(n.stop_detail.limit_coef[0]==doctest::Approx(ref.meanlog));
  CHECK(n.stop_detail.limit_coef[1]==doctest::Approx(ref.sdlog));
  CHECK(n.stop_detail.limit_loglik==doctest::Approx(ref.logLik));
  const auto r=cppunivariate::trgamma_mle(mltest::as_span(p));
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  double sum=0;const double endpoint=*std::max_element(p.begin(),p.end());
  for(double v:p)sum+=std::log(endpoint/v);
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(p.size()/sum));
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(endpoint));
  const auto public_fit=cppunivariate::power_mle(mltest::as_span(p));
  CHECK(r.stop_detail.limit_coef[0]==doctest::Approx(public_fit.alpha));
  CHECK(r.stop_detail.limit_coef[1]==doctest::Approx(public_fit.beta));
  CHECK(r.stop_detail.limit_loglik==doctest::Approx(public_fit.logLik));
}

TEST_CASE("trgamma: small certificate cap retains a feasible witness") {
  const auto g=mltest::load_distribution_golden("trgamma");
  std::vector<double> logs;for(double v:g.data)logs.push_back(std::log(v));
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize({logs.data(),logs.size()},d));
  const auto r=cppunivariate::trgamma_detail::fit_free(d,1e-10,100,1);
  CHECK(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  REQUIRE(r.stop_detail.budget_exhausted);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  CHECK(std::isfinite(r.stop_detail.loglik_lower));
  CHECK(r.stop_detail.loglik_upper==INFINITY);
  const auto& c=r.stop_detail.candidates.front();
  REQUIRE(c.coef.size()==3);
  double ll=0;
  for(double v:g.data)ll+=cppunivariate::distributions::dtrgamma(v,c.coef[0],c.coef[1],c.coef[2],true);
  CHECK(ll>=c.loglik_lower-1e-8);
  CHECK(ll<=c.loglik_upper+1e-8);
  CHECK_FALSE(cppunivariate::trgamma_mle({g.data.data(),g.data.size()},1e-10,100).stop_detail.budget_exhausted);
}

TEST_CASE("trgamma: uniform refitted seam includes zero skewness") {
  for(double skew:{-0.001,0.0,0.001}) {
    std::vector<double> y;
    for(int i=0;i<101;++i) {
      const double z=cppunivariate::special::qnorm((i+.5)/101,0,1,true,false);
      y.push_back(z+skew*(z*z-1));
    }
    cppunivariate::trgamma_detail::LogData d;
    REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(y),d));
    const double T=.01/std::max(d.umax,-d.umin);
    const double upper=cppunivariate::trgamma_detail::normal_seam_upper(d,T);
    CHECK(std::isfinite(upper));
    for(int j=1;j<=100;++j) {
      int evaluations=0;
      const auto p=cppunivariate::trgamma_detail::profile(d,T*j/100,evaluations);
      CHECK(p.delta<=upper+2e-13);
    }
    if(skew==0)CHECK(upper<1e-30);
  }
}

TEST_CASE("trgamma: held endpoint retains each exp minus one factor") {
  const std::vector<double> x={.1,.3,1.0};
  const auto r=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),1);
  REQUIRE(r.status==cppunivariate::TrGammaStatus::Ok);
  CHECK(r.scale==1);
  CHECK(r.logLik==doctest::Approx(trgamma_loglik(x,r.shape1,r.shape2,1)).epsilon(1e-11));
  const double sum=std::log(.1)+std::log(.3);
  const double face=3*(std::log(-3/sum)-1)-sum-1;
  const auto tail=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),1,NAN,1e6);
  REQUIRE(tail.status==cppunivariate::TrGammaStatus::Ok);
  CHECK(tail.logLik==doctest::Approx(face).epsilon(1e-5));
  CHECK(r.logLik>face+.01);
  const std::vector<double> common={2,2,2};
  CHECK(cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(common),1).logLik==INFINITY);
  const auto held=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(common),1,2);
  CHECK(held.status==cppunivariate::TrGammaStatus::Ok);
}

TEST_CASE("trgamma: held-scale exhausted certificate keeps original witnesses") {
  const auto g=mltest::load_distribution_golden("trgamma");
  std::vector<double> logs;for(double v:g.data)logs.push_back(std::log(v));
  const auto r=cppunivariate::trgamma_fixed_scale_log(mltest::as_span(logs),std::log(2.0),NAN,NAN,1e-10,100,1);
  CHECK(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  REQUIRE(r.stop_detail.budget_exhausted);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  const auto& c=r.stop_detail.candidates.front();
  const double ll=trgamma_loglik(g.data,c.coef[0],c.coef[1],c.coef[2]);
  CHECK(ll>=c.loglik_lower-1e-8);
  CHECK(ll<=c.loglik_upper+1e-8);
}

TEST_CASE("trgamma: distinct observations collapsed by logarithms stop numerically") {
  const std::vector<double> x={1e300,std::nextafter(1e300,INFINITY)};
  REQUIRE(x[0]!=x[1]);
  REQUIRE(std::log(x[0])==std::log(x[1]));
  CHECK(cppunivariate::trgamma_mle(mltest::as_span(x)).status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),1e300).status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(cppunivariate::invtrgamma_mle(mltest::as_span(x)).status==cppunivariate::InvTrGammaStatus::IterationFailure);
  CHECK(cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(x),1e300).status==cppunivariate::InvTrGammaStatus::IterationFailure);
}

TEST_CASE("trgamma: exact zero first seam coefficient has a uniform exclusion") {
  // sd=2 and all standardized odd powers cancel exactly in binary arithmetic.
  const std::vector<double> y={-3,-1,0,1,3};
  cppunivariate::trgamma_detail::LogData d;
  REQUIRE(cppunivariate::trgamma_detail::standardize(mltest::as_span(y),d));
  REQUIRE(d.kappa3==0);
  const double T=.001/d.umax;
  CHECK(cppunivariate::trgamma_detail::normal_seam_upper(d,T)==0);
  for(double fraction:{.01,.1,1.0}) {
    int evaluations=0;
    CHECK(cppunivariate::trgamma_detail::profile(d,T*fraction,evaluations).delta<=2e-13);
  }
}

TEST_CASE("trgamma: held coefficients survive logarithmic round trips exactly") {
  const std::vector<double> x={.1,.3,1};
  const auto a=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),.1,2.2,.7);
  const auto b=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(x),.1,2.2,.7);
  REQUIRE(a.status==cppunivariate::TrGammaStatus::Ok);
  REQUIRE(b.status==cppunivariate::InvTrGammaStatus::Ok);
  CHECK(a.shape1==2.2);CHECK(a.shape2==.7);CHECK(a.scale==.1);
  CHECK(b.shape1==2.2);CHECK(b.shape2==.7);CHECK(b.scale==.1);
}

TEST_CASE("trgamma: a held-endpoint tolerance cannot establish nonattainment") {
  const std::vector<double> x={std::exp(-1.0),std::nextafter(1.0,0.0),1};
  const auto r=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(x),1);
  CHECK(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  REQUIRE(r.stop_detail.comparison_tie);
  REQUIRE(r.stop_detail.candidates.size()==2);
  CHECK(r.stop_detail.candidates.back().model==cppunivariate::StopModel::Power);
  CHECK(r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower);
  CHECK(r.stop_detail.loglik_upper-r.stop_detail.loglik_lower<1e-7);
  std::vector<double> inverse;for(double v:x)inverse.push_back(1/v);
  const auto t=cppunivariate::invtrgamma_mle_fixed_scale(mltest::as_span(inverse),1);
  CHECK(t.status==cppunivariate::InvTrGammaStatus::IterationFailure);
  REQUIRE(t.stop_detail.comparison_tie);
  CHECK(t.stop_detail.candidates.back().model==cppunivariate::StopModel::Pareto);
}

TEST_CASE("trgamma: represented-scale likelihood loss is a numerical stop") {
  std::vector<double> x;
  for(int i=0;i<201;++i) {
    const double z=cppunivariate::special::qnorm((i+.5)/201,0,1,true,false);
    x.push_back(std::exp(1e-14*(z-.1*(z*z-1))));
  }
  const auto r=cppunivariate::trgamma_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(r.reconstruction_failure);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  REQUIRE_FALSE(r.stop_detail.candidates.empty());
  const auto& c=r.stop_detail.candidates.front();
  const double ll=trgamma_loglik(x,c.coef[0],c.coef[1],c.coef[2]);
  CHECK(ll>=c.loglik_lower-1e-8);
  CHECK(ll<=c.loglik_upper+1e-8);
}

TEST_CASE("trgamma: partial logarithmic ties and invented endpoint ties stop") {
  double endpoint=7,below=std::nextafter(endpoint,0.0);
  for(int i=0;i<16 && std::log(endpoint)!=std::log(below);++i) {
    endpoint=below;below=std::nextafter(endpoint,0.0);
  }
  REQUIRE(endpoint!=below);
  REQUIRE(std::log(endpoint)==std::log(below));
  const std::vector<double> x={1,below,endpoint};
  const auto r=cppunivariate::trgamma_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(r.transformation_failure);
  const auto inverse=cppunivariate::invtrgamma_mle(mltest::as_span(x));
  CHECK(inverse.status==cppunivariate::InvTrGammaStatus::IterationFailure);
  CHECK(inverse.transformation_failure);
  // Below a held endpoint the constant sample has a finite maximum, not an
  // unbounded density at the endpoint invented by subtracting rounded logs.
  const std::vector<double> one={below};
  const auto held=cppunivariate::trgamma_mle_fixed_scale(mltest::as_span(one),endpoint);
  CHECK(held.status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(held.transformation_failure);
  CHECK_FALSE(held.logLik==INFINITY);
}

TEST_CASE("trgamma: limit witnesses retain raw support endpoints exactly") {
  std::vector<double> x,inverse;
  for(int i=1;i<=400;++i) {
    const double v=7*std::pow(i/400.0,1/2.5);
    x.push_back(v);inverse.push_back(1/v);
  }
  const auto r=cppunivariate::trgamma_mle(mltest::as_span(x));
  REQUIRE(r.status==cppunivariate::TrGammaStatus::PowerBoundary);
  REQUIRE(r.stop_detail.limit_coef.size()==2);
  CHECK(r.scale==*std::max_element(x.begin(),x.end()));
  CHECK(r.stop_detail.limit_coef[0]==r.scale);
  CHECK(r.stop_detail.candidates[0].coef[0]==r.scale);
  const auto t=cppunivariate::invtrgamma_mle(mltest::as_span(inverse));
  REQUIRE(t.status==cppunivariate::InvTrGammaStatus::ParetoBoundary);
  REQUIRE(t.stop_detail.limit_coef.size()==2);
  CHECK(t.scale==*std::min_element(inverse.begin(),inverse.end()));
  CHECK(t.stop_detail.limit_coef[1]==t.scale);
  const double tiny=std::numeric_limits<double>::denorm_min();
  const std::vector<double> subnormal={tiny,2*tiny,3*tiny};
  const auto u=cppunivariate::invtrgamma_mle(mltest::as_span(subnormal));
  REQUIRE(u.status==cppunivariate::InvTrGammaStatus::ParetoBoundary);
  REQUIRE(u.stop_detail.limit_coef.size()==2);
  CHECK(u.stop_detail.limit_coef[1]==tiny);
  CHECK(u.stop_detail.candidates[0].coef[1]==tiny);
}

TEST_CASE("trgamma: positive near-face improvement is a comparison stop") {
  std::vector<double> x;
  for(int i=0;i<201;++i) {
    const double z=cppunivariate::special::qnorm((i+.5)/201,0,1,true,false);
    x.push_back(std::exp(.001*(z-1e-5*(z*z-1))));
  }
  const auto r=cppunivariate::trgamma_mle(mltest::as_span(x));
  CHECK(r.status==cppunivariate::TrGammaStatus::IterationFailure);
  CHECK(r.stop_detail.comparison_tie);
  CHECK_FALSE(r.stop_detail.budget_exhausted);
  CHECK(r.stop_detail.loglik_upper>=r.stop_detail.loglik_lower);
  CHECK(r.stop_detail.loglik_upper-r.stop_detail.loglik_lower<3e-6);
  CHECK(r.stop_detail.candidates.size()>=2);
}

TEST_CASE("transformed gamma helper inclusion keeps its independent gamma fit") {
 const std::vector<double> x{.03,.1,.4,.7,1,2,4,12};const auto r=cppunivariate::trgamma_mle(mltest::as_span(x),1e-8,1000,NAN,1);
 REQUIRE(r.status==cppunivariate::TrGammaStatus::Ok);CHECK(r.shape2==1);CHECK(std::isfinite(r.logLik));
 double ll=0;for(double v:x)ll+=cppunivariate::distributions::dtrgamma(v,r.shape1,r.shape2,r.scale,true);
 CHECK(r.logLik==doctest::Approx(ll).epsilon(1e-10));
}
