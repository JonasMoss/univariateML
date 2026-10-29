#include "test_support.hpp"
#include "cppunivariate/distributions/tnorm/mle.hpp"
#include "cppunivariate/distributions/tnorm/dpqr.hpp"
#include "cppunivariate/distributions/norm/mle.hpp"
#include "cppunivariate/distributions/rng_std.hpp"
#include <cmath>
#include <limits>
#include <vector>

namespace {
const double kInf = std::numeric_limits<double>::infinity();
const double kNan = std::numeric_limits<double>::quiet_NaN();

// Independent log(Phi(beta) - Phi(alpha)) from std::erfc, for moderate
// arguments (|alpha|, |beta| < 30, interval not narrow).
double ref_log_mass(double alpha, double beta) {
  auto upper = [](double z) { return 0.5 * std::erfc(z / std::sqrt(2.0)); };  // 1 - Phi(z)
  if (alpha >= 0) return std::log(upper(alpha) - upper(beta));
  if (beta <= 0) return std::log(upper(-beta) - upper(-alpha));
  return std::log1p(-upper(-alpha) - upper(beta));
}
double ref_loglik(const std::vector<double> &x, double mean, double sd, double a, double b) {
  const double lz = ref_log_mass((a - mean) / sd, (b - mean) / sd);
  double ll = 0;
  for (double v : x) {
    const double z = (v - mean) / sd;
    ll += -std::log(sd) - 0.5 * std::log(2 * M_PI) - z * z / 2 - lz;
  }
  return ll;
}
std::vector<double> tnorm_quantiles(int n, double mean, double sd, double a, double b) {
  std::vector<double> x;
  for (int i = 0; i < n; ++i)
    x.push_back(cppunivariate::distributions::qtnorm((i + 0.5) / n, mean, sd, a, b, true, false));
  return x;
}
// Score and observed information of ref_loglik in (mean, log sd).
void check_regular_max(const std::vector<double> &x, const cppunivariate::TnormResult &r) {
  const double h = 1e-5;
  auto ll = [&](double m, double ls) { return ref_loglik(x, m, std::exp(ls), r.a, r.b); };
  const double ls = std::log(r.sd);
  const double sm = r.sd;  // location step on the sd scale
  const double g1 = (ll(r.mean + h * sm, ls) - ll(r.mean - h * sm, ls)) / (2 * h);
  const double g2 = (ll(r.mean, ls + h) - ll(r.mean, ls - h)) / (2 * h);
  CHECK(std::sqrt(g1 * g1 + g2 * g2) < 1e-3);
  const double f0 = ll(r.mean, ls), k = 1e-3;
  const double h11 = (ll(r.mean + k * sm, ls) - 2 * f0 + ll(r.mean - k * sm, ls)) / (k * k);
  const double h22 = (ll(r.mean, ls + k) - 2 * f0 + ll(r.mean, ls - k)) / (k * k);
  const double h12 = (ll(r.mean + k * sm, ls + k) - ll(r.mean + k * sm, ls - k) -
                      ll(r.mean - k * sm, ls + k) + ll(r.mean - k * sm, ls - k)) / (4 * k * k);
  CHECK(h11 < 0);
  CHECK(h11 * h22 - h12 * h12 > 0);
  CHECK(r.logLik == doctest::Approx(f0).epsilon(1e-11));
}
}  // namespace

TEST_CASE("tnorm: golden for the default normal member") {
  const auto g = mltest::load_distribution_golden("tnorm");
  const auto r = cppunivariate::tnorm_mle(mltest::as_span(g.data), -kInf, kInf);
  REQUIRE(r.status == cppunivariate::TnormStatus::Ok);
  CHECK(r.mean == doctest::Approx(g.estimates.at("mean")).epsilon(mltest::kTol));
  CHECK(r.sd == doctest::Approx(g.estimates.at("sd")).epsilon(mltest::kTol));
  CHECK(r.a == g.estimates.at("a"));
  CHECK(r.b == g.estimates.at("b"));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
  check_regular_max(g.data, r);
}

TEST_CASE("tnorm: interior fits, one- and two-sided, are regular maxima") {
  struct Case { double mean, sd, a, b; };
  for (const Case c : {Case{0.3, 0.25, 0, 1}, Case{0, 1, -1, 2}, Case{2, 1, 0, kInf},
                       Case{0, 1, 1, kInf}, Case{0, 1, -kInf, -0.5}, Case{5, 3, -kInf, 4},
                       Case{-3, 2, -4, 10}}) {
    CAPTURE(c.mean); CAPTURE(c.sd); CAPTURE(c.a); CAPTURE(c.b);
    const auto x = tnorm_quantiles(300, c.mean, c.sd, c.a, c.b);
    const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), c.a, c.b);
    REQUIRE(r.status == cppunivariate::TnormStatus::Ok);
    CHECK(r.a == c.a);
    CHECK(r.b == c.b);
    // Quantile samples recover the generating law closely.
    CHECK(r.mean == doctest::Approx(c.mean).epsilon(0.1).scale(c.sd));
    CHECK(r.sd == doctest::Approx(c.sd).epsilon(0.1));
    check_regular_max(x, r);
    // The interior maximum beats the face.
    CHECK(r.logLik > r.limit_logLik);
    // Affine equivariance: x -> 3 - 2x maps (a, b) to (3 - 2b, 3 - 2a).
    std::vector<double> y;
    for (double v : x) y.push_back(3 - 2 * v);
    const auto s = cppunivariate::tnorm_mle(mltest::as_span(y), 3 - 2 * c.b, 3 - 2 * c.a);
    REQUIRE(s.status == cppunivariate::TnormStatus::Ok);
    CHECK(s.mean == doctest::Approx(3 - 2 * r.mean).epsilon(1e-8));
    CHECK(s.sd == doctest::Approx(2 * r.sd).epsilon(1e-8));
    CHECK(s.logLik == doctest::Approx(r.logLik - x.size() * std::log(2.0)).epsilon(1e-10));
  }
}

TEST_CASE("tnorm: no truncation is the normal fit") {
  const auto x = tnorm_quantiles(50, 1, 2, -kInf, kInf);
  const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), -kInf, kInf);
  const auto n = cppunivariate::norm_mle(mltest::as_span(x));
  REQUIRE(r.status == cppunivariate::TnormStatus::Ok);
  CHECK(r.mean == doctest::Approx(n.mean).epsilon(1e-14));
  CHECK(r.sd == doctest::Approx(n.sd).epsilon(1e-14));
  CHECK(r.logLik == doctest::Approx(n.logLik).epsilon(1e-13));
  // Bounds far outside the data change nothing to rounding.
  const auto far = cppunivariate::tnorm_mle(mltest::as_span(x), -1e3, 1e3);
  REQUIRE(far.status == cppunivariate::TnormStatus::Ok);
  CHECK(far.mean == doctest::Approx(n.mean).epsilon(1e-10));
  CHECK(far.sd == doctest::Approx(n.sd).epsilon(1e-10));
  CHECK(far.logLik == doctest::Approx(n.logLik).epsilon(1e-12));
}

TEST_CASE("tnorm: escapes to the exponential faces") {
  using S = cppunivariate::TnormStatus;
  // Exact exponential spacing: the population variance of x - a exceeds
  // the squared mean for these quantiles stretched by a power above one.
  std::vector<double> e;
  for (int i = 0; i < 200; ++i) e.push_back(std::pow(-std::log1p(-(i + 0.5) / 200), 1.1));
  double m = 0, v = 0;
  for (double z : e) m += z / e.size();
  for (double z : e) v += (z - m) * (z - m) / e.size();
  REQUIRE(v > m * m);
  std::vector<double> x, y;
  for (double z : e) { x.push_back(2 + z); y.push_back(-1 - z); }
  const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), 2, kInf);
  CHECK(r.status == S::ExponentialBoundary);
  CHECK(r.limit_rate == doctest::Approx(-1 / m).epsilon(1e-12));
  CHECK(r.limit_logLik == doctest::Approx(200 * (std::log(1 / m) - 1)).epsilon(1e-12));
  const auto s = cppunivariate::tnorm_mle(mltest::as_span(y), -kInf, -1);
  CHECK(s.status == S::ReflectedExponentialBoundary);
  CHECK(s.limit_rate == doctest::Approx(1 / m).epsilon(1e-12));
  // The exact criterion v >= (mean - a)^2: scale the spread about the mean
  // to either side of equality. Interior fits approach the face from below
  // (sd -> inf, mean -> -inf); within 1e-8 n the face wins the tie.
  for (double ratio : {0.97, 0.99, 0.9999, 1.0 + 1e-9, 1.001}) {
    std::vector<double> t;
    const double target = std::sqrt(ratio) * m;
    for (double z : e) t.push_back(5 + m + (z - m) * target / std::sqrt(v));
    const auto f = cppunivariate::tnorm_mle(mltest::as_span(t), 5, kInf);
    CAPTURE(ratio);
    if (ratio < 0.999) {
      REQUIRE(f.status == S::Ok);
      CHECK(f.mean < -50);
      CHECK(f.logLik - f.limit_logLik > 1e-8 * 200);
      CHECK(f.logLik - f.limit_logLik < 0.03);
      check_regular_max(t, f);
    } else {
      CHECK(f.status == S::ExponentialBoundary);
    }
  }
}

TEST_CASE("tnorm: escapes to the truncated exponential and uniform faces") {
  using S = cppunivariate::TnormStatus;
  // Mass near both ends: more dispersed than the uniform on (0, 1).
  const std::vector<double> ends{0.02, 0.05, 0.1, 0.12, 0.85, 0.9, 0.95, 0.97};
  const auto r = cppunivariate::tnorm_mle(mltest::as_span(ends), 0, 1);
  CHECK(r.status == S::TruncatedExponentialBoundary);
  // Independent face: solve 1/(1 - exp(-s)) - 1/s = mean(x) for the rate s.
  double m = 0;
  for (double z : ends) m += z / ends.size();
  auto face_mean = [](double s) { return std::abs(s) < 1e-8 ? 0.5 + s / 12 : 1 / (-std::expm1(-s)) - 1 / s; };
  double sl = -50, sh = 50;
  for (int i = 0; i < 200; ++i) { const double mid = (sl + sh) / 2; (face_mean(mid) < m ? sl : sh) = mid; }
  const double rate = (sl + sh) / 2;
  CHECK(r.limit_rate == doctest::Approx(rate).epsilon(1e-10));
  double face_ll = -static_cast<double>(ends.size()) * std::log(std::expm1(rate) / rate);
  for (double z : ends) face_ll += rate * z;
  CHECK(r.limit_logLik == doctest::Approx(face_ll).epsilon(1e-12));
  // Truncated exponential quantiles on (-1, 3) with rate 0.7, spread
  // shrunk about their mean: interior maxima approaching the face.
  const double lo = -1, hi = 3;
  std::vector<double> t;
  for (int i = 0; i < 400; ++i) {
    const double p = (i + 0.5) / 400;
    t.push_back(lo + std::log1p(p * std::expm1(0.7 * (hi - lo))) / 0.7);
  }
  double tm = 0;
  for (double z : t) tm += z / t.size();
  for (double shrink : {0.97, 0.99, 1.0, 1.0001}) {
    std::vector<double> w;
    for (double z : t) w.push_back(tm + shrink * (z - tm));
    const auto f = cppunivariate::tnorm_mle(mltest::as_span(w), lo, hi);
    CAPTURE(shrink);
    CHECK(f.limit_rate == doctest::Approx(0.7).epsilon(1e-4));
    if (shrink < 1) {
      REQUIRE(f.status == S::Ok);
      CHECK(f.logLik > f.limit_logLik + 1e-8 * 400);
      CHECK(f.mean > hi);
      check_regular_max(w, f);
    } else {
      // The quantile grid itself is a near tie, decided for the face.
      CHECK(f.status == S::TruncatedExponentialBoundary);
    }
  }
  // Held mean: the uniform face, mean((x - mean)^2) >= ((b-mean)^3 - (a-mean)^3)/(3(b-a)).
  for (double mean : {0.5, 0.3}) {
    const auto held = cppunivariate::tnorm_mle(mltest::as_span(ends), 0, 1, 1e-12, 100, mean, kNan);
    CHECK(held.status == S::UniformBoundary);
    CHECK(held.limit_logLik == 0);
  }
  std::vector<double> c;
  for (double z : ends) c.push_back(0.5 + 0.5 * (z - 0.5));
  // mean((c - 0.5)^2) = 0.0425 < 1/12.
  const auto ok = cppunivariate::tnorm_mle(mltest::as_span(c), 0, 1, 1e-12, 100, 0.5, kNan);
  CHECK(ok.status == S::Ok);
  CHECK(ok.sd > 0.2);
  const double h = 1e-6;
  CHECK(std::abs(ref_loglik(c, 0.5, ok.sd * (1 + h), 0, 1) - ref_loglik(c, 0.5, ok.sd * (1 - h), 0, 1)) / (2 * h) < 1e-5);
}

TEST_CASE("tnorm: held mean or sd") {
  using S = cppunivariate::TnormStatus;
  const auto x = tnorm_quantiles(200, 0.5, 1.5, 0, kInf);
  const double h = 1e-6;
  for (double sd : {0.3, 1.5, 20.0}) {
    const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), 0, kInf, 1e-12, 100, kNan, sd);
    REQUIRE(r.status == S::Ok);
    CHECK(r.sd == sd);
    const double g = (ref_loglik(x, r.mean + h, sd, 0, kInf) - ref_loglik(x, r.mean - h, sd, 0, kInf)) / (2 * h);
    CHECK(std::abs(g) < 1e-4);
    CHECK(r.logLik == doctest::Approx(ref_loglik(x, r.mean, sd, 0, kInf)).epsilon(1e-11));
  }
  for (double mean : {-2.0, 0.5, 3.0}) {
    const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), 0, kInf, 1e-12, 100, mean, kNan);
    REQUIRE(r.status == S::Ok);
    CHECK(r.mean == mean);
    const double g = (ref_loglik(x, mean, r.sd * (1 + h), 0, kInf) - ref_loglik(x, mean, r.sd * (1 - h), 0, kInf)) / (2 * h);
    CHECK(std::abs(g) < 1e-4);
    CHECK(r.logLik == doctest::Approx(ref_loglik(x, mean, r.sd, 0, kInf)).epsilon(1e-11));
  }
  // Held sd: one observation, or constant data, still give an MLE.
  const std::vector<double> one{0.2}, same{0.2, 0.2, 0.2};
  const auto r1 = cppunivariate::tnorm_mle(mltest::as_span(one), 0, 1, 1e-12, 100, kNan, 0.5);
  CHECK(r1.status == S::Ok);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(same), 0, 1, 1e-12, 100, kNan, 0.5).mean ==
        doctest::Approx(r1.mean).epsilon(1e-12));
  // Both held: the log-likelihood at the given values.
  const auto both = cppunivariate::tnorm_mle(mltest::as_span(x), 0, kInf, 1e-12, 100, 0.5, 1.5);
  CHECK(both.status == S::Ok);
  CHECK(both.logLik == doctest::Approx(ref_loglik(x, 0.5, 1.5, 0, kInf)).epsilon(1e-12));
  // Free sd needs spread (about the held mean).
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(same), 0, 1).status == S::DegenerateData);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(one), 0, 1).status == S::DegenerateData);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(same), 0, 1, 1e-12, 100, 0.2, kNan).status ==
        S::DegenerateData);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(same), 0, 1, 1e-12, 100, 0.3, kNan).status == S::Ok);
}

TEST_CASE("tnorm: input validation") {
  using S = cppunivariate::TnormStatus;
  const std::vector<double> x{0.1, 0.5, 0.7};
  auto status = [&](const std::vector<double> &y, double a, double b, double m = kNan, double s = kNan) {
    return cppunivariate::tnorm_mle(mltest::as_span(y), a, b, 1e-12, 100, m, s).status;
  };
  CHECK(status(x, 0, 1) == S::Ok);
  CHECK(status(x, 0.1, 1) != S::OutsideBounds);  // closed endpoint
  CHECK(status(x, 0, 0.7) != S::OutsideBounds);
  CHECK(status(x, 0.2, 1) == S::OutsideBounds);
  CHECK(status(x, 1, 0) == S::InvalidBounds);
  CHECK(status(x, 0, 0) == S::InvalidBounds);
  CHECK(status(x, kNan, 1) != S::InvalidBounds);  // NaN estimates min(x)
  CHECK(status(x, kInf, kInf) == S::InvalidBounds);
  CHECK(status(x, -kInf, -kInf) == S::InvalidBounds);
  CHECK(status(x, 0, 1, kInf) == S::InvalidFixed);
  CHECK(status(x, 0, 1, kNan, 0) == S::InvalidFixed);
  CHECK(status(x, 0, 1, kNan, -1) == S::InvalidFixed);
  CHECK(status({}, 0, 1) == S::InvalidInput);
  CHECK(status({0.5, kNan}, 0, 1) == S::InvalidInput);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(x), 0, 1, 0, 100).status == S::InvalidInput);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(x), 0, 1, 1e-12, 0).status == S::InvalidInput);
  const auto y = tnorm_quantiles(100, 0, 1, 1, kInf);
  CHECK(cppunivariate::tnorm_mle(mltest::as_span(y), 1, kInf, 1e-12, 1).status == S::IterationFailure);
}

TEST_CASE("tnorm: quadrature masses and moments") {
  // Normal masses against erfc, and deep tails against Mills-ratio series.
  for (double alpha : {-3.0, -1.0, 0.0, 0.5, 2.0})
    for (double width : {0.5, 2.0, 10.0})
      CHECK(cppunivariate::distributions::tnorm_log_mass(alpha, alpha + width) ==
            doctest::Approx(ref_log_mass(alpha, alpha + width)).epsilon(1e-13));
  for (double t : {40.0, 100.0, 1e4}) {
    // log(1 - Phi(t)) = -t^2/2 - log(t sqrt(2 pi)) + log(1 - 1/t^2 + 3/t^4 - 15/t^6)
    const double series = -t * t / 2 - std::log(t) - 0.5 * std::log(2 * M_PI) +
                          std::log1p(-1 / (t * t) + 3 / std::pow(t, 4) - 15 / std::pow(t, 6) + 105 / std::pow(t, 8) - 945 / std::pow(t, 10));
    CHECK(cppunivariate::distributions::tnorm_log_mass(t, kInf) == doctest::Approx(series).epsilon(1e-14));
    CHECK(cppunivariate::distributions::tnorm_log_mass(-kInf, -t) == doctest::Approx(series).epsilon(1e-14));
  }
  // Narrow interval: mass = width * phi(midpoint) to second order.
  const double w = 1e-8;
  CHECK(cppunivariate::distributions::tnorm_log_mass(0.5, 0.5 + w) ==
        doctest::Approx(std::log((0.5 + w) - 0.5) - 0.5 * std::log(2 * M_PI) - (0.5 + w / 2) * (0.5 + w / 2) / 2).epsilon(1e-14));
  // Truncated exponential moments: mean 1/(1 - e^{-t}) - 1/t on (0, 1).
  for (double t : {-30.0, -3.0, 1e-9, 3.0, 300.0}) {
    const auto m = cppunivariate::tnorm_detail::mass(t, 0, 0, 1);
    REQUIRE(m.ok);
    const double mean = std::abs(t) < 1e-5 ? 0.5 + t / 12 : 1 / (-std::expm1(-t)) - 1 / t;
    const double var = 1 / (t * t) - 1 / (4 * std::sinh(t / 2) * std::sinh(t / 2));
    CHECK(m.mean == doctest::Approx(mean).epsilon(1e-12));
    if (std::abs(t) > 1e-3) CHECK(m.var == doctest::Approx(var).epsilon(1e-11));
    else CHECK(m.var == doctest::Approx(1.0 / 12).epsilon(1e-12));
  }
  // Normal moments: mean 0, variance 1, third 0, fourth 3.
  const auto n = cppunivariate::tnorm_detail::mass(0, -0.5, -kInf, kInf);
  CHECK(std::abs(n.mean) < 1e-15);
  CHECK(n.var == doctest::Approx(1).epsilon(1e-14));
  CHECK(std::abs(n.m3) < 1e-14);
  CHECK(n.m4 == doctest::Approx(3).epsilon(1e-14));
  CHECK(n.log_mass == doctest::Approx(0.5 * std::log(2 * M_PI)).epsilon(1e-15));
}

TEST_CASE("tnorm: d/p/q/r") {
  namespace d = cppunivariate::distributions;
  struct Case { double mean, sd, a, b; };
  for (const Case c : {Case{0, 1, -1, 2}, Case{0, 1, 8, 9}, Case{0, 1, 40, 41}, Case{0, 1, 10, kInf},
                       Case{0, 1, -kInf, -40}, Case{1e6, 1e3, 0, 1}, Case{2, 3, -kInf, kInf}}) {
    CAPTURE(c.a); CAPTURE(c.b); CAPTURE(c.mean);
    // Density integrates to one over the effective support.
    const double lo = std::isfinite(c.a) ? c.a : c.b - 12 * c.sd;
    const double hi = std::isfinite(c.b) ? c.b : lo + 12 * c.sd;
    const double lo2 = std::isfinite(c.a) || std::isfinite(c.b) ? lo : c.mean - 12 * c.sd;
    const double hi2 = std::isfinite(c.a) || std::isfinite(c.b) ? hi : c.mean + 12 * c.sd;
    double integral = 0;
    const int cells = 200000;
    for (int i = 0; i < cells; ++i)
      integral += (hi2 - lo2) / cells * d::dtnorm(lo2 + (i + 0.5) * (hi2 - lo2) / cells, c.mean, c.sd, c.a, c.b, false);
    CHECK(integral == doctest::Approx(1).epsilon(1e-6));
    for (bool lower : {true, false}) for (bool logp : {true, false})
      for (double u : {1e-12, 0.01, 0.5, 0.99, 1 - 1e-9}) {
        const double p = logp ? std::log(u) : u;
        const double q = d::qtnorm(p, c.mean, c.sd, c.a, c.b, lower, logp);
        REQUIRE(q >= c.a);
        REQUIRE(q <= c.b);
        // Round trip in probability, allowing for the spacing of doubles at q.
        const double back = d::ptnorm(q, c.mean, c.sd, c.a, c.b, lower, false);
        const double ulp = std::nextafter(q, kInf) - q;
        const double dens = d::dtnorm(q, c.mean, c.sd, c.a, c.b, false);
        CHECK(std::abs(back - u) <= 1e-12 * u + 4 * ulp * dens + 1e-300);
      }
    CHECK(d::ptnorm(c.a, c.mean, c.sd, c.a, c.b, true, false) == 0);
    CHECK(d::ptnorm(c.b, c.mean, c.sd, c.a, c.b, true, false) == 1);
  }
  // Deep upper tail: approximately a + exponential with rate a.
  CHECK(d::dtnorm(40.01, 0, 1, 40, 41, true) == doctest::Approx(3.2894534805491).epsilon(1e-12));
  CHECK(d::dtnorm(10.05, 0, 1, 10, kInf, true) == doctest::Approx(1.81109661730779).epsilon(1e-12));
  // Zero at and outside the bounds, like extraDistr.
  CHECK(d::dtnorm(0, 0, 1, 0, 1, false) == 0);
  CHECK(d::dtnorm(1, 0, 1, 0, 1, false) == 0);
  CHECK(d::dtnorm(-1, 0, 1, 0, 1, false) == 0);
  CHECK(std::isnan(d::dtnorm(0.5, 0, -1, 0, 1, false)));
  CHECK(std::isnan(d::dtnorm(0.5, 0, 1, 1, 0, false)));
  CHECK(std::isnan(d::qtnorm(1.5, 0, 1, 0, 1, true, false)));
  CHECK(d::qtnorm(0, 0, 1, 0, 1, true, false) == 0);
  CHECK(d::qtnorm(1, 0, 1, 0, 1, true, false) == 1);
  cppunivariate::distributions::StdRng<> rng(17);
  double sum = 0;
  for (int i = 0; i < 2000; ++i) {
    const double r = d::rtnorm(rng, 0, 1, 1, kInf);
    REQUIRE(r > 1);
    sum += r;
  }
  // E X = phi(1)/(1 - Phi(1)) = 1.5251.
  CHECK(sum / 2000 == doctest::Approx(1.5251352761609812).epsilon(0.02));
}

// Independent high-precision oracle; see quality/truth.py.
// Current failures: notes/evaluations/2026-10-29-dpqr-truth/summary.csv.
TEST_CASE("tnorm: d/p/q/r against the truth table" * doctest::may_fail()) {
  mltest::check_dpqr_truth("tnorm",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dtnorm(x, p[0], p[1], p[2], p[3], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::ptnorm(x, p[0], p[1], p[2], p[3], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qtnorm(x, p[0], p[1], p[2], p[3], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rtnorm(rng, p[0], p[1], p[2], p[3]); });
}

TEST_CASE("tnorm free bounds are exact sample extremes") {
  const std::vector<double> x{-2, -.2, 0, .1, 2};
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const auto r = cppunivariate::tnorm_mle(mltest::as_span(x), nan, nan, 1e-12, 100, 0, 1);
  CHECK(r.status == cppunivariate::TnormStatus::Ok);
  CHECK(r.a == -2);
  CHECK(r.b == 2);
  const auto fixed = cppunivariate::tnorm_mle(mltest::as_span(x), -2, 2, 1e-12, 100, 0, 1);
  CHECK(r.logLik == fixed.logLik);
}
