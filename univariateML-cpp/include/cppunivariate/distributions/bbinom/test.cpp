// bbinom: golden, score gate, faces, size = 1, fixed shapes, d/p/q/r.

#include "test_support.hpp"

#include "cppunivariate/distributions/bbinom/dpqr.hpp"
#include "cppunivariate/distributions/bbinom/mle.hpp"
#include "cppunivariate/distributions/binom/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <cmath>
#include <limits>
#include <vector>

namespace {

constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

double bb_loglik(const std::vector<int> &x, double size, double a, double b) {
  double ll = 0;
  for (int v : x) ll += cppunivariate::distributions::dbbinom(v, size, a, b, true);
  return ll;
}

// ||finite-difference score|| in the free natural parameters.
double score_norm(const std::vector<int> &x, double size, double a, double b, bool free_a,
                  bool free_b) {
  double norm2 = 0;
  const double h = 1e-6;
  if (free_a) {
    const double g = (bb_loglik(x, size, a * (1 + h), b) - bb_loglik(x, size, a * (1 - h), b)) /
                     (2 * h * a);
    norm2 += g * g;
  }
  if (free_b) {
    const double g = (bb_loglik(x, size, a, b * (1 + h)) - bb_loglik(x, size, a, b * (1 - h))) /
                     (2 * h * b);
    norm2 += g * g;
  }
  return std::sqrt(norm2);
}

std::vector<int> sample(unsigned seed, int n, double size, double a, double b) {
  cppunivariate::distributions::StdRng<> rng(seed);
  std::vector<int> x;
  for (int i = 0; i < n; ++i)
    x.push_back(static_cast<int>(cppunivariate::distributions::rbbinom(rng, size, a, b)));
  return x;
}

cppunivariate::BbinomResult fit(const std::vector<int> &x, double size, double a = kNaN,
                                double b = kNaN) {
  return cppunivariate::bbinom_mle({x.data(), x.size()}, size, 1e-10, 100, a, b);
}

}  // namespace

TEST_CASE("bbinom: golden and score gate") {
  const auto g = mltest::load_distribution_golden("bbinom");
  std::vector<int> x;
  for (double v : g.data) x.push_back(static_cast<int>(v));
  const double size = g.estimates.at("size");
  const auto r = fit(x, size);
  REQUIRE(r.status == cppunivariate::BbinomStatus::Ok);
  CHECK(r.alpha == doctest::Approx(g.estimates.at("alpha")).epsilon(mltest::kTol));
  CHECK(r.beta == doctest::Approx(g.estimates.at("beta")).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
  CHECK(r.logLik == doctest::Approx(bb_loglik(x, size, r.alpha, r.beta)).epsilon(1e-12));
  CHECK(score_norm(x, size, r.alpha, r.beta, true, true) < 1e-3);
}

TEST_CASE("bbinom: interior fits pass the score gate, including mean above size / 2") {
  const double shapes[][3] = {{10, 2, 3}, {20, 5, 1.2}, {5, 0.3, 0.4}, {40, 30, 60}, {3, 1, 1}};
  unsigned seed = 11;
  for (const auto &s : shapes) {
    const auto x = sample(seed++, 2000, s[0], s[1], s[2]);
    const auto r = fit(x, s[0]);
    REQUIRE(r.status == cppunivariate::BbinomStatus::Ok);
    CHECK(score_norm(x, s[0], r.alpha, r.beta, true, true) < 1e-3);
    CHECK(r.logLik == doctest::Approx(bb_loglik(x, s[0], r.alpha, r.beta)).epsilon(1e-11));
    // Mirroring the data swaps the shapes.
    std::vector<int> y;
    for (int v : x) y.push_back(static_cast<int>(s[0]) - v);
    const auto m = fit(y, s[0]);
    REQUIRE(m.status == cppunivariate::BbinomStatus::Ok);
    CHECK(m.alpha == doctest::Approx(r.beta).epsilon(1e-7));
    CHECK(m.beta == doctest::Approx(r.alpha).epsilon(1e-7));
  }
}

TEST_CASE("bbinom: binomial face, compared by maximised likelihood") {
  // Binomial data: not overdispersed; the binomial face wins (or ties).
  cppunivariate::distributions::StdRng<> rng(5);
  std::vector<int> x;
  for (int i = 0; i < 3000; ++i)
    x.push_back(static_cast<int>(cppunivariate::distributions::rbinom(rng, 12, 0.35)));
  double mean = 0;
  for (int v : x) mean += v;
  mean /= x.size();
  const auto r = fit(x, 12);
  double binom_ll = 0;
  for (int v : x) binom_ll += cppunivariate::distributions::dbinom(v, 12, mean / 12, true);
  // A binomial sample is overdispersed half the time; when it is, the
  // interior beta-binomial fit is returned and beats the binomial face.
  if (r.status == cppunivariate::BbinomStatus::Ok) {
    CHECK(r.logLik > binom_ll + 1e-8 * x.size());
  } else {
    CHECK(r.status == cppunivariate::BbinomStatus::BinomialLimit);
    CHECK(r.logLik == doctest::Approx(binom_ll).epsilon(1e-12));
  }
  // Underdispersed: var < N p (1 - p).
  const std::vector<int> under{4, 5, 5, 6, 5, 4, 6, 5, 5, 5};
  const auto u = fit(under, 10);
  CHECK(u.status == cppunivariate::BbinomStatus::BinomialLimit);
}

TEST_CASE("bbinom: endpoint-only, point-mass, size 0 and size 1 samples") {
  const std::vector<int> ends{0, 0, 7, 7, 7, 0};
  const auto e = fit(ends, 7);
  CHECK(e.status == cppunivariate::BbinomStatus::TwoPointLimit);
  CHECK(e.logLik == doctest::Approx(6 * std::log(0.5)));
  // An interior observation makes the two-point face unreachable.
  const std::vector<int> ends1{0, 0, 7, 7, 7, 0, 3};
  CHECK(fit(ends1, 7).status == cppunivariate::BbinomStatus::Ok);
  CHECK(fit({0, 0, 0}, 4).status == cppunivariate::BbinomStatus::PointMass);
  CHECK(fit({4, 4}, 4).status == cppunivariate::BbinomStatus::PointMass);
  CHECK(fit({0, 0}, 0).status == cppunivariate::BbinomStatus::Unidentified);
  // size = 1: Bernoulli, alpha + beta unidentified unless a shape is fixed.
  const std::vector<int> bern{0, 1, 1, 0, 1};
  CHECK(fit(bern, 1).status == cppunivariate::BbinomStatus::Unidentified);
  const auto held = fit(bern, 1, 2.0);
  REQUIRE(held.status == cppunivariate::BbinomStatus::Ok);
  CHECK(held.beta == doctest::Approx(2.0 * 2 / 3).epsilon(1e-9));
  // Holding a shape removes the endpoint escape.
  const auto ends_held = fit(ends, 7, 0.5);
  REQUIRE(ends_held.status == cppunivariate::BbinomStatus::Ok);
  CHECK(score_norm(ends, 7, 0.5, ends_held.beta, false, true) < 1e-6);
  CHECK(fit({0, 0, 0}, 4, 1.0).status == cppunivariate::BbinomStatus::PointMass);
}

TEST_CASE("bbinom: input validation") {
  const std::vector<int> x{1, 2, 3};
  CHECK(fit(x, 2).status == cppunivariate::BbinomStatus::SizeTooSmall);
  CHECK(fit(x, 3.5).status == cppunivariate::BbinomStatus::InvalidInput);
  CHECK(fit(x, -1).status == cppunivariate::BbinomStatus::InvalidInput);
  CHECK(fit(x, kNaN).status == cppunivariate::BbinomStatus::InvalidInput);
  CHECK(fit(x, 5, 1.0, 1.0).status == cppunivariate::BbinomStatus::Ok);
  CHECK(fit(x, 5, -1.0).status == cppunivariate::BbinomStatus::InvalidInput);
  CHECK(fit({-1, 2}, 5).status == cppunivariate::BbinomStatus::InvalidInput);
  CHECK(fit({}, 5).status == cppunivariate::BbinomStatus::InvalidInput);
}

TEST_CASE("bbinom: fixed alpha or beta") {
  const auto x = sample(3, 1500, 15, 2.5, 4);
  const auto a = fit(x, 15, 2.5);
  REQUIRE(a.status == cppunivariate::BbinomStatus::Ok);
  CHECK(a.alpha == 2.5);
  CHECK(score_norm(x, 15, 2.5, a.beta, false, true) < 1e-3);
  const auto b = fit(x, 15, kNaN, 4.0);
  REQUIRE(b.status == cppunivariate::BbinomStatus::Ok);
  CHECK(b.beta == 4.0);
  CHECK(score_norm(x, 15, b.alpha, 4.0, true, false) < 1e-3);
  const auto free = fit(x, 15);
  CHECK(free.logLik >= a.logLik);
  CHECK(free.logLik >= b.logLik);
  // A fixed shape far from the truth still has an interior maximiser.
  const auto far = fit(x, 15, 1e-3);
  REQUIRE(far.status == cppunivariate::BbinomStatus::Ok);
  CHECK(score_norm(x, 15, 1e-3, far.beta, false, true) < 1e-3);
}

TEST_CASE("bbinom: d/p/q/r") {
  namespace d = cppunivariate::distributions;
  double total = 0;
  for (int k = 0; k <= 10; ++k) total += d::dbbinom(k, 10, 2, 3, false);
  CHECK(total == doctest::Approx(1).epsilon(1e-13));
  // Independent value (80-digit log-gamma evaluation, discrete-mixtures note).
  CHECK(d::dbbinom(3, 10, 2, 5, true) == doctest::Approx(-1.80280930541464).epsilon(1e-13));
  CHECK(d::dbbinom(5, 10, 1e12, 1e12, true) ==
        doctest::Approx(-1.40204271809053).epsilon(1e-12));
  CHECK(d::dbbinom(5, 10, 1e15, 1e15, true) ==
        doctest::Approx(d::dbinom(5, 10, 0.5, true)).epsilon(1e-12));
  CHECK(d::dbbinom(0, 10, 1e-320, 1e-320, false) == doctest::Approx(0.5).epsilon(1e-11));
  CHECK(d::dbbinom(5, 10, 1e308, 1e308, true) ==
        doctest::Approx(d::dbinom(5, 10, 0.5, true)).epsilon(1e-12));
  CHECK(d::dbbinom(11, 10, 2, 3, false) == 0);
  CHECK(d::dbbinom(2.5, 10, 2, 3, false) == 0);
  CHECK(std::isnan(d::dbbinom(2, 10, -1, 3, false)));
  for (int k = -1; k <= 10; ++k) {
    double cdf = 0;
    for (int j = 0; j <= k; ++j) cdf += d::dbbinom(j, 10, 2, 3, false);
    CHECK(d::pbbinom(k, 10, 2, 3, true, false) == doctest::Approx(cdf).epsilon(1e-13));
    CHECK(d::pbbinom(k, 10, 2, 3, false, false) == doctest::Approx(1 - cdf).epsilon(1e-12));
  }
  for (double p : {0.0, 1e-9, 0.1, 0.4545454545, 0.5, 0.9, 1.0}) {
    for (bool lower : {true, false}) {
      const double q = d::qbbinom(p, 10, 2, 3, lower, false);
      if (lower) {
        CHECK(d::pbbinom(q, 10, 2, 3, true, false) >= p);
        if (q > 0) CHECK(d::pbbinom(q - 1, 10, 2, 3, true, false) < p);
      } else {
        CHECK(d::pbbinom(q, 10, 2, 3, false, false) <= p);
        if (q > 0) CHECK(d::pbbinom(q - 1, 10, 2, 3, false, false) > p);
      }
    }
  }
  CHECK(d::qbbinom(std::log(0.5), 10, 2, 3, true, true) == d::qbbinom(0.5, 10, 2, 3, true, false));
  CHECK(std::isnan(d::qbbinom(1.5, 10, 2, 3, true, false)));
  const auto x = sample(9, 20000, 10, 2, 3);
  double mean = 0;
  for (int v : x) mean += v;
  CHECK(mean / x.size() == doctest::Approx(4.0).epsilon(0.02));
}

TEST_CASE("bbinom: certificate detects a missed peak and bounds a concave numerator") {
  // A grid-only search would miss this arbitrarily narrow peak.
  const std::vector<double> grid{0, 1};
  CHECK_FALSE(cppunivariate::bbinom_detail::certify(grid, 0.0, 1e-8,
    [](double a, double, double b) { return a < 0.123456 && b > 0.123456 ? 1.0 : 0.0; }));
  CHECK(cppunivariate::bbinom_detail::certify(grid, 1.0, 1e-8,
    [](double, double, double) { return 1.0; }));
  cppunivariate::bbinom_detail::Data d;
  d.size = 3; d.n = 6; d.sum = 9;
  d.A = {4,3,2}; d.C = {4,3,2};
  for (double a : {0.0,0.01,0.2,2.0}) {
    const double b = a + 0.15, m = (a+b)/2;
    const double upper = cppunivariate::bbinom_detail::profile_bound(d,a,m,b);
    for (int i = 0; i <= 20; ++i) {
      const double s = a + (b-a)*i/20;
      const auto point = cppunivariate::bbinom_detail::profile(d,s,.5);
      CHECK(point.value <= upper);
    }
  }
}

TEST_CASE("bbinom: inward binomial score uses variance with divisor n") {
  const std::vector<int> x{0,1,1,2,4,4};
  cppunivariate::bbinom_detail::Data d;
  d.size = 4; d.n = x.size(); d.A.assign(4,0); d.C.assign(4,0);
  double sum2 = 0;
  for (int v : x) {
    d.sum += v; sum2 += v*v;
    for (int j = 0; j < 4; ++j) { d.A[j] += v>j; d.C[j] += v<4-j; }
  }
  const double mean = d.sum/d.n, p = mean/4;
  const double variance = sum2/d.n-mean*mean;
  const double expected = d.n*(variance-4*p*(1-p))/(2*p*(1-p));
  const double s = 1e-8;
  double g, H;
  cppunivariate::bbinom_detail::profile_derivs(d,s,cppunivariate::bbinom_detail::solve_p(d,s,p),g,H);
  CHECK(g/s == doctest::Approx(expected).epsilon(1e-6));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("bbinom: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("bbinom",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dbbinom(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::pbbinom(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qbbinom(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rbbinom(rng, p[0], p[1], p[2]); });
}

TEST_CASE("bbinom: fully held and both-shape-held integer profiles") {
 for(auto x:{std::vector<int>{0,1,2,4},std::vector<int>{0,0},std::vector<int>{3,3}}) {
  const auto f=fit(x,kNaN,2,3);
  REQUIRE(f.status==cppunivariate::BbinomStatus::Ok);
  CHECK(f.alpha==2);CHECK(f.beta==3);
  CHECK(f.logLik==doctest::Approx(bb_loglik(x,f.size,2,3)).epsilon(1e-10));
  for(int k=*std::max_element(x.begin(),x.end());k<80;++k)CHECK(f.logLik>=bb_loglik(x,k,2,3)-1e-10);
 }
}

TEST_CASE("bbinom: P3 unfinished optimizer is a failure") {
  auto g=mltest::load_distribution_golden("bbinom");
  const std::vector<int> values(g.data.begin(),g.data.end());
  const cppunivariate::ispan ix{values.data(),values.size()};
  CHECK(cppunivariate::bbinom_mle(ix,10,1e-8,1).status==cppunivariate::BbinomStatus::IterationFailure);
}

TEST_CASE("bbinom: unsupported free-size shape profiles never return an estimate") {
  const std::vector<int> x{0, 1, 2, 4, 8};
  for (const auto &shapes : std::vector<std::pair<double, double>>{
           {kNaN, kNaN}}) {
    const auto result = fit(x, kNaN, shapes.first, shapes.second);
    CHECK(result.status == cppunivariate::BbinomStatus::InvalidInput);
  }
}


TEST_CASE("bbinom: held beta free size certificates") {
  const std::vector<int> x{0, 1, 2, 4};
  const auto result = fit(x, kNaN, kNaN, 0.5);
  REQUIRE(result.status == cppunivariate::BbinomStatus::Ok);
  CHECK(result.size == 4);
  for (int n = 4; n <= 30; ++n) {
    const auto candidate = fit(x, n, kNaN, 0.5);
    REQUIRE(candidate.status == cppunivariate::BbinomStatus::Ok);
    CHECK(result.logLik >= candidate.logLik - 1e-7);
  }
  CHECK(fit(std::vector<int>{3,3}, kNaN, kNaN, 2).status == cppunivariate::BbinomStatus::PointMass);
  CHECK(fit(std::vector<int>{0,0}, kNaN, kNaN, 2).status == cppunivariate::BbinomStatus::Unidentified);
  CHECK(fit(x, kNaN, kNaN, 1e9).status == cppunivariate::BbinomStatus::SizeBudget);
}

TEST_CASE("bbinom: seeded held-beta searches agree with a dense size grid") {
  unsigned int seed = 113;
  for (int rep = 0; rep < 6; ++rep) {
    std::vector<int> x{0, 4};
    for (int i = 0; i < 8; ++i) {
      seed = seed * 1664525u + 1013904223u;
      x.push_back(static_cast<int>((seed >> 16) % 5));
    }
    const double b = rep % 2 ? 1.2 : 0.5;
    const auto result = fit(x, kNaN, kNaN, b);
    REQUIRE(result.status == cppunivariate::BbinomStatus::Ok);
    for (int n = 4; n <= 40; ++n) {
      const auto candidate = fit(x, n, kNaN, b);
      REQUIRE(candidate.status == cppunivariate::BbinomStatus::Ok);
      CHECK(result.logLik >= candidate.logLik - 1e-7);
    }
  }
}

TEST_CASE("bbinom: held alpha free-size certified and unresolved outcomes") {
  const std::vector<int> x{0, 0, 1, 1};
  const auto result = fit(x, kNaN, 1, kNaN);
  REQUIRE(result.status == cppunivariate::BbinomStatus::Ok);
  CHECK(result.size == 1);
  for (int n = 1; n <= 30; ++n) {
    const auto candidate = fit(x, n, 1, kNaN);
    REQUIRE(candidate.status == cppunivariate::BbinomStatus::Ok);
    CHECK(result.logLik >= candidate.logLik - 1e-7);
  }
  CHECK(fit(std::vector<int>{0, 0}, kNaN, 1, kNaN).status == cppunivariate::BbinomStatus::Unidentified);
  CHECK(fit(std::vector<int>{3, 3}, kNaN, 1, kNaN).status == cppunivariate::BbinomStatus::PointMass);
  CHECK(fit(std::vector<int>{0, 1001}, kNaN, 1, kNaN).status == cppunivariate::BbinomStatus::SizeBudget);
  CHECK(fit(std::vector<int>{0, 0, 0, 10}, kNaN, 1, kNaN).status == cppunivariate::BbinomStatus::FaceUnresolved);
}

TEST_CASE("bbinom: seeded held-alpha searches agree with finite size profiles") {
  unsigned int seed = 113;
  for (int rep = 0; rep < 3; ++rep) {
    std::vector<int> x{0, 0, 0, 0, 1, 1, 1, 1};
    for (int i = 0; i < 4; ++i) {
      seed = seed * 1664525u + 1013904223u;
      x.push_back(static_cast<int>((seed >> 16) & 1u));
    }
    const auto result = fit(x, kNaN, 1, kNaN);
    REQUIRE(result.status == cppunivariate::BbinomStatus::Ok);
    for (int n = 1; n <= 20; ++n) {
      const auto candidate = fit(x, n, 1, kNaN);
      REQUIRE(candidate.status == cppunivariate::BbinomStatus::Ok);
      CHECK(result.logLik >= candidate.logLik - 1e-7);
    }
  }
}
