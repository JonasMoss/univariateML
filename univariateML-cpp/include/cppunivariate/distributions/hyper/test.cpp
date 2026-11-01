#include "test_support.hpp"
#include "cppunivariate/distributions/hyper/dpqr.hpp"
#include "cppunivariate/distributions/rng_std.hpp"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

namespace {

namespace dist = cppunivariate::distributions;

constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();

cppunivariate::span as_span(const std::vector<double> &v) { return {v.data(), v.size()}; }

long double lchoose_ld(double n, double r) {
  return std::lgamma(static_cast<long double>(n) + 1) - std::lgamma(static_cast<long double>(r) + 1) -
         std::lgamma(static_cast<long double>(n - r) + 1);
}

// Independent log-likelihood for brute-force enumeration (small N only).
long double brute_ll(const std::vector<double> &x, double N, double m, double k) {
  long double s = 0;
  for (double v : x) {
    if (v < std::max(0.0, k - (N - m)) || v > std::min(k, m))
      return -std::numeric_limits<long double>::infinity();
    s += lchoose_ld(m, v) + lchoose_ld(N - m, k - v) - lchoose_ld(N, k);
  }
  return s;
}

// Log-likelihood on the whole numbers lo, ..., hi.
struct Grid {
  double lo;
  std::vector<long double> ll;
  long double best;
};

template <typename F>
Grid enumerate(double lo, double hi, F ll) {
  Grid g{lo, {}, -std::numeric_limits<long double>::infinity()};
  for (double t = lo; t <= hi; ++t) {
    g.ll.push_back(ll(t));
    g.best = std::max(g.best, g.ll.back());
  }
  return g;
}

// Maximisers within a relative tolerance of the best value.
std::vector<double> maximisers(const Grid &g, long double tol) {
  std::vector<double> out;
  for (std::size_t i = 0; i < g.ll.size(); ++i)
    if (g.ll[i] >= g.best - tol * (1 + std::fabs(g.best))) out.push_back(g.lo + static_cast<double>(i));
  return out;
}

std::vector<double> draw(dist::StdRng<> &rng, int h, double N, double m, double k) {
  std::vector<double> x;
  for (int i = 0; i < h; ++i) x.push_back(dist::rhyper(rng, m, N, k));
  return x;
}

}  // namespace

TEST_CASE("hyper: golden") {
  const auto g = mltest::load_distribution_golden("hyper");
  const auto r = cppunivariate::hyper_mle(mltest::as_span(g.data), 10, 50, kNaN);
  CHECK(r.status == cppunivariate::HyperStatus::Ok);
  CHECK(r.N == g.estimates.at("N"));
  CHECK(r.m == g.estimates.at("m"));
  CHECK(r.k == g.estimates.at("k"));
  CHECK(r.logLik == doctest::Approx(g.logLik).epsilon(mltest::kTol));
}

TEST_CASE("hyper: log pmf against independent formulas") {
  for (double N = 0; N <= 30; ++N)
    for (double m = 0; m <= N; ++m)
      for (double k = 0; k <= N; ++k) {
        long double total = 0;
        for (double x = 0; x <= k; ++x) {
          const double l = dist::dhyper(x, m, N, k, true);
          const long double b = brute_ll({x}, N, m, k);
          if (std::isinf(b)) {
            CHECK(std::isinf(l));
          } else {
            CHECK(std::fabs(l - b) <= 1e-13 * (1 + std::fabs(b)));
            total += std::exp(static_cast<long double>(l));
          }
        }
        CHECK(std::fabs(total - 1) < 1e-13);
      }
  // Large arguments: log C(n, r) as a sum of r long-double logarithms.
  auto lchoose_sum = [](double n, double r) {
    long double s = 0;
    for (double j = 0; j < r; ++j) s += std::log((static_cast<long double>(n) - j) / (r - j));
    return s;
  };
  struct Case { double x, N, m, k; };
  for (const Case c : {Case{500, 2e9, 1e9, 1000}, Case{0, 1e9 + 1, 1, 1000},
                       Case{3, 50, 20, 10}, Case{7, 1e15, 3e14, 20}}) {
    const long double want = lchoose_sum(c.m, c.x) + lchoose_sum(c.N - c.m, c.k - c.x) -
                             lchoose_sum(c.N, c.k);
    // This reference subtracts three sums of logarithms. Account for their
    // rounding when long double has only double precision; the resulting
    // bound reflects the reference calculation, not the pmf's accuracy.
    const long double scale = std::fabs(lchoose_sum(c.m, c.x)) +
        std::fabs(lchoose_sum(c.N-c.m, c.k-c.x)) + std::fabs(lchoose_sum(c.N,c.k));
    const long double reference_error = 4 * c.k * std::numeric_limits<long double>::epsilon() * scale;
    CHECK(std::fabs(cppunivariate::hyper_log_pmf(c.x, c.N, c.m, c.k) - want) <=
          1e-13 * (1 + std::fabs(want)) + reference_error);
  }
}

TEST_CASE("hyper: m with known N agrees with enumeration, ties included") {
  dist::StdRng<> rng(20261028);
  int ties = 0;
  for (int rep = 0; rep < 3000; ++rep) {
    const double N = 1 + std::floor(rng.runif(0, 60));
    const double m0 = std::floor(rng.runif(0, N + 1)), k = std::floor(rng.runif(0, N + 1));
    const auto x = draw(rng, 1 + static_cast<int>(rng.runif(0, 8)), N, m0, k);
    const auto fit = cppunivariate::hyper_mle(as_span(x), k, N, kNaN);
    const double lo = *std::max_element(x.begin(), x.end());
    const double hi = N - k + *std::min_element(x.begin(), x.end());
    CAPTURE(N); CAPTURE(m0); CAPTURE(k); CAPTURE(rep);
    if (k == 0) {
      CHECK(fit.status == cppunivariate::HyperStatus::Unidentified);
      CHECK(fit.m == 0);
      CHECK(fit.tie == N);
      continue;
    }
    auto ll = [&](double m) { return brute_ll(x, N, m, k); };
    const Grid grid = enumerate(lo, hi, ll);
    const long double best = grid.best;
    const auto loose = maximisers(grid, 1e-12L);
    const auto tight = maximisers(grid, 1e-15L);
    REQUIRE((fit.status == cppunivariate::HyperStatus::Ok || fit.status == cppunivariate::HyperStatus::Tie));
    CHECK(fit.N == N);
    CHECK(fit.k == k);
    CHECK(std::find(loose.begin(), loose.end(), fit.m) != loose.end());
    CHECK(std::fabs(fit.logLik - best) <= 1e-12 * (1 + std::fabs(best)));
    if (std::numeric_limits<long double>::digits <= std::numeric_limits<double>::digits) {
      // The lgamma-based enumeration cannot reliably distinguish exact ties
      // at 1e-15 in double precision. Check both reported points against its
      // wider numerical envelope; exact tie cases are tested separately.
      if (fit.status == cppunivariate::HyperStatus::Tie) {
        ++ties;
        CHECK(fit.tie == fit.m + 1);
        CHECK(std::find(loose.begin(), loose.end(), fit.tie) != loose.end());
      }
    } else if (tight.size() == 2) {
      ++ties;
      CHECK(fit.status == cppunivariate::HyperStatus::Tie);
      CHECK(fit.m == tight[0]);
      CHECK(fit.tie == tight[1]);
    } else {
      CHECK(tight.size() == 1);
      CHECK(fit.status == cppunivariate::HyperStatus::Ok);
      CHECK(fit.m == tight[0]);
    }
  }
  CHECK(ties > 10);
}

TEST_CASE("hyper: N with known m agrees with enumeration past the certificate") {
  dist::StdRng<> rng(1028);
  int ties = 0, fits = 0;
  for (int rep = 0; rep < 1500; ++rep) {
    const double N0 = 1 + std::floor(rng.runif(0, 40));
    const double m = std::floor(rng.runif(0, N0 + 1)), k = std::floor(rng.runif(0, N0 + 1));
    const auto x = draw(rng, 1 + static_cast<int>(rng.runif(0, 8)), N0, m, k);
    const auto fit = cppunivariate::hyper_mle(as_span(x), k, kNaN, m);
    double sum = 0;
    for (double v : x) sum += v;
    const double lo = m + k - *std::min_element(x.begin(), x.end());
    CAPTURE(N0); CAPTURE(m); CAPTURE(k); CAPTURE(rep);
    if (sum == 0) {
      if (m == 0 || k == 0) {
        CHECK(fit.status == cppunivariate::HyperStatus::Unidentified);
        CHECK(fit.N == lo);
      } else {
        CHECK(fit.status == cppunivariate::HyperStatus::AllZero);
        CHECK(std::isinf(fit.N));
      }
      continue;
    }
    ++fits;
    // Closed-form tail certificate: D(u) < 0 beyond k (m + k) / mean.
    const double h = static_cast<double>(x.size());
    const double cert = std::floor(k * (m + k) * h / sum) + 1;
    const auto counts = cppunivariate::hyper_detail::tabulate(as_span(x));
    if (cert > lo) CHECK(cppunivariate::hyper_detail::increment_N(counts, m, k, cert).value() < 0);
    const double top = 3 * std::max(cert, lo) + 50;
    auto ll = [&](double N) { return brute_ll(x, N, m, k); };
    const Grid grid = enumerate(lo, top, ll);
    const long double best = grid.best;
    const auto loose = maximisers(grid, 1e-12L);
    const auto tight = maximisers(grid, 1e-15L);
    REQUIRE((fit.status == cppunivariate::HyperStatus::Ok || fit.status == cppunivariate::HyperStatus::Tie));
    CHECK(fit.m == m);
    CHECK(std::find(loose.begin(), loose.end(), fit.N) != loose.end());
    CHECK(std::fabs(fit.logLik - best) <= 1e-12 * (1 + std::fabs(best)));
    if (std::numeric_limits<long double>::digits <= std::numeric_limits<double>::digits) {
      // The lgamma-based enumeration cannot reliably distinguish exact ties
      // at 1e-15 in double precision. Check both reported points against its
      // wider numerical envelope; exact tie cases are tested separately.
      if (fit.status == cppunivariate::HyperStatus::Tie) {
        ++ties;
        CHECK(fit.tie == fit.N + 1);
        CHECK(std::find(loose.begin(), loose.end(), fit.tie) != loose.end());
      }
    } else if (tight.size() == 2) {
      ++ties;
      CHECK(fit.status == cppunivariate::HyperStatus::Tie);
      CHECK(fit.N == tight[0]);
      CHECK(fit.tie == tight[1]);
    } else {
      CHECK(tight.size() == 1);
      CHECK(fit.status == cppunivariate::HyperStatus::Ok);
      CHECK(fit.N == tight[0]);
    }
  }
  CHECK(fits > 1000);
  CHECK(ties > 10);
}

TEST_CASE("hyper: Lincoln-Petersen ties, support limits, fixed design") {
  // One observation: L(N) = L(N - 1) at N = m k / x, L(m) = L(m - 1) at
  // m = x (N + 1) / k.
  const std::vector<double> five{5};
  auto n_fit = cppunivariate::hyper_mle(as_span(five), 10, kNaN, 10);
  CHECK(n_fit.status == cppunivariate::HyperStatus::Tie);
  CHECK(n_fit.N == 19);
  CHECK(n_fit.tie == 20);
  auto m_fit = cppunivariate::hyper_mle(as_span(five), 10, 19, kNaN);
  CHECK(m_fit.status == cppunivariate::HyperStatus::Tie);
  CHECK(m_fit.m == 9);
  CHECK(m_fit.tie == 10);
  CHECK(m_fit.logLik == doctest::Approx(dist::dhyper(5, 10, 19, 10, true)).epsilon(1e-14));
  // max(x) = N - k + min(x): a single feasible m.
  const std::vector<double> three{3, 3};
  auto edge = cppunivariate::hyper_mle(as_span(three), 10, 10, kNaN);
  CHECK(edge.status == cppunivariate::HyperStatus::Ok);
  CHECK(edge.m == 3);
  CHECK(edge.logLik == 0);
  // Observations at both support ends.
  const std::vector<double> ends{0, 6, 6, 0, 3};
  auto both = cppunivariate::hyper_mle(as_span(ends), 6, 30, kNaN);
  CHECK(both.status == cppunivariate::HyperStatus::Ok);
  CHECK(both.m >= 6);
  CHECK(both.m <= 24);
  // Every observation equal to k = m: N = m.
  const std::vector<double> full{4, 4, 4};
  auto all_k = cppunivariate::hyper_mle(as_span(full), 4, kNaN, 4);
  CHECK(all_k.status == cppunivariate::HyperStatus::Ok);
  CHECK(all_k.N == 4);
  CHECK(all_k.logLik == 0);
  // Fully specified design: likelihood evaluation only.
  const std::vector<double> y{2, 3, 2};
  auto held = cppunivariate::hyper_mle(as_span(y), 6, 20, 8);
  CHECK(held.status == cppunivariate::HyperStatus::Ok);
  CHECK(held.logLik == doctest::Approx(2 * dist::dhyper(2, 8, 20, 6, true) +
                                       dist::dhyper(3, 8, 20, 6, true)).epsilon(1e-14));
}

TEST_CASE("hyper: escapes, nonidentification and invalid designs") {
  const std::vector<double> zeros{0, 0, 0}, zero{0};
  auto escape = cppunivariate::hyper_mle(as_span(zeros), 3, kNaN, 5);
  CHECK(escape.status == cppunivariate::HyperStatus::AllZero);
  CHECK(std::isinf(escape.N));
  CHECK(escape.logLik == 0);
  auto no_successes = cppunivariate::hyper_mle(as_span(zeros), 4, kNaN, 0);
  CHECK(no_successes.status == cppunivariate::HyperStatus::Unidentified);
  CHECK(no_successes.N == 4);
  CHECK(no_successes.logLik == 0);
  auto no_draws_N = cppunivariate::hyper_mle(as_span(zero), 0, kNaN, 3);
  CHECK(no_draws_N.status == cppunivariate::HyperStatus::Unidentified);
  CHECK(no_draws_N.N == 3);
  auto no_draws_m = cppunivariate::hyper_mle(as_span(zero), 0, 7, kNaN);
  CHECK(no_draws_m.status == cppunivariate::HyperStatus::Unidentified);
  CHECK(no_draws_m.m == 0);
  CHECK(no_draws_m.tie == 7);
  // N = k = 0: the single admissible m.
  CHECK(cppunivariate::hyper_mle(as_span(zero), 0, 0, kNaN).status == cppunivariate::HyperStatus::Ok);
  // All-zero data with N known: m = 0 is attained.
  auto m_zero = cppunivariate::hyper_mle(as_span(zeros), 3, 10, kNaN);
  CHECK(m_zero.status == cppunivariate::HyperStatus::Ok);
  CHECK(m_zero.m == 0);
  CHECK(m_zero.logLik == 0);

  const std::vector<double> y{1, 2};
  CHECK(cppunivariate::hyper_mle(as_span(y), 5, kNaN, kNaN).status == cppunivariate::HyperStatus::Ok);
  const double big = 9007199254740992.0;  // 2^53
  for (double k : {2.5, -1.0, big})
    CHECK(cppunivariate::hyper_mle(as_span(y), k, 10, kNaN).status == cppunivariate::HyperStatus::InvalidInput);
  CHECK(cppunivariate::hyper_mle(as_span(y), 5, big, kNaN).status == cppunivariate::HyperStatus::InvalidInput);
  CHECK(cppunivariate::hyper_mle(as_span(y), 5, 4, kNaN).status == cppunivariate::HyperStatus::InvalidInput);
  CHECK(cppunivariate::hyper_mle(as_span(y), 5, 10, 11).status == cppunivariate::HyperStatus::InvalidInput);
  CHECK(cppunivariate::hyper_mle(as_span(y), 5, kNaN, 1.5).status == cppunivariate::HyperStatus::InvalidInput);
  CHECK(cppunivariate::hyper_mle({y.data(), 0}, 5, 10, kNaN).status == cppunivariate::HyperStatus::InvalidInput);
  for (const std::vector<double> &bad : {std::vector<double>{1.5}, std::vector<double>{-1}})
    CHECK(cppunivariate::hyper_mle(as_span(bad), 5, 10, kNaN).status == cppunivariate::HyperStatus::InvalidInput);

  const std::vector<double> four{4}, three{3}, spread{0, 5};
  CHECK(cppunivariate::hyper_mle(as_span(four), 3, 10, kNaN).status == cppunivariate::HyperStatus::Infeasible);
  CHECK(cppunivariate::hyper_mle(as_span(three), 5, kNaN, 2).status == cppunivariate::HyperStatus::Infeasible);
  CHECK(cppunivariate::hyper_mle(as_span(spread), 5, 8, kNaN).status == cppunivariate::HyperStatus::Infeasible);
  CHECK(cppunivariate::hyper_mle(as_span(zero), 5, 10, 8).status == cppunivariate::HyperStatus::Infeasible);
}

TEST_CASE("hyper: large populations stay exact or report TooLarge") {
  namespace hd = cppunivariate::hyper_detail;
  const std::vector<double> x{1, 2, 0, 1};
  const double m = 1e6, k = 1e6;
  const auto fit = cppunivariate::hyper_mle(as_span(x), k, kNaN, m);
  REQUIRE(fit.status == cppunivariate::HyperStatus::Ok);
  const auto c = hd::tabulate(as_span(x));
  // The sign change sits between N and N + 1, near m k / mean(x) = 1e12.
  CHECK(hd::increment_N(c, m, k, fit.N).value() > 0);
  CHECK(hd::increment_N(c, m, k, fit.N + 1).value() <= 0);
  CHECK(std::fabs(fit.N / 1e12 - 1) < 1e-5);
  CHECK(fit.iterations < 200);
  const std::vector<double> one{1};
  CHECK(cppunivariate::hyper_mle(as_span(one), 1099511627776.0, kNaN, 1099511627776.0).status ==
        cppunivariate::HyperStatus::TooLarge);
  const std::vector<double> zero{0};
  CHECK(cppunivariate::hyper_mle(as_span(zero), 10, kNaN, 9007199254740991.0).status ==
        cppunivariate::HyperStatus::TooLarge);
  // An optimum at the last admissible integer still needs its right
  // neighbour checked (2^53 is exact as an internal search sentinel).
  const std::vector<double> cap_data{9007199254740989.0};
  const auto cap_fit = cppunivariate::hyper_mle(as_span(cap_data),
      9007199254740990.0, kNaN, 9007199254740990.0);
  CHECK(cap_fit.status == cppunivariate::HyperStatus::Ok);
  CHECK(cap_fit.N == 9007199254740991.0);
  // Known N near 2^53: the m search is a bisection on an exact interval.
  const auto wide = cppunivariate::hyper_mle(as_span(x), 1000, 9007199254740991.0, kNaN);
  CHECK(wide.status == cppunivariate::HyperStatus::Ok);
  CHECK(hd::increment_m(c, 9007199254740991.0, 1000, wide.m).value() > 0);
  CHECK(hd::increment_m(c, 9007199254740991.0, 1000, wide.m + 1).value() <= 0);
}

TEST_CASE("hyper dpqr: CDF tails, quantile inversion, sampler support") {
  const double N = 40, m = 15, k = 12;
  for (double q = -1; q <= 13; ++q) {
    const double lower = dist::phyper(q, m, N, k, true, false);
    const double upper = dist::phyper(q, m, N, k, false, false);
    CHECK(lower + upper == doctest::Approx(1.0).epsilon(1e-14));
    if (q >= 0 && q <= 12 && lower > 0) {
      CHECK(dist::qhyper(lower, m, N, k, true, false) == q);
      CHECK(dist::qhyper(std::log(lower), m, N, k, true, true) == q);
    }
  }
  CHECK(dist::qhyper(0, m, N, k, true, false) == 0);
  CHECK(dist::qhyper(1, m, N, k, true, false) == 12);
  CHECK(dist::qhyper(0, 35, 40, 12, true, false) == 7);
  CHECK(dist::qhyper(1, 1, 1e15, 1, true, false) == 1);
  CHECK(dist::qhyper(0, 1, 1e15, 1, false, false) == 1);
  CHECK(std::isnan(dist::dhyper(1, 11, 10, 3, false)));
  CHECK(std::isnan(dist::phyper(1, 5, 10, 11, true, false)));
  CHECK(std::isnan(dist::qhyper(1.5, 5, 10, 3, true, false)));
  CHECK(dist::dhyper(2.5, 5, 10, 3, false) == 0);
  dist::StdRng<> rng(7);
  double total = 0;
  for (int i = 0; i < 4000; ++i) {
    const double v = dist::rhyper(rng, m, N, k);
    CHECK(v >= 0);
    CHECK(v <= 12);
    total += v;
  }
  CHECK(total / 4000 == doctest::Approx(k * m / N).epsilon(0.03));
}

// Independent high-precision oracle; see quality/truth.py.
TEST_CASE("hyper: d/p/q/r against the truth table") {
  mltest::check_dpqr_truth("hyper",
      [](double x, const std::vector<double> &p, bool log) { return cppunivariate::distributions::dhyper(x, p[0], p[1], p[2], log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::phyper(x, p[0], p[1], p[2], lower, log); },
      [](double x, const std::vector<double> &p, bool lower, bool log) { return cppunivariate::distributions::qhyper(x, p[0], p[1], p[2], lower, log); },
      [](cppunivariate::distributions::StdRng<> &rng, const std::vector<double> &p) { return cppunivariate::distributions::rhyper(rng, p[0], p[1], p[2]); });
}

TEST_CASE("hyper: population size holds and free-N fits return the design coefficient") {
  const std::vector<double> x{1,2,3,2};
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const auto held = cppunivariate::hyper_mle(mltest::as_span(x),5,20,nan);
  CHECK(held.N == 20);
  const auto free = cppunivariate::hyper_mle(mltest::as_span(x),5,nan,8);
  CHECK(free.m == 8);
  CHECK(std::isfinite(free.N));
  CHECK(free.logLik == doctest::Approx(cppunivariate::hyper_mle(
      mltest::as_span(x),5,free.N,8).logLik));
}

TEST_CASE("hyper: README total population validation") {
  cppunivariate::distributions::StdRng<> rng(94);
  for (double N : {3.0, 10.5, double(INFINITY), double(NAN)}) {
    CHECK(std::isnan(cppunivariate::distributions::dhyper(0.5, 4.0, N, 2.0, true)));
    CHECK(std::isnan(cppunivariate::distributions::phyper(0.5, 4.0, N, 2.0, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::qhyper(0.5, 4.0, N, 2.0, false, true)));
    CHECK(std::isnan(cppunivariate::distributions::rhyper(rng, 4.0, N, 2.0)));
  }
}

TEST_CASE("hyper: free draws agree with a complete finite grid") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  for (int N = 1; N <= 20; ++N) for (int m = 0; m <= N; ++m)
    for (int x = 0; x <= m; ++x) {
      const std::vector<double> data{double(x), double(x)};
      const auto fit = cppunivariate::hyper_mle(mltest::as_span(data), nan, N, m);
      double best = -std::numeric_limits<double>::infinity();
      for (int k = x; k <= N - m + x; ++k)
        best = std::max(best, 2 * cppunivariate::hyper_log_pmf(x, N, m, k));
      CHECK(fit.m == m);
      CHECK(fit.N == N);
      CHECK(std::fabs(fit.logLik - best) < 1e-11);
    }
}

TEST_CASE("hyper: held population joint finite grid") {
  for (int N = 2; N <= 18; ++N) for (int a = 0; a <= N; ++a) {
    const std::vector<double> x{double(a), double(std::min(N, a + 2))};
    const auto f = cppunivariate::hyper_mle(mltest::as_span(x), kNaN, N, kNaN);
    long double best = -1e100L;
    for (int m = 0; m <= N; ++m) for (int k = 0; k <= N; ++k) {
      if (x.back() > std::min(m,k) || x.front() < k-(N-m)) continue;
      best = std::max(best, brute_ll(x,N,m,k));
    }
    if (best == -1e100L) { CHECK(f.status == cppunivariate::HyperStatus::Infeasible); continue; }
    CHECK(f.status == cppunivariate::HyperStatus::Ok);
    CHECK(std::fabs(f.logLik-best) < 1e-11);
    CHECK(f.N == N);
    CHECK(f.m == std::floor(f.m)); CHECK(f.k == std::floor(f.k));
  }
  const std::vector<double> x{0,1};
  CHECK(cppunivariate::hyper_mle(mltest::as_span(x),kNaN,10000,kNaN).status == cppunivariate::HyperStatus::NumericalFailure);
}

// Hyper(N=10,m=5,k=5) is symmetric: F(2)=1/2 exactly.
TEST_CASE("hyper: symmetric lattice median allows adjacent log-half answers") {
  const auto adjacent=[](double x,double lo) {return x==lo || x==lo+1;};
  CHECK(cppunivariate::distributions::qhyper(.31,5,10,5,true,false)==2);
  CHECK(cppunivariate::distributions::qhyper(.31,5,10,5,false,false)==3);

  CHECK(adjacent(cppunivariate::distributions::qhyper(.5,5,10,5,true,false),2));
  CHECK(adjacent(cppunivariate::distributions::qhyper(.5,5,10,5,false,false),2));
  CHECK(adjacent(cppunivariate::distributions::qhyper(-0.6931471805599453,5,10,5,true,true),2));
  CHECK(adjacent(cppunivariate::distributions::qhyper(-0.6931471805599453,5,10,5,false,true),2));
}

TEST_CASE("hyper: certified joint population profiles and unresolved face") {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const std::vector<double> x{2,2,2,3,3,3};
  const auto f = cppunivariate::hyper_mle(mltest::as_span(x),5,nan,nan);
  REQUIRE(f.status == cppunivariate::HyperStatus::Ok);
  double best = -INFINITY;
  for (int N=5; N<=500; ++N) for (int m=0; m<=N; ++m) {
    double ll=0; for (double v:x) ll+=cppunivariate::hyper_log_pmf(v,N,m,5);
    best=std::max(best,ll);
  }
  CHECK(f.logLik == doctest::Approx(best).epsilon(1e-10));
  const auto symmetric = cppunivariate::hyper_mle(mltest::as_span(x),nan,nan,5);
  CHECK(symmetric.status == cppunivariate::HyperStatus::Ok);
  CHECK(symmetric.N == f.N); CHECK(symmetric.k == f.m);
  const std::vector<double> tie{0,1};
  CHECK(cppunivariate::hyper_mle(mltest::as_span(tie),1,nan,nan).status == cppunivariate::HyperStatus::Ok);
  const std::vector<double> spread{0,0,0,5,5,5};
  CHECK(cppunivariate::hyper_mle(mltest::as_span(spread),5,nan,nan).status == cppunivariate::HyperStatus::FaceUnresolved);
  const std::vector<double> large{0,1};
  CHECK(cppunivariate::hyper_mle(mltest::as_span(large),100001,nan,nan).status == cppunivariate::HyperStatus::NumericalFailure);
}
