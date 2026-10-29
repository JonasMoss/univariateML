// Hypergeometric MLE with a known number of draws k.
//
// Coefficients: N (population size), m (successes in the population) and
// k (draws); R's stats::dhyper(x, m, n = N - m, k). Observations are iid
// repetitions from the same urn. Exactly one of N, m is estimated, on a
// feasible integer set:
//
//   known N, k: m in [max(x), N - k + min(x)];
//   known m, k: N in [m + k - min(x), infinity).
//
// Both one-parameter likelihoods are unimodal, so the maximiser is the last
// t at which the adjacent log-ratio D(t) = log L(t) - log L(t - 1) is
// positive; the fitter brackets that sign change and bisects it.
//
// * m: L(m + 1) / L(m) = prod_i (m + 1) / (m + 1 - x_i) *
//   (N - m - k + x_i) / (N - m), positive factors that never increase in m;
//   for k >= 1 every observation has a strictly decreasing factor.
// * N: D(N) >= 0 iff A(N) >= G(N), where A(u) = (u - m)(u - k) / u and G is
//   the geometric mean of u - m - (k - x_i). G is concave, so A - G is
//   convex; it tends to -mean(x) < 0, so it never increases and has at most
//   one zero: D changes sign once, from + to -. Once D(t) <= 0, L does not
//   increase beyond t (the tail certificate). For u > m + k, log1p(z) <= z
//   bounds D(u)/h by (k - mean(x))/(u - m - k) - k/u. Thus D(u) < 0
//   for every u > k (m + k) / mean(x), so doubling from the
//   lower end reaches a certificate after finitely many steps.
//
// D is a compensated long-double sum of log1p of exact integer ratios. A
// |D| within 64 long-double ulps of the summed term magnitudes is a tie
// between adjacent maximisers, up to rounding; the smaller is returned with
// status Tie.
// Integers are exact in double precision only up to 2^53, so every count is
// at most 2^53 - 1.

#pragma once

#include "../../core/span.hpp"

#include <algorithm>
#include <cfloat>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class HyperStatus {
  Ok = 0,
  Tie,           // two adjacent maximisers; `tie` holds the larger one
  Unidentified,  // every feasible value maximises (k = 0, or m = 0 for N)
  AllZero,       // N free, m, k > 0, all x = 0: sup at N -> infinity
  BothFree,      // N and m both free: not supported
  InvalidInput,  // not whole numbers in [0, 2^53 - 1], k > N or m > N
  Infeasible,    // no admissible value gives all observations probability
  TooLarge       // the maximising N would exceed 2^53 - 1
};

struct HyperResult {
  double N;
  double m;
  double k;
  double logLik;
  double tie;  // other end of a tie / unidentified set, else NaN
  int iterations;
  HyperStatus status;
};

namespace hyper_detail {

constexpr double kMaxCount = 9007199254740991.0;  // 2^53 - 1

inline bool is_count(double v) {
  return v >= 0.0 && v <= kMaxCount && v == std::floor(v);
}

// Loader's saddle-point decomposition of the binomial pmf, as in R's
// dbinom_raw: stirlerr(n) = log(n!) - log(sqrt(2 pi n) (n / e)^n) for
// whole n >= 1 and bd0(x, np) = x log(x / np) + np - x.
inline double stirlerr(double n) {
  static const double table[16] = {
      0.0,
      0.08106146679532725821967,
      0.04134069595540929409382,
      0.02767792568499833914879,
      0.02079067210376509311152,
      0.01664469118982119216319,
      0.01387612882307074799875,
      0.01189670994589177009506,
      0.01041126526197209649748,
      0.009255462182712732917729,
      0.008330563433362871256469,
      0.007573675487951840794972,
      0.006942840107209529865664,
      0.00640899418800420706844,
      0.005951370112758847735624,
      0.005554733551962801371039};
  if (n <= 15.0) return table[static_cast<int>(n)];
  const double s0 = 1.0 / 12, s1 = 1.0 / 360, s2 = 1.0 / 1260,
               s3 = 1.0 / 1680, s4 = 1.0 / 1188;
  const double nn = n * n;
  if (n > 500) return (s0 - s1 / nn) / n;
  if (n > 80) return (s0 - (s1 - s2 / nn) / nn) / n;
  if (n > 35) return (s0 - (s1 - (s2 - s3 / nn) / nn) / nn) / n;
  return (s0 - (s1 - (s2 - (s3 - s4 / nn) / nn) / nn) / nn) / n;
}

inline double bd0(double x, double np) {
  if (std::fabs(x - np) < 0.1 * (x + np)) {
    double v = (x - np) / (x + np);
    double s = (x - np) * v;
    if (std::fabs(s) < DBL_MIN) return s;
    double ej = 2.0 * x * v;
    v *= v;
    for (int j = 1; j < 1000; ++j) {
      ej *= v;
      const double s1 = s + ej / (2 * j + 1);
      if (s1 == s) return s1;
      s = s1;
    }
  }
  return x * std::log(x / np) + np - x;
}

// log of C(n, x) p^x q^(n - x) for whole 0 <= x <= n, p + q = 1.
inline double lbinom_raw(double x, double n, double p, double q) {
  constexpr double kNegInf = -std::numeric_limits<double>::infinity();
  if (p == 0.0) return x == 0.0 ? 0.0 : kNegInf;
  if (q == 0.0) return x == n ? 0.0 : kNegInf;
  if (x == 0.0) {
    if (n == 0.0) return 0.0;
    return p < 0.1 ? -bd0(n, n * q) - n * p : n * std::log(q);
  }
  if (x == n) return q < 0.1 ? -bd0(n, n * p) - n * q : n * std::log(p);
  const double lc = stirlerr(n) - stirlerr(x) - stirlerr(n - x) -
                    bd0(x, n * p) - bd0(n - x, n * q);
  const double lf = 1.837877066409345483560659472811 + std::log(x) +
                    std::log1p(-x / n);
  return lc - 0.5 * lf;
}

// Distinct observed values and their multiplicities.
struct Counts {
  std::vector<double> value;
  std::vector<double> count;
  double h = 0.0, sum = 0.0, min = 0.0, max = 0.0;
};

inline Counts tabulate(span x) {
  std::vector<double> v(x.begin(), x.end());
  std::sort(v.begin(), v.end());
  Counts c;
  for (double z : v) {
    if (c.value.empty() || c.value.back() != z) {
      c.value.push_back(z);
      c.count.push_back(0.0);
    }
    c.count.back() += 1.0;
    c.sum += z;
  }
  c.h = static_cast<double>(v.size());
  c.min = v.front();
  c.max = v.back();
  return c;
}

// Neumaier-compensated sum that also accumulates the term magnitudes.
struct Sum {
  long double s = 0.0L, c = 0.0L, scale = 0.0L;
  void add(long double t) {
    const long double u = s + t;
    c += std::fabs(s) >= std::fabs(t) ? (s - u) + t : (t - u) + s;
    s = u;
    scale += std::fabs(t);
  }
  long double value() const { return s + c; }
};

// Compensated difference a*b-c*d. The fused residual preserves small
// integer differences even when the individual products exceed 2^64.
inline long double product_difference(long double a, long double b,
                                      long double c, long double d) {
  const long double ab = a * b;
  return std::fma(-c, d, ab) + std::fma(a, b, -ab);
}

// Combine the two factors before taking logs. This avoids cancellation of
// O(k/N) terms when the adjacent log-ratio is only O(1/N^2).
inline Sum increment_m(const Counts &c, double N, double k, double m) {
  Sum d;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    const long double v = c.value[j], w = c.count[j];
    const long double failures = static_cast<long double>(N) - m + 1;
    const long double z = product_difference(v, N + 1.0L, m, k) /
                          ((m - v) * failures);
    if (std::fabs(z) < 0.5L) {
      d.add(w * std::log1p(z));
    } else {
      if (v > 0) d.add(w * std::log1p(v / (m - v)));
      if (v < k) d.add(w * std::log1p(-(k - v) / failures));
    }
  }
  return d;
}

inline Sum increment_N(const Counts &c, double m, double k, double N) {
  Sum d;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    const long double v = c.value[j], w = c.count[j], y = k - v;
    const long double remaining = static_cast<long double>(N) - m - y;
    const long double z = product_difference(m, k, N, v) /
                          (static_cast<long double>(N) * remaining);
    if (std::fabs(z) < 0.5L) {
      d.add(w * std::log1p(z));
    } else {
      if (y > 0) d.add(w * std::log1p(y / remaining));
      d.add(w * std::log1p(-static_cast<long double>(k) / N));
    }
  }
  return d;
}

// Rounding bound for D, relative to its summed term magnitudes: each term
// is log1p of a once-rounded ratio of exact integers.
constexpr long double kTieUlps = 64 * std::numeric_limits<long double>::epsilon();

struct Peak {
  double at;
  double tie;
  int evaluations;
  bool too_large;
};

// Last t in [lo, hi] (or [lo, 2^53 - 1] when open) with D(t) > 0, or lo,
// for a D that changes sign at most once, from + to -.
template <typename Increment>
inline Peak unimodal_peak(double lo, double hi, bool open, Increment D) {
  Peak p{lo, std::numeric_limits<double>::quiet_NaN(), 0, false};
  if (!open && lo == hi) return p;
  auto rising = [&](double t) {
    ++p.evaluations;
    return D(t).value() > 0;
  };
  double a = lo, b = hi + 1.0;  // L(hi + 1) = 0 on a closed interval
  if (open) {
    for (double step = 1.0;; step *= 2.0) {
      const double t = std::min(lo + step, kMaxCount + 1.0);
      if (!rising(t)) {
        b = t;
        break;
      }
      a = t;
      if (t == kMaxCount + 1.0) {
        p.too_large = true;
        return p;
      }
    }
  }
  while (b - a > 1.0) {
    const double mid = a + std::floor((b - a) / 2.0);
    if (rising(mid)) a = mid; else b = mid;
  }
  auto flat = [&](double t) {
    ++p.evaluations;
    const Sum d = D(t);
    return std::fabs(d.value()) <= kTieUlps * d.scale;
  };
  p.at = a;
  if (a > lo && flat(a)) {
    p.at = a - 1.0;
    p.tie = a;
  } else if ((open ? a <= kMaxCount : a < hi) && flat(a + 1.0)) {
    p.tie = a + 1.0;
  }
  return p;
}

}  // namespace hyper_detail

// log P(X = x) for X ~ Hypergeometric(N, m, k), with whole N, m, k,
// m <= N, k <= N; R's dhyper(x, m, N - m, k, log = TRUE).
inline double hyper_log_pmf(double x, double N, double m, double k) {
  constexpr double kNegInf = -std::numeric_limits<double>::infinity();
  if (x != std::floor(x) || x < std::max(0.0, k - (N - m)) ||
      x > std::min(k, m)) {
    return kNegInf;
  }
  if (k == 0.0) return 0.0;
  const double p = k / N, q = (N - k) / N;
  return hyper_detail::lbinom_raw(x, m, p, q) +
         hyper_detail::lbinom_raw(k - x, N - m, p, q) -
         hyper_detail::lbinom_raw(k, N, p, q);
}

// N or m NaN: estimate it. k is always held.
inline HyperResult hyper_mle(span x, double k, double N, double m) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kInf = std::numeric_limits<double>::infinity();
  HyperResult r{N, m, k, kNaN, kNaN, 0, HyperStatus::InvalidInput};
  const bool estimate_N = std::isnan(N), estimate_m = std::isnan(m);
  if (x.size() == 0 || !hyper_detail::is_count(k) ||
      (!estimate_N && !hyper_detail::is_count(N)) ||
      (!estimate_m && !hyper_detail::is_count(m))) {
    return r;
  }
  for (double v : x) {
    if (!hyper_detail::is_count(v)) return r;
  }
  if (!estimate_N && (k > N || (!estimate_m && m > N))) return r;
  if (estimate_N && estimate_m) {
    r.status = HyperStatus::BothFree;
    return r;
  }
  const hyper_detail::Counts c = hyper_detail::tabulate(x);
  r.status = HyperStatus::Infeasible;
  if (c.max > k || (!estimate_m && c.max > m)) return r;

  if (!estimate_N && !estimate_m) {
    if (c.min < k - (N - m)) return r;
    r.status = HyperStatus::Ok;
  } else if (estimate_m) {
    const double lo = c.max, hi = N - k + c.min;
    if (hi < lo) return r;
    if (k == 0.0 && lo < hi) {
      r.m = lo;
      r.tie = hi;
      r.status = HyperStatus::Unidentified;
    } else {
      const hyper_detail::Peak p = hyper_detail::unimodal_peak(
          lo, hi, false,
          [&](double t) { return hyper_detail::increment_m(c, N, k, t); });
      r.m = p.at;
      r.tie = p.tie;
      r.iterations = p.evaluations;
      r.status = std::isnan(p.tie) ? HyperStatus::Ok : HyperStatus::Tie;
    }
  } else {
    const long double lo_exact = static_cast<long double>(m) + k - c.min;
    if (lo_exact > hyper_detail::kMaxCount) {
      r.status = HyperStatus::TooLarge;
      return r;
    }
    const double lo = static_cast<double>(lo_exact);
    if (c.sum == 0.0 && (m == 0.0 || k == 0.0)) {
      r.N = lo;
      r.tie = kInf;
      r.status = HyperStatus::Unidentified;
    } else if (c.sum == 0.0) {
      r.N = kInf;
      r.logLik = 0.0;
      r.status = HyperStatus::AllZero;
      return r;
    } else {
      const hyper_detail::Peak p = hyper_detail::unimodal_peak(
          lo, hyper_detail::kMaxCount, true,
          [&](double t) { return hyper_detail::increment_N(c, m, k, t); });
      r.iterations = p.evaluations;
      if (p.too_large) {
        r.status = HyperStatus::TooLarge;
        return r;
      }
      r.N = p.at;
      r.tie = p.tie;
      r.status = std::isnan(p.tie) ? HyperStatus::Ok : HyperStatus::Tie;
    }
  }
  long double ll = 0.0L;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    ll += c.count[j] * hyper_log_pmf(c.value[j], r.N, r.m, k);
  }
  r.logLik = static_cast<double>(ll);
  return r;
}

}  // namespace cppunivariate
