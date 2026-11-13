// Hypergeometric MLE: one-parameter searches and a finite grid with N held.
//
// Coefficients: N (population size), m (successes in the population) and
// k (draws); R's stats::dhyper(x, m, n = N - m, k). Observations are iid
// repetitions from the same urn. With k held, at most one of N, m is
// estimated on a feasible integer set:
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
#include "../../core/outcome.hpp"
#include "../../core/discrete_stop.hpp"
#include "../../core/precision.hpp"

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
  FaceUnresolved, // no strict improvement found in the exploratory search
  FaceTie,       // a finite point matched the face numerically; globality unknown
  InvalidInput,  // not whole numbers in [0, 2^53 - 1], k > N or m > N
  Infeasible,    // no admissible value gives all observations probability
  NumericalFailure, // finite grid exceeds budget or nonfinite likelihood
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
  StopDetail stop_detail{};
  bool certified = true;
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
  CPPUNIV_EXTENDED_REAL s = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), c = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L), scale = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  void add(CPPUNIV_EXTENDED_REAL t) {
    const CPPUNIV_EXTENDED_REAL u = s + t;
    c += std::fabs(s) >= std::fabs(t) ? (s - u) + t : (t - u) + s;
    s = u;
    scale += std::fabs(t);
  }
  CPPUNIV_EXTENDED_REAL value() const { return s + c; }
};

// Compensated difference a*b-c*d. The fused residual preserves small
// integer differences even when the individual products exceed 2^64.
inline CPPUNIV_EXTENDED_REAL product_difference(CPPUNIV_EXTENDED_REAL a, CPPUNIV_EXTENDED_REAL b,
                                      CPPUNIV_EXTENDED_REAL c, CPPUNIV_EXTENDED_REAL d) {
  const CPPUNIV_EXTENDED_REAL ab = a * b;
  return std::fma(-c, d, ab) + std::fma(a, b, -ab);
}

// Combine the two factors before taking logs. This avoids cancellation of
// O(k/N) terms when the adjacent log-ratio is only O(1/N^2).
inline Sum increment_m(const Counts &c, double N, double k, double m) {
  Sum d;
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    const CPPUNIV_EXTENDED_REAL v = c.value[j], w = c.count[j];
    const CPPUNIV_EXTENDED_REAL failures = static_cast<CPPUNIV_EXTENDED_REAL>(N) - m + 1;
    const CPPUNIV_EXTENDED_REAL z = product_difference(v, N + CPPUNIV_EXTENDED_LITERAL(1.0,1.0L), m, k) /
                          ((m - v) * failures);
    if (std::fabs(z) < CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)) {
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
    const CPPUNIV_EXTENDED_REAL v = c.value[j], w = c.count[j], y = k - v;
    const CPPUNIV_EXTENDED_REAL remaining = static_cast<CPPUNIV_EXTENDED_REAL>(N) - m - y;
    const CPPUNIV_EXTENDED_REAL z = product_difference(m, k, N, v) /
                          (static_cast<CPPUNIV_EXTENDED_REAL>(N) * remaining);
    if (std::fabs(z) < CPPUNIV_EXTENDED_LITERAL(0.5,0.5L)) {
      d.add(w * std::log1p(z));
    } else {
      if (y > 0) d.add(w * std::log1p(y / remaining));
      d.add(w * std::log1p(-static_cast<CPPUNIV_EXTENDED_REAL>(k) / N));
    }
  }
  return d;
}

// Rounding bound for D, relative to its summed term magnitudes: each term
// is log1p of a once-rounded ratio of exact integers.
constexpr CPPUNIV_EXTENDED_REAL kTieUlps = 64 * std::numeric_limits<CPPUNIV_EXTENDED_REAL>::epsilon();

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

// Finite-grid policy: two complete passes, at most 10^7 likelihood
// evaluations in total. Compare to the final maximum, avoiding chained ties.
constexpr CPPUNIV_EXTENDED_REAL kGridBudget = 10000000;
inline CPPUNIV_EXTENDED_REAL grid_choose(CPPUNIV_EXTENDED_REAL n, CPPUNIV_EXTENDED_REAL k) {
  return std::lgamma(n + 1) - std::lgamma(k + 1) - std::lgamma(n - k + 1);
}
inline bool grid_tied(CPPUNIV_EXTENDED_REAL a, CPPUNIV_EXTENDED_REAL b) {
  return std::fabs(a - b) <= 64 * std::numeric_limits<double>::epsilon() *
      std::max(std::fabs(a), std::fabs(b));
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

// NaN requests estimation: N held permits either or both m and k free.
inline HyperResult hyper_mle(span x, double k, double N, double m);
inline HyperResult hyper_mle_search(span x, double k, double N, double m) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  constexpr double kInf = std::numeric_limits<double>::infinity();
  HyperResult r{N, m, k, kNaN, kNaN, 0, HyperStatus::InvalidInput};
  if (std::isnan(N) && std::isnan(m) && std::isnan(k)) {
    if (x.size() == 0) return r;
    for (double v : x) if (!hyper_detail::is_count(v)) return r;
    const auto c = hyper_detail::tabulate(x);
    if (c.min == c.max) {
      r.N = c.max; r.m = c.max; r.k = c.max; r.logLik = 0;
      r.status = HyperStatus::Ok; return r;
    }
    r.status = HyperStatus::NumericalFailure;
    if(c.max>100000){
      double value=-kInf;
      for(double size=c.max;size<c.max+16 && size<=hyper_detail::kMaxCount;++size) {
        const double lo=size+c.max-c.min;
        for(double nn=lo;nn<lo+128 && nn<=hyper_detail::kMaxCount;++nn) {
          const auto seed=hyper_mle(x,size,nn,kNaN);
          r.iterations+=seed.iterations;
          if(std::isfinite(seed.logLik) && seed.logLik>value) {
            value=seed.logLik;r.stop_detail.candidates={{{seed.m,seed.N,seed.k},value,value}};
          }
        }
      }
      r.stop_detail=discrete_budget(r.stop_detail);
      return r;
    }
    if(!std::isfinite(c.sum)||c.sum<=0)return r;
    const double mean = c.sum / c.h;
    double poisson = 0;
    for (std::size_t j = 0; j < c.value.size(); ++j)
      poisson += c.count[j] * (c.value[j] * std::log(mean) - mean - std::lgamma(c.value[j] + 1));
    if (!std::isfinite(poisson)) return r;
    r.stop_detail=discrete_face(StopModel::Pois,{mean},poisson);
    const double margin = 1e-9 * (1 + std::fabs(poisson));
    double best = -kInf;
    HyperResult winner = r;
    int work = 0;
    auto evaluate = [&](double size, double nn) {
      if (work >= hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return false;}
      const HyperResult f = hyper_mle(x, size, nn, kNaN);
      work += std::max(1, f.iterations);
      r.iterations = work;
      if(work >= hyper_detail::kGridBudget)r.stop_detail=discrete_budget(r.stop_detail);
      if (work >= hyper_detail::kGridBudget ||
          (f.status != HyperStatus::Ok && f.status != HyperStatus::Tie &&
           f.status != HyperStatus::Unidentified) || !std::isfinite(f.logLik)) return false;
      if (f.logLik > best) {
        best = f.logLik; winner = f;
        r.stop_detail.candidates.erase(std::remove_if(r.stop_detail.candidates.begin(),r.stop_detail.candidates.end(),
          [](const FitCandidate& c){return c.model==StopModel::None;}),r.stop_detail.candidates.end());
        r.stop_detail.candidates.push_back({{f.m,f.N,f.k},f.logLik,f.logLik});
        winner.m = std::min(size, f.m); winner.k = std::max(size, f.m);
      }
      return true;
    };
    auto face = [&](double size) {
      ++work; r.iterations = work;
      if (work >= hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return kNaN;}
      const double p = mean / size;
      double ll = 0;
      for (std::size_t j = 0; j < c.value.size(); ++j)
        ll += c.count[j] * hyper_detail::lbinom_raw(c.value[j], size, p, 1-p);
      return ll;
    };
    // Search a growing rectangle in the smaller size and population. At
    // each population the existing unimodal search profiles the other size.
    const double exploration_sizes = 16;
    if (c.max + exploration_sizes + 256 >= hyper_detail::kMaxCount) return r;
    for (double size = c.max; size < c.max + exploration_sizes; ++size) {
      const double lo = size + c.max - c.min;
      for (double nn = lo; nn < lo + 128; ++nn)
        if (!evaluate(size, nn)) return r;
    }
    // A face at least as good as the candidate prevents a strict certificate.
    // tie records the observed binomial size, or zero for the Poisson face.
    double face_best = poisson, face_size = 0;
    for (double size = c.max; size < c.max + exploration_sizes; ++size) {
      const double b = face(size);
      if (!std::isfinite(b)) return r;
      if (b > face_best) { face_best = b; face_size = size; }
    }
    const double initial_margin = 1e-9 * (1 + std::fabs(face_best));
    if (best <= face_best + initial_margin) {
      r.status = std::fabs(best-face_best) <= initial_margin ? HyperStatus::FaceTie : HyperStatus::FaceUnresolved;
      r.stop_detail=discrete_face(face_size==0?StopModel::Pois:StopModel::Binom,
        face_size==0?std::vector<double>{mean}:std::vector<double>{face_size,mean/face_size},face_best);
      r.stop_detail.candidates.push_back({{winner.m,winner.N,winner.k},best,best});
      r.stop_detail.loglik_upper=INFINITY;
      r.tie = face_size; r.iterations = work; return r;
    }
    // Divide the candidate/Poisson gap between the two corrections. All
    // larger smaller-sizes are covered by the uniform Poisson envelope.
    const double gap = best - poisson - margin;
    if (!(gap > 0)) return r;
    const double den_s = -std::expm1(-gap / (4 * c.sum));
    const double den_n = -std::expm1(-gap / (2 * c.sum));
    if (!(den_s > 0) || !(den_n > 0)) return r;
    const double S = std::max(c.max + 1, std::floor(c.max / den_s) + 2);
    const double n0 = std::max(c.max + 1, std::floor(c.max / den_n) + 2);
    if(S>100000 && std::isfinite(S)){r.stop_detail=discrete_budget(r.stop_detail);return r;}
    if(!std::isfinite(S)||!std::isfinite(n0)||n0>=hyper_detail::kMaxCount)return r;
    // Each smaller-size face has its own, usually much smaller, cutoff.
    // -c_N(s) <= s(s-1)/(2(N-s+1)). Preflight before returning any fit.
    std::vector<double> ends;
    double populations = 0;
    for (double size = c.max; size < S; ++size) {
      const double b = face(size);
      if (!std::isfinite(b)) return r;
      const double face_margin = 1e-9 * (1 + std::fabs(b));
      if (best <= b + face_margin) {
        r.status = std::fabs(best-b) <= face_margin ? HyperStatus::FaceTie : HyperStatus::FaceUnresolved;
        r.stop_detail=discrete_face(StopModel::Binom,{size,mean/size},b);
        r.stop_detail.candidates.push_back({{winner.m,winner.N,winner.k},best,best});r.stop_detail.loglik_upper=INFINITY;
        r.tie = size; r.iterations = work; return r;
      }
      const double d = best - b - face_margin;
      const double cutoff = std::floor(size - 1 + c.h * size * (size - 1) / (2 * d)) + 2;
      const double lo = size + c.max - c.min;
      if (!std::isfinite(cutoff) || cutoff >= hyper_detail::kMaxCount) return r;
      const double end = std::max(lo, cutoff);
      ends.push_back(end);
      populations += end - lo;
      if (populations + work >= hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return r;}
    }
    // For sizes >= S the envelope applies only above n0. Below n0,
    // enumerate the smaller size through its support bound at each N.
    for (double nn = 2*c.max-c.min; nn < n0; ++nn) {
      const double hi = std::floor((nn + c.min) / 2);
      if (hi >= S) populations += hi - S + 1;
      if (populations + work >= hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return r;}
    }
    for (double size = c.max; size < S; ++size) {
      const double lo = size + c.max - c.min;
      for (double nn = lo; nn < ends[static_cast<std::size_t>(size-c.max)]; ++nn)
        if (!evaluate(size, nn)) return r;
    }
    for (double nn = 2*c.max-c.min; nn < n0; ++nn) {
      const double hi = std::floor((nn + c.min) / 2);
      for (double size = S; size <= hi; ++size)
        if (!evaluate(size, nn)) return r;
    }
    winner.iterations = work; winner.status = HyperStatus::Ok;
    return winner;
  }
  // One held sample/population size and both other counts free. The
  // composition profile at each population is found by the existing
  // one-coordinate search. Every population below the tail cutoff is visited.
  if (std::isnan(N) && (std::isnan(k) != std::isnan(m))) {
    const bool held_k = !std::isnan(k);
    const double size = held_k ? k : m;
    if (x.size() == 0 || !hyper_detail::is_count(size)) return r;
    for (double v : x) if (!hyper_detail::is_count(v)) return r;
    const auto c = hyper_detail::tabulate(x);
    r.status = HyperStatus::Infeasible;
    if (c.max > size) return r;
    // N=size makes the observed count deterministic when the sample is constant.
    if (c.min == c.max) {
      r.N = size; r.m = held_k ? c.max : size;
      r.k = held_k ? size : c.max; r.logLik = 0;
      r.status = HyperStatus::Ok; return r;
    }
    // For size one the entire family is Bernoulli, and p=sum(x)/h is
    // attained exactly by N=h and the free count sum(x).
    if (size == 1 && c.h <= hyper_detail::kMaxCount) {
      r.N = c.h; r.m = held_k ? c.sum : size;
      r.k = held_k ? size : c.sum;
      r.logLik = c.sum * std::log(c.sum/c.h) +
                 (c.h-c.sum) * std::log1p(-c.sum/c.h);
      r.status = HyperStatus::Ok; return r;
    }
    r.status = HyperStatus::NumericalFailure;
    // Explicit work guard also bounds the correction's summation.
    if(size>100000){
      double value=-kInf;
      const double lo=size+c.max-c.min;
      for(double nn=lo;nn<lo+256 && nn<=hyper_detail::kMaxCount;++nn) {
        const auto seed=hyper_mle(x,size,nn,kNaN);r.iterations+=seed.iterations;
        if(std::isfinite(seed.logLik) && seed.logLik>value) {
          value=seed.logLik;r.stop_detail.candidates={{{held_k?seed.m:size,seed.N,held_k?size:seed.m},value,value}};
        }
      }
      r.stop_detail=discrete_budget(r.stop_detail);
      return r;
    }
    if(size<1)return r;
    const double p = c.sum / (c.h * size);
    double face = 0;
    for (std::size_t j = 0; j < c.value.size(); ++j)
      face += c.count[j] * hyper_detail::lbinom_raw(c.value[j], size, p, 1-p);
    if (!std::isfinite(face)) return r;
    r.stop_detail=discrete_face(StopModel::Binom,{size,p},face);
    const double margin = 1e-9 * (1 + std::fabs(face));
    double best = -kInf;
    HyperResult winner = r;
    int work = 0;
    bool face_tie = false;
    auto evaluate = [&](double nn) {
      const HyperResult f = hyper_mle(x, size, nn, kNaN);
      work += std::max(1, f.iterations);
      if ((f.status != HyperStatus::Ok && f.status != HyperStatus::Tie &&
           f.status != HyperStatus::Unidentified) || !std::isfinite(f.logLik)) return false;
      if (f.logLik > best) { best = f.logLik; winner = f;
        r.stop_detail.candidates.erase(std::remove_if(r.stop_detail.candidates.begin(),r.stop_detail.candidates.end(),
          [](const FitCandidate& c){return c.model==StopModel::None;}),r.stop_detail.candidates.end());
        r.stop_detail.candidates.push_back({{held_k?f.m:size,f.N,held_k?size:f.m},f.logLik,f.logLik}); }
      if (std::fabs(f.logLik-face) <= margin) face_tie = true;
      return true;
    };
    const double lo = size + c.max - c.min;
    const double exploratory_end = lo + 255;
    if (exploratory_end >= hyper_detail::kMaxCount) return r;
    for (double nn = lo; nn <= exploratory_end; ++nn)
      if (!evaluate(nn)) return r;
    if (best <= face + margin) {
      r.stop_detail=discrete_face(StopModel::Binom,{size,p},face);
      r.stop_detail.candidates.push_back({{held_k?winner.m:size,winner.N,held_k?size:winner.m},best,best});r.stop_detail.loglik_upper=INFINITY;
      r.status = face_tie ? HyperStatus::FaceTie : HyperStatus::FaceUnresolved;
      r.iterations = work; return r;
    }
    auto correction = [&](double nn) {
      double s = 0;
      for (double j = 1; j < size; ++j) s -= std::log1p(-j/nn);
      return c.h * s;
    };
    double end = std::max(lo, 1.0);
    while (face + correction(end) >= best - margin) {
      if (end >= hyper_detail::kMaxCount / 2) return r;
      end *= 2;
    }
    double lower = lo;
    while (end - lower > 1) {
      const double mid = lower + std::floor((end-lower)/2);
      if (face + correction(mid) < best-margin) end = mid; else lower = mid;
    }
    if (end-lo > hyper_detail::kGridBudget / 4){r.stop_detail=discrete_budget(r.stop_detail);return r;}
    for (double nn = lo; nn < end; ++nn) {
      if(work>=hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return r;}
      else if(!evaluate(nn))return r;
    }
    if (work >= hyper_detail::kGridBudget){r.stop_detail=discrete_budget(r.stop_detail);return r;}
    if (held_k) r = winner;
    else { r = winner; r.k = winner.m; r.m = size; }
    r.iterations = work; r.status = HyperStatus::Ok;
    return r;
  }
  if (std::isnan(k) && std::isnan(m) && !std::isnan(N)) {
    if (x.size() == 0 || !hyper_detail::is_count(N)) return r;
    for (double v : x) if (!hyper_detail::is_count(v)) return r;
    const auto c = hyper_detail::tabulate(x);
    r.status = HyperStatus::Infeasible;
    const CPPUNIV_EXTENDED_REAL width = static_cast<CPPUNIV_EXTENDED_REAL>(N) - 2 * c.max + c.min + 1;
    if (width <= 0) return r;
    r.status = HyperStatus::NumericalFailure;
    const bool partial=width*(width+1)>hyper_detail::kGridBudget;
    CPPUNIV_EXTENDED_REAL best = -kInf;
    for (int pass = 0; pass < 2; ++pass) {
      for (double mm = c.max; mm <= N - c.max + c.min; ++mm) {
        for (double kk = c.max; kk <= N - mm + c.min; ++kk) {
          hyper_detail::Sum ll;
          for (std::size_t j = 0; j < c.value.size(); ++j) {
            const CPPUNIV_EXTENDED_REAL v = c.value[j];
            ll.add(c.count[j] * (hyper_detail::grid_choose(mm, v) +
                hyper_detail::grid_choose(static_cast<CPPUNIV_EXTENDED_REAL>(N) - mm, kk - v) -
                hyper_detail::grid_choose(N, kk)));
          }
          ++r.iterations;
          if (!std::isfinite(ll.value())) {
            if(partial && std::isfinite(r.logLik)){r.status=HyperStatus::Ok;r.certified=false;}
            return r;
          }
          if(partial) {
            if(ll.value()>best) {
              best=ll.value();r.m=mm;r.k=kk;r.logLik=static_cast<double>(best);
            }
            if(r.iterations>=hyper_detail::kGridBudget/2) {
              r.status=HyperStatus::Ok;r.certified=false;r.stop_detail=discrete_budget(r.stop_detail);return r;
            }
          }
          if (pass == 0) best = std::max(best, ll.value());
          else if (hyper_detail::grid_tied(ll.value(), best)) {
            r.m = mm; r.k = kk; r.logLik = static_cast<double>(ll.value());
            r.status = HyperStatus::Ok;
            return r; // lexicographic (m, N, k) traversal
          }
        }
      }
    }
    return r;
  }
  // The hypergeometric mass is symmetric in m and k. Reuse the exact
  // bounded composition search when both population parameters are held.
  if (std::isnan(k) && !std::isnan(N) && !std::isnan(m)) {
    HyperResult swapped = hyper_mle(x, m, N, kNaN);
    r = swapped;
    r.m = m;
    r.k = swapped.m;
    return r;
  }
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
    const CPPUNIV_EXTENDED_REAL lo_exact = static_cast<CPPUNIV_EXTENDED_REAL>(m) + k - c.min;
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
      r.stop_detail=discrete_point(0);r.status = HyperStatus::AllZero;
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
  CPPUNIV_EXTENDED_REAL ll = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (std::size_t j = 0; j < c.value.size(); ++j) {
    ll += c.count[j] * hyper_log_pmf(c.value[j], r.N, r.m, k);
  }
  r.logLik = static_cast<double>(ll);
  return r;
}

inline HyperResult hyper_mle(span x, double k, double N, double m) {
  auto out=hyper_mle_search(x, k, N, m);
  if(out.status==HyperStatus::FaceUnresolved || out.status==HyperStatus::FaceTie ||
     out.status==HyperStatus::NumericalFailure) {
    for(const auto& candidate:out.stop_detail.candidates) {
      if(candidate.model!=StopModel::None || candidate.coef.size()!=3 || !std::isfinite(candidate.loglik_lower))continue;
      if(out.certified || candidate.loglik_lower>out.logLik) {
        out.m=candidate.coef[0];
        out.N=candidate.coef[1];
        out.k=candidate.coef[2];
        out.logLik=candidate.loglik_lower;out.certified=false;
      }
    }
    if(!out.certified)out.status=HyperStatus::Ok;
  }
  return out;
}
}  // namespace cppunivariate

namespace cppunivariate {
inline FitOutcome outcome(const HyperResult& r) {
  if (r.stop_detail.budget_exhausted && r.status != HyperStatus::Ok &&
      r.status != HyperStatus::Tie && r.status != HyperStatus::Unidentified) return FitOutcome::budget();
  switch (r.status) {
    case HyperStatus::Ok: return FitOutcome::estimate(r.certified);
    case HyperStatus::Tie: return FitOutcome::estimate(r.certified, false);
    case HyperStatus::Unidentified: return FitOutcome::estimate(r.certified, false);
    case HyperStatus::AllZero: return FitOutcome::limit();
    case HyperStatus::BothFree: return FitOutcome::failure(OutcomeReason::Unsupported);
    case HyperStatus::FaceUnresolved: return FitOutcome::failure(OutcomeReason::Unresolved);
    case HyperStatus::FaceTie: return FitOutcome::estimate(false);
    case HyperStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case HyperStatus::Infeasible: return FitOutcome::nonexistence(OutcomeReason::Unattained);
    case HyperStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case HyperStatus::TooLarge: return FitOutcome::failure(OutcomeReason::Evaluation);
  }
  return {};
}
} // namespace cppunivariate
