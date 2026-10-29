// Beta-binomial MLE with known size N.
//
// Parametrisation (Griffiths 1973): p = alpha/(alpha+beta), s = 1/(alpha+beta).
// With A_j = #{x_i > j} and C_j = #{x_i < N - j}, j = 0..N-1, the
// log-likelihood is a sum of log rising factorials,
//   l(p, s) = sum_i log C(N, x_i)
//           + sum_j [A_j log(p + j s) + C_j log(1 - p + j s) - h log(1 + j s)],
// so huge shapes need no lbeta subtraction; s = 0 is the binomial face.
//
// Free fit. For fixed s, l is strictly concave in p (d2l/dp2 < 0), so the
// problem is the profile L(s) = max_p l(p, s). For s > h H_{N-1} / m
// (m = #{0 < x_i < N}, H the harmonic number), dl/ds < 0 for every p, so the
// maximiser lies in [0, h H_{N-1} / m]. The search scans a log-grid of s on
// that interval, refines every grid local maximum by safeguarded Newton on
// the profile score, and compares the best interior value with the binomial
// face L(0); the face wins ties at 1e-8 h. Tangent/chord bounds certify
// every interval including s = 0; an unresolved bound returns IterationFailure.
// Endpoint-only samples (all x_i in
// {0, N}, both present) have l(p, s) strictly increasing in s: the supremum is
// the two-point law on {0, N}. All-0 / all-N samples reach a point mass.
// N = 1 (Bernoulli) leaves alpha + beta unidentified.
//
// One shape fixed. With alpha = a fixed, dl/dc > 0 for
// c < C_0 / (h sum_j 1/(a+j)) and dl/dc < 0 for c > (N - xbar)(a + N)/xbar,
// so the maximiser lies in that interval; same log-grid scan and Newton
// refinement. Only all-0 / all-N samples escape (to a point mass). Fixing
// beta is the mirror image (x -> N - x).
//
// Cost: O(N) per likelihood evaluation, adaptive certificate work (at most
// 200000 interval evaluations); no unimodality assumption is made.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class BbinomStatus {
  Ok = 0,
  InvalidInput,      // empty data, negative count, invalid size/tol/maxit/fixed shape
  SizeTooSmall,      // max(x) > size
  BinomialLimit,     // alpha + beta -> infinity: Binomial(size, mean/size) wins
  TwoPointLimit,     // alpha, beta -> 0: all observations in {0, size}, both present
  PointMass,         // all observations 0 or all equal size
  Unidentified,      // size = 0, or size = 1 with both shapes free
  IterationFailure
};

struct BbinomResult {
  double size, alpha, beta, logLik;
  int iterations;
  BbinomStatus status;
};

namespace bbinom_detail {

struct Data {
  int size = 0;
  double n = 0, interior = 0, zeros = 0, tops = 0, sum = 0;
  double log_choose = 0;  // sum_i log C(N, x_i)
  std::vector<double> A, C;  // length N
};

// l(p, s) without the binomial coefficients; q = 1 - p passed separately.
inline double loglik(const Data &d, double p, double q, double s) {
  double ll = 0;
  for (int j = 0; j < d.size; ++j) {
    const double js = static_cast<double>(j) * s;
    if (d.A[j] > 0) ll += d.A[j] * std::log(p + js);
    if (d.C[j] > 0) ll += d.C[j] * std::log(q + js);
    ll -= d.n * std::log1p(js);
  }
  return ll;
}

// Binomial face: l(xbar/N, 0).
inline double binomial_face(const Data &d) {
  const double N = d.size, total = d.n * N;
  double ll = 0;
  if (d.sum > 0) ll += d.sum * std::log(d.sum / total);
  if (total - d.sum > 0) ll += (total - d.sum) * std::log((total - d.sum) / total);
  return ll;
}

struct Point {
  double s, p, value;
};

// argmax_p l(p, s): the score sum_j A_j/(p+js) - C_j/(1-p+js) decreases
// strictly from +inf to -inf on (0, 1). Safeguarded Newton.
inline double solve_p(const Data &d, double s, double p0) {
  if (s == 0) return d.sum / (d.n * d.size);
  double lo = 0, hi = 1, p = std::min(std::max(p0, 1e-300), 1 - 1e-16);
  if (!(p > 0 && p < 1)) p = 0.5;
  for (int it = 0; it < 200; ++it) {
    double g = 0, H = 0;
    for (int j = 0; j < d.size; ++j) {
      const double js = static_cast<double>(j) * s;
      const double a = p + js, c = (1 - p) + js;
      g += d.A[j] / a - d.C[j] / c;
      H += d.A[j] / (a * a) + d.C[j] / (c * c);
    }
    if (g > 0) lo = p; else hi = p;
    double next = p + g / H;
    if (!(next > lo && next < hi)) next = 0.5 * (lo + hi);
    if (std::abs(next - p) <= 1e-15 * std::min(p, 1 - p) || hi - lo <= 1e-300) {
      p = next;
      break;
    }
    p = next;
  }
  return p;
}

inline Point profile(const Data &d, double s, double p0) {
  const double p = solve_p(d, s, p0);
  return {s, p, loglik(d, p, 1 - p, s)};
}

// d/du and d2/du2 of the profile L at u = log s (envelope theorem; the
// second derivative is l_ss - l_ps^2 / l_pp in s, then the chain rule).
inline void profile_derivs(const Data &d, double s, double p, double &g, double &H) {
  double ls = 0, lpp = 0, lps = 0, lss = 0;
  for (int j = 0; j < d.size; ++j) {
    const double dj = static_cast<double>(j), js = dj * s;
    const double a = p + js, c = (1 - p) + js, e = 1 + js;
    const double ia = d.A[j] / a, ic = d.C[j] / c;
    const double ia2 = ia / a, ic2 = ic / c, ie2 = d.n / (e * e);
    ls += dj * (ia + ic - d.n / e);
    lpp -= ia2 + ic2;
    lps -= dj * (ia2 - ic2);
    lss -= dj * dj * (ia2 + ic2 - ie2);
  }
  const double L2 = lss - lps * lps / lpp;
  g = s * ls;
  H = s * ls + s * s * L2;
}

// One free shape b with the other shape k fixed; W_j = C_j (b = beta) or
// A_j (b = alpha). Returns d/du and d2/du2 of l at u = log b.
inline void shape_derivs(const Data &d, const std::vector<double> &W, double k, double b,
                         double &g, double &H) {
  g = 0; H = 0;
  for (int j = 0; j < d.size; ++j) {
    const double dj = static_cast<double>(j);
    const double bj = b + dj, t = k + b + dj;
    g += b * (W[j] / bj - d.n / t);
    H += W[j] * b * dj / (bj * bj) - d.n * b * (k + dj) / (t * t);
  }
}

// Maximise a smooth function of u over a grid scan. `deriv(u, g, H)` returns
// the value and sets the first two derivatives. Every grid interval on which
// g changes sign from + to - holds a local maximum; each is refined by
// Newton on g, safeguarded by bisection inside its bracket. The analytic
// derivative stays accurate where values are flat to rounding (near the
// binomial face), so no spurious maxima arise there. Returns the best value
// (-inf when no sign change exists) and its location.
template <typename Deriv>
inline double maximise_on_grid(const std::vector<double> &grid, double tol, int maxit,
                               Deriv deriv, double &best_u, int &iterations) {
  double best = -std::numeric_limits<double>::infinity();
  std::vector<double> g(grid.size()), H(grid.size());
  for (std::size_t k = 0; k < grid.size(); ++k) deriv(grid[k], g[k], H[k]);
  for (std::size_t k = 0; k + 1 < grid.size(); ++k) {
    if (!(g[k] > 0 && g[k + 1] <= 0)) continue;
    double a = grid[k], b = grid[k + 1];
    double u = 0.5 * (a + b), gu, Hu;
    double value = deriv(u, gu, Hu);
    for (int it = 0; it < maxit; ++it) {
      ++iterations;
      if (gu > 0) a = u; else b = u;
      double next = (Hu < 0) ? u - gu / Hu : 0.5 * (a + b);
      if (!(next > a && next < b)) next = 0.5 * (a + b);
      const double step = std::abs(next - u);
      u = next;
      value = deriv(u, gu, Hu);
      if (step < tol * (1 + std::abs(u)) || b - a < tol * (1 + std::abs(u))) break;
    }
    if (value > best) { best = value; best_u = u; }
  }
  return best;
}

// Certify the spaces between grid points without assuming unimodality.
// Write l = V(p,s) - D(s), with V jointly concave and D concave.
// A tangent to V and a chord to -D bound l above throughout each interval.
// The residual p score contributes at most |l_p|, since 0 <= p <= 1.
// This is a floating-point likelihood certificate, not interval arithmetic.
template <typename Bound>
inline bool certify(const std::vector<double> &grid, double best, double allowance,
                    Bound bound) {
  struct Interval { double a, b; };
  std::vector<Interval> pending;
  for (std::size_t k = 0; k + 1 < grid.size(); ++k)
    pending.push_back({grid[k], grid[k + 1]});
  int evaluations = 0;
  while (!pending.empty()) {
    const Interval z = pending.back();
    pending.pop_back();
    if (++evaluations > 200000) return false;
    const double m = z.a + (z.b - z.a) * 0.5;
    if (!(m > z.a && m < z.b)) return false;
    const double upper = bound(z.a, m, z.b);
    if (!std::isfinite(upper)) return false;
    if (upper <= best + allowance) continue;
    pending.push_back({z.a, m});
    pending.push_back({m, z.b});
  }
  return true;
}

inline double profile_bound(const Data &d, double a, double m, double b) {
  const double p = solve_p(d, m, d.sum / (d.n * d.size));
  const double value = loglik(d, p, 1 - p, m);
  double vs = 0, gp = 0, da = 0, db = 0;
  for (int j = 0; j < d.size; ++j) {
    const double jm = j * m;
    const double ia = d.A[j] / (p + jm), ic = d.C[j] / (1 - p + jm);
    vs += j * (ia + ic);
    gp += ia - ic;
    da += d.n * std::log1p(j * (a - m) / (1 + jm));
    db += d.n * std::log1p(j * (b - m) / (1 + jm));
  }
  const double rounding = 128 * std::numeric_limits<double>::epsilon() *
                          (1 + std::abs(value) + d.n * d.size);
  return value + std::abs(gp) + std::max(vs * (a - m) - da, vs * (b - m) - db) + rounding;
}

inline double shape_bound(const Data &d, double k, double a, double m, double b) {
  const double value = loglik(d, k / (k + m), m / (k + m), 1 / (k + m));
  double vs = 0, da = 0, db = 0;
  for (int j = 0; j < d.size; ++j) {
    vs += d.C[j] / (m + j);
    da += d.n * std::log1p((a - m) / (k + m + j));
    db += d.n * std::log1p((b - m) / (k + m + j));
  }
  const double rounding = 128 * std::numeric_limits<double>::epsilon() *
                          (1 + std::abs(value) + d.n * d.size);
  return value + std::max(vs * (a - m) - da, vs * (b - m) - db) + rounding;
}

// Grid of n points from lo to hi inclusive, at most `step` apart.
inline std::vector<double> log_grid(double lo, double hi, double step) {
  const int n = std::max(3, static_cast<int>(std::ceil((hi - lo) / step)) + 1);
  std::vector<double> u(n);
  for (int i = 0; i < n; ++i) u[i] = lo + (hi - lo) * i / (n - 1);
  return u;
}

}  // namespace bbinom_detail

// size: known number of trials (required). alpha_fixed / beta_fixed: NaN to
// estimate; at most one may be finite. tol: relative tolerance on log shape
// steps in the Newton refinement; maxit: iterations per refinement.
inline BbinomResult bbinom_mle(ispan x, double size, double tol = 1e-10, int maxit = 100,
                               double alpha_fixed = std::numeric_limits<double>::quiet_NaN(),
                               double beta_fixed = std::numeric_limits<double>::quiet_NaN()) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  BbinomResult out{size, nan, nan, nan, 0, BbinomStatus::InvalidInput};
  const bool fix_a = !std::isnan(alpha_fixed), fix_b = !std::isnan(beta_fixed);
  if (x.size() == 0 || !(tol > 0) || !std::isfinite(tol) || maxit < 1) return out;
  if (!std::isfinite(size) || size < 0 || size != std::floor(size) || size > 1e7) return out;
  if (fix_a && fix_b) return out;
  if (fix_a && !(alpha_fixed > 0 && std::isfinite(alpha_fixed))) return out;
  if (fix_b && !(beta_fixed > 0 && std::isfinite(beta_fixed))) return out;

  bbinom_detail::Data d;
  d.size = static_cast<int>(size);
  const int N = d.size;
  std::vector<double> counts(static_cast<std::size_t>(N) + 1, 0.0);
  for (int v : x) {
    if (v < 0) return out;
    if (v > N) { out.status = BbinomStatus::SizeTooSmall; return out; }
    counts[v] += 1;
  }
  d.n = static_cast<double>(x.size());
  d.zeros = counts[0];
  d.tops = counts[N];
  d.interior = N >= 1 ? d.n - d.zeros - d.tops : 0;
  const double lgN = special::lgamma(size + 1);
  for (int v = 0; v <= N; ++v) {
    if (counts[v] == 0) continue;
    d.sum += counts[v] * v;
    if (v > 0 && v < N)
      d.log_choose += counts[v] * (lgN - special::lgamma(v + 1.0) - special::lgamma(N - v + 1.0));
  }
  if (fix_a) out.alpha = alpha_fixed;
  if (fix_b) out.beta = beta_fixed;
  if (N == 0) { out.logLik = 0; out.status = BbinomStatus::Unidentified; return out; }
  if (d.zeros == d.n || d.tops == d.n) {
    out.logLik = 0; out.status = BbinomStatus::PointMass; return out;
  }
  if (N == 1 && !fix_a && !fix_b) {
    out.logLik = d.tops * std::log(d.tops / d.n) + d.zeros * std::log(d.zeros / d.n);
    out.status = BbinomStatus::Unidentified;
    return out;
  }
  // Mirror x -> N - x when the mean exceeds N/2 (free fit) or beta is fixed,
  // so the probability solved for is at most about 1/2 and the free shape
  // in a fixed fit is always beta.
  const bool mirror = (fix_a || fix_b) ? fix_b : (d.sum > 0.5 * d.n * N);
  d.A.assign(N, 0.0);
  d.C.assign(N, 0.0);
  {
    // A_j = #{x > j}, C_j = #{x < N - j}; with mirroring, x -> N - x swaps them.
    std::vector<double> cum(static_cast<std::size_t>(N) + 2, 0.0);  // cum[k] = #{x < k}
    for (int v = 0; v <= N; ++v) cum[v + 1] = cum[v] + counts[v];
    for (int j = 0; j < N; ++j) {
      const double above = d.n - cum[j + 1];
      const double below = cum[N - j];
      d.A[j] = mirror ? below : above;
      d.C[j] = mirror ? above : below;
    }
    if (mirror) { std::swap(d.zeros, d.tops); d.sum = d.n * N - d.sum; }
  }
  auto finish = [&](double a, double c, double value, int iterations, BbinomStatus status) {
    BbinomResult r{size, mirror ? c : a, mirror ? a : c, value + d.log_choose, iterations, status};
    return r;
  };

  if (fix_a || fix_b) {
    // Free shape c (beta after mirroring) with a held at the fixed value.
    const double a = fix_a ? alpha_fixed : beta_fixed;
    double inv = 0;
    for (int j = 0; j < N; ++j) inv += 1 / (a + j);
    const double c_lo = d.C[0] / (d.n * inv);
    const double c_hi = (d.n * N - d.sum) * (a + N) / d.sum;
    const double lo = std::log(c_lo) - 0.25, hi = std::log(c_hi) + 0.25;
    // Grid step 0.2 < 0.25: the end points lie where the sign of the score is known.
    const std::vector<double> u = bbinom_detail::log_grid(lo, hi, 0.2);
    auto deriv = [&](double uu, double &g, double &H) {
      const double c = std::exp(uu), t = a + c;
      bbinom_detail::shape_derivs(d, d.C, a, c, g, H);
      return bbinom_detail::loglik(d, a / t, c / t, 1 / t);
    };
    double best_u = 0;
    int iterations = 0;
    const double best = bbinom_detail::maximise_on_grid(u, tol, maxit, deriv, best_u, iterations);
    if (!std::isfinite(best)) {
      out.status = BbinomStatus::IterationFailure;
      return out;
    }
    std::vector<double> intervals;
    for (double uu : u) intervals.push_back(std::exp(uu));
    if (!bbinom_detail::certify(intervals, best, 1e-8 * d.n,
        [&](double l, double m, double h) { return bbinom_detail::shape_bound(d, a, l, m, h); })) {
      out.status = BbinomStatus::IterationFailure;
      return out;
    }
    return finish(a, std::exp(best_u), best, iterations, BbinomStatus::Ok);
  }

  // Free fit, N >= 2, both 0 < xbar < N.
  const double face = bbinom_detail::binomial_face(d);
  const double tie = 1e-8 * d.n;
  if (d.interior == 0) {
    // Endpoint-only: l increases strictly in s; supremum at the two-point law.
    out.logLik = d.tops * std::log(d.tops / d.n) + d.zeros * std::log(d.zeros / d.n);
    out.status = BbinomStatus::TwoPointLimit;
    return out;
  }
  out.logLik = face + d.log_choose;
  double harmonic = 0;
  for (int j = 1; j < N; ++j) harmonic += 1.0 / j;
  const double p0 = d.sum / (d.n * N);
  const double s_lo = 1e-12 * std::min(p0, 1 - p0) / (static_cast<double>(N) * N);
  const double s_hi = d.n * harmonic / d.interior;
  // The grid ends two steps beyond s_hi, where the profile score is negative.
  std::vector<double> u = bbinom_detail::log_grid(std::log(s_lo), std::log(s_hi) + 0.5, 0.25);
  double p_last = p0;
  auto deriv = [&](double uu, double &g, double &H) {
    const double s = std::exp(uu);
    p_last = bbinom_detail::solve_p(d, s, p_last);
    bbinom_detail::profile_derivs(d, s, p_last, g, H);
    return bbinom_detail::loglik(d, p_last, 1 - p_last, s);
  };
  double best_u = u[0];
  int iterations = 0;
  const double best = bbinom_detail::maximise_on_grid(u, tol, maxit, deriv, best_u, iterations);
  // Include s = 0: no arbitrary lower concentration cutoff in the certificate.
  std::vector<double> intervals{0};
  for (double uu : u) intervals.push_back(std::exp(uu));
  if (!bbinom_detail::certify(intervals, std::max(best, face), tie * 0.25,
      [&](double l, double m, double h) { return bbinom_detail::profile_bound(d, l, m, h); })) {
    out.status = BbinomStatus::IterationFailure;
    return out;
  }
  // Compare maximised likelihoods, independently of the boundary score.
  if (!(best > face + tie)) {
    out.iterations = iterations;
    out.status = BbinomStatus::BinomialLimit;
    return out;
  }
  const double s = std::exp(best_u);
  const double p = bbinom_detail::solve_p(d, s, p_last);
  const double value = bbinom_detail::loglik(d, p, 1 - p, s);
  return finish(p / s, (1 - p) / s, value, iterations, BbinomStatus::Ok);
}

}  // namespace cppunivariate
