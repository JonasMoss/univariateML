// Paralogistic and inverse paralogistic MLE for near-constant samples, solved in an offset coordinate.
//
// With c = min(x) and r_i = sigma * log(x_i / c) (sigma = +1 paralogis, -1 inverse paralogis), the
// log density is, in w_i = alpha * (r_i - mu) with theta = c * exp(sigma * mu),
//   2 log(alpha) + w_i - log(x_i) - (alpha + 1) * log1p(exp(w_i)).
// Samples whose relative spread is near 1e-16 have alpha of order 1e16 and mu of order 1e-15: mu is
// then a double with full relative precision, while theta = c (1 + t) shares one ulp of c with t.
// So the solve runs in (alpha, z = alpha * (mu - mean r)), where w_i = alpha (r_i - mean r) - z
// is O(1). The relative logs are log1p((x_i - c) / c), exact in the subtraction (Sterbenz).
// For fixed alpha the likelihood is concave in z (an exponential-family score, sum p_i = n/(alpha+1)
// with p_i the logistic of w_i): one root, found by safeguarded Newton. The profile in alpha is
// scanned on a geometric grid, its best point is bracketed and its score root bisected.
// The result is the correctly rounded exact maximizer: alpha is not re-optimized at the rounded
// scale, and logLik is evaluated at the returned coefficients. The bound on the maximizer is
// numerical (a grid scan and a score root), so the fit is returned uncertified.
#pragma once
#include "../../core/precision.hpp"
#include "../../core/span.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {
namespace paralogis_offset_detail {

constexpr double kMaxSpread = 1e-6;  // relative spread (max/min - 1) up to which the route is taken

using Real = ExtendedReal;  // long double, or double under BOOST_MATH_NO_LONG_DOUBLE_MATH_FUNCTIONS

inline Real sigmoid(Real w) {
  if (w >= 0) return 1 / (1 + std::exp(-w));
  const Real e = std::exp(w);
  return e / (1 + e);
}
inline Real softplus(Real w) { return w > 0 ? w + std::log1p(std::exp(-w)) : std::log1p(std::exp(w)); }

struct Problem {
  std::vector<double> r, rho;  // r_i, and r_i minus its mean
  double rbar = 0, sum_log_x = 0, c = 0, sigma = 1;
};

struct Point { double ll = 0, ga = 0, gz = 0, haa = 0, haz = 0, hzz = 0; };

// Log likelihood, and gradient and Hessian in (log alpha, z), accumulated in extended precision.
inline Point evaluate(const Problem& P, double alpha_in, Real z) {
  const Real alpha = alpha_in;
  const Real n = static_cast<Real>(P.rho.size());
  Real ll = 2 * n * std::log(alpha) - static_cast<Real>(P.sum_log_x), ga = 2 * n, gz = 0, haa = 0, haz = 0, hzz = 0;
  for (double rho : P.rho) {
    const Real s = alpha * rho, w = s - z, p = sigmoid(w), sp = softplus(w), q = p * (1 - p);
    ll += w - (alpha + 1) * sp;
    gz += -1 + (alpha + 1) * p;
    ga += s - alpha * sp - (alpha + 1) * p * s;
    hzz -= (alpha + 1) * q;
    haz += alpha * p + (alpha + 1) * q * s;
    haa += s - alpha * sp - (3 * alpha + 1) * p * s - (alpha + 1) * q * s * s;
  }
  Point out;
  out.ll = static_cast<double>(ll); out.ga = static_cast<double>(ga); out.gz = static_cast<double>(gz);
  out.haa = static_cast<double>(haa); out.haz = static_cast<double>(haz); out.hzz = static_cast<double>(hzz);
  return out;
}

// The root in z of sum p_i = n / (alpha + 1), by Newton on log(sum p_i) with a bisection bracket.
inline bool solve_z(const Problem& P, double alpha_in, Real& z) {
  const Real alpha = alpha_in, n = static_cast<Real>(P.rho.size());
  Real smin = INFINITY, smax = -INFINITY;
  for (double rho : P.rho) { smin = std::min(smin, alpha * rho); smax = std::max(smax, alpha * rho); }
  Real lo = smin - 745, hi = smax + 745, total = 0;
  for (double rho : P.rho) total += std::exp(alpha * rho - smax);
  Real v = smax + std::log(total) + std::log((alpha + 1) / n);
  if (!(v > lo && v < hi)) v = (lo + hi) / 2;
  const Real target = std::log(n / (alpha + 1));
  const Real eps = std::numeric_limits<Real>::epsilon();
  for (int it = 0; it < 400; ++it) {
    Real sum_p = 0, sum_q = 0;
    for (double rho : P.rho) {
      const Real p = sigmoid(alpha * rho - v);
      sum_p += p; sum_q += p * (1 - p);
    }
    const Real g = std::log(sum_p) - target;
    if (g > 0) lo = v; else hi = v;
    Real next = std::isfinite(g) && sum_p > 0 ? v + g * sum_p / sum_q : NAN;
    if (!(next > lo && next < hi)) next = (lo + hi) / 2;
    if (std::abs(next - v) <= 4 * eps * std::max<Real>(1, std::abs(v)) || !(hi > lo) || next == lo || next == hi) {
      z = next; return std::isfinite(z);
    }
    v = next;
  }
  return false;
}

struct Fit {
  bool ok = false;
  double shape = NAN, scale = NAN, logLik = NAN;
  int iterations = 0;
};

inline Fit fit(span x, bool inverse) {
  Fit out;
  const std::size_t n = x.size();
  if (n < 2) return out;
  double mn = INFINITY, mx = -INFINITY;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v)) return out;
    mn = std::min(mn, v); mx = std::max(mx, v);
  }
  if (!(mx > mn) || !(mx / mn - 1 <= kMaxSpread)) return out;
  Problem P;
  P.c = mn; P.sigma = inverse ? -1 : 1;
  for (double v : x) {
    P.r.push_back(P.sigma * std::log1p((v - mn) / mn));
    P.sum_log_x += std::log(v);
  }
  double sum = 0;
  for (double r : P.r) sum += r;
  P.rbar = sum / static_cast<double>(n);
  double lo_rho = INFINITY, hi_rho = -INFINITY;
  for (double r : P.r) { P.rho.push_back(r - P.rbar); lo_rho = std::min(lo_rho, P.rho.back()); hi_rho = std::max(hi_rho, P.rho.back()); }
  const double range = hi_rho - lo_rho;
  if (!(range > 0) || !std::isfinite(range)) return out;

  struct Grid { double alpha; Real z; double ll, ga; };
  const auto at = [&](double alpha, Grid& g) {
    g.alpha = alpha;
    if (!solve_z(P, alpha, g.z)) return false;
    const Point q = evaluate(P, alpha, g.z);
    g.ll = q.ll; g.ga = q.ga;
    return std::isfinite(g.ll) && std::isfinite(g.ga);
  };
  constexpr int kLow = -8, kHigh = 30;
  std::vector<Grid> grid;
  for (int k = kLow; k <= kHigh; ++k) {
    Grid g;
    if (!at(std::pow(1.5, k) / range, g)) { g.ll = -INFINITY; g.ga = NAN; }
    grid.push_back(g);
  }
  std::size_t best = 0;
  for (std::size_t k = 1; k < grid.size(); ++k) if (grid[k].ll > grid[best].ll) best = k;
  if (best == 0 || best + 1 == grid.size() || !std::isfinite(grid[best].ll)) return out;
  Grid left = grid[best - 1], right = grid[best + 1];
  if (!(std::isfinite(left.ga) && std::isfinite(right.ga) && left.ga > 0 && right.ga < 0)) return out;
  double a = left.alpha, b = right.alpha;
  int iterations = 0;
  Grid root;
  for (; iterations < 400; ++iterations) {
    const double mid = a + 0.5 * (b - a);
    if (!(mid > a && mid < b)) break;
    if (!at(mid, root)) return out;
    if (root.ga > 0) a = mid; else b = mid;
  }
  const double alpha = a + 0.5 * (b - a);
  if (!at(alpha, root)) return out;
  const Point q = evaluate(P, alpha, root.z);
  // The profile must be concave at the root: haa - haz^2 / hzz < 0.
  if (!(q.hzz < 0) || !(q.haa - q.haz * q.haz / q.hzz < 0)) return out;

  // theta = c * exp(sigma * mu), mu = rbar + z / alpha, rounded once: c * (1 + t) = fma(c, t, c).
  const Real mu_exact = static_cast<Real>(P.rbar) + root.z / alpha;
  const double scale = std::fma(P.c, static_cast<double>(std::expm1(P.sigma * mu_exact)), P.c);
  if (!(scale > 0) || !std::isfinite(scale) || !std::isfinite(alpha)) return out;
  // Log likelihood at the returned coefficients; log(scale / c) from an exact subtraction.
  const double ratio = scale / P.c;
  const double lambda = ratio > 0.5 && ratio < 2 ? std::log1p((scale - P.c) / P.c) : std::log(scale) - std::log(P.c);
  const double mu_returned = P.sigma * lambda;
  Real total = 2 * static_cast<Real>(n) * std::log(static_cast<Real>(alpha)) - static_cast<Real>(P.sum_log_x);
  for (double r : P.r) {
    const Real w = static_cast<Real>(alpha) * (static_cast<Real>(r) - static_cast<Real>(mu_returned));
    total += w - (static_cast<Real>(alpha) + 1) * softplus(w);
  }
  const double ll = static_cast<double>(total);
  if (!std::isfinite(ll)) return out;
  out.ok = true; out.shape = alpha; out.scale = scale; out.logLik = ll; out.iterations = iterations;
  return out;
}

}  // namespace paralogis_offset_detail
}  // namespace cppunivariate
