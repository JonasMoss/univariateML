// Transformed gamma (Stacy's generalized gamma) MLE, actuar parameterisation
//   f(x) = shape2 * u^shape1 * exp(-u) / (x * Gamma(shape1)),
//   u = (x / scale)^shape2.
//
// For fixed shape2 = tau, z = x^tau is gamma(shape1, scale^tau): shape1
// solves log(a) - digamma(a) = K and the scale is closed form. Write
// y = log(x), u = (y - mean(y)) / sd(y) (divisor n), t = tau * sd(y) and
// K(t) = log(mean(exp(t * u))). The profile log likelihood per observation,
// relative to the lognormal fit, is
//   Delta(t) = log(t) + log(a)/2 - stirling(a) - a*K(t) + 1/2,
//   a = a(K(t)),  dDelta/dt = 1/t - a*K'(t),
// where stirling(a) = lgamma(a) - (a - 1/2) log(a) + a - log(2 pi)/2.
// Delta -> 0 as t -> 0 (lognormal face, slope -mean(u^3)/6 there) and
// Delta -> log(2 pi)/2 - 1/2 - log(max u) as t -> inf (power face with
// endpoint max(x)), from below. The free fit scans t on a log grid,
// refines every sign change of the slope and compares with both faces;
// faces win ties at 1e-8 per observation. Fixed shape1 gives a strictly
// concave problem in log(t); fixed shape2 is closed form.
//
// Reuse (e.g. by a transformed beta fitter): trgamma_mle(x).logLik is the
// supremum of the likelihood over the family and its two faces; `status`
// says where it is attained. trgamma_fit_log() takes log data, so a
// reciprocal fit passes -log(x) without forming 1/x.

#pragma once

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>
#include <vector>

namespace cppunivariate {

enum class TrGammaStatus {
  Ok,
  LognormalBoundary,
  PowerBoundary,
  ScaleOutOfRange,
  InvalidData,
  InvalidFixed,
  IterationFailure
};

struct TrGammaResult {
  double shape1, shape2, scale;
  double log_scale;
  double logLik;
  int iterations;
  TrGammaStatus status;
};

namespace trgamma_detail {

constexpr double kHalfLog2Pi = 0.91893853320467274178;
constexpr double kEulerGamma = 0.57721566490153286061;
constexpr double kTiePerObs = 1e-8;
// Below t = kMinT the shape exceeds 1e200 and no fit is representable.
constexpr double kMinT = 1e-100;

// e^z - 1 - z without cancellation near zero.
inline double expm1_minus(double z) {
  if (std::abs(z) < 0.1) {
    double p = 1.0 / 479001600.0;  // 1/12!
    double factorial = 479001600.0;
    for (int k = 12; k > 2; --k) {
      factorial /= k;
      p = p * z + 1.0 / factorial;
    }
    return p * z * z;
  }
  return std::expm1(z) - z;
}

// log(a) - digamma(a) > 0, with the asymptotic series where the direct
// difference cancels.
inline double log_minus_digamma(double a) {
  if (a >= 100.0) {
    const double r = 1.0 / a, r2 = r * r;
    return 0.5 * r +
           r2 * (1.0 / 12.0 +
                 r2 * (-1.0 / 120.0 +
                       r2 * (1.0 / 252.0 + r2 * (-1.0 / 240.0 + r2 / 132.0))));
  }
  return std::log(a) - special::digamma(a);
}

// d/da (log(a) - digamma(a)) = 1/a - trigamma(a) < 0.
inline double dlog_minus_digamma(double a) {
  if (a >= 100.0) {
    const double r = 1.0 / a, r2 = r * r;
    return -r2 * (0.5 + r * (1.0 / 6.0 +
                             r2 * (-1.0 / 30.0 + r2 * (1.0 / 42.0 - r2 / 30.0))));
  }
  return 1.0 / a - special::trigamma(a);
}

// lgamma(a) - (a - 1/2) log(a) + a - log(2 pi)/2.
inline double stirling_remainder(double a) {
  if (a >= 10.0) {
    const double r = 1.0 / a, r2 = r * r;
    return r * (1.0 / 12.0 +
                r2 * (-1.0 / 360.0 +
                      r2 * (1.0 / 1260.0 +
                            r2 * (-1.0 / 1680.0 +
                                  r2 * (1.0 / 1188.0 + r2 * (-691.0 / 360360.0))))));
  }
  return std::lgamma(a) - (a - 0.5) * std::log(a) + a - kHalfLog2Pi;
}

// Gamma shape a > 0 solving log(a) - digamma(a) = s > 0: Newton in log(a)
// (the equation is decreasing and convex there).
inline double gamma_shape(double s, int &iterations) {
  if (!(s > 0.0) || !std::isfinite(s)) return std::numeric_limits<double>::quiet_NaN();
  double a = s < 3.0
                 ? (3.0 - s + std::sqrt((s - 3.0) * (s - 3.0) + 24.0 * s)) / (12.0 * s)
                 : 1.0 / (s + std::log(s) - kEulerGamma);
  double b = std::log(a);
  int it = 0;
  for (; it < 60; ++it) {
    a = std::exp(b);
    const double f = log_minus_digamma(a) - s;
    const double df = a * dlog_minus_digamma(a);
    if (f == 0.0 || !(df < 0.0)) break;
    double step = f / df;
    step = std::max(-5.0, std::min(5.0, step));
    b -= step;
    if (std::abs(step) <= 4e-16 * (1.0 + std::abs(b))) break;
  }
  iterations += it + 1;
  return std::exp(b);
}

// Standardized log data u = (y - mean) / sd, mean(u) = 0, mean(u^2) = 1.
struct LogData {
  std::vector<double> u;
  double mean = 0.0, sd = 0.0, sum = 0.0;
  double umax = 0.0, umin = 0.0, kappa3 = 0.0;
  std::size_t n = 0;
};

inline bool standardize(span y, LogData &d) {
  d.n = y.size();
  if (d.n < 2) return false;
  long double total = 0.0L;
  for (double v : y) {
    if (!std::isfinite(v)) return false;
    total += v;
  }
  const double dn = static_cast<double>(d.n);
  d.sum = static_cast<double>(total);
  d.mean = static_cast<double>(total / static_cast<long double>(d.n));
  d.u.resize(d.n);
  long double shift = 0.0L;
  for (std::size_t i = 0; i < d.n; ++i) {
    d.u[i] = y[i] - d.mean;
    shift += d.u[i];
  }
  const double correction = static_cast<double>(shift / static_cast<long double>(d.n));
  long double squares = 0.0L;
  for (double &v : d.u) {
    v -= correction;
    squares += static_cast<long double>(v) * v;
  }
  d.mean += correction;
  d.sd = std::sqrt(static_cast<double>(squares / static_cast<long double>(d.n)));
  if (!(d.sd > 0.0) || !std::isfinite(d.sd)) return false;
  long double cubes = 0.0L;
  d.umax = -std::numeric_limits<double>::infinity();
  d.umin = std::numeric_limits<double>::infinity();
  for (double &v : d.u) {
    v /= d.sd;
    cubes += static_cast<long double>(v) * v * v;
    d.umax = std::max(d.umax, v);
    d.umin = std::min(d.umin, v);
  }
  d.kappa3 = static_cast<double>(cubes) / dn;
  // A sorted scan can stop once all remaining tilted weights are below
  // rounding relevance; this matters in the power-face tail.
  std::sort(d.u.begin(), d.u.end(), [](double a, double b) { return a > b; });
  return d.umax > 0.0;
}

// Maximised log likelihoods of the two faces (suprema of the family).
inline double lognormal_loglik(const LogData &d) {
  const double dn = static_cast<double>(d.n);
  return -dn * (kHalfLog2Pi + std::log(d.sd) + 0.5) - d.sum;
}
inline double power_delta(const LogData &d) {
  return kHalfLog2Pi - 0.5 - std::log(d.umax);
}

// K(t) = log mean exp(t u) and its first two derivatives.
struct Cgf {
  double K, K1, K2;
};

inline Cgf cgf(const LogData &d, double t) {
  const double dn = static_cast<double>(d.n);
  Cgf c{0.0, 0.0, 0.0};
  if (t * d.umax <= 30.0) {
    // mean(u) = 0 exactly after centring: mean(exp(t u)) = 1 + mean(e2(t u)).
    double m2 = 0.0, s1 = 0.0, s2 = 0.0;
    for (double v : d.u) {
      const double z = t * v;
      const double e2 = z < -50.0 ? -1.0 - z : expm1_minus(z);  // one transcendental call per term
      const double e1 = e2 + z;          // expm1(z)
      m2 += e2;
      s1 += v * e1;
      s2 += v * v * (1.0 + e1);
    }
    m2 /= dn;
    s1 /= dn;
    s2 /= dn;
    c.K = std::log1p(m2);
    c.K1 = s1 / (1.0 + m2);
    c.K2 = s2 / (1.0 + m2) - c.K1 * c.K1;
  } else {
    double w0 = 0.0, w1 = 0.0, w2 = 0.0;
    for (double v : d.u) {
      const double dv = v - d.umax;
      const double z = t * dv;
      if (z < -50.0) break;
      const double w = std::exp(z);
      w0 += w;
      w1 += dv * w;
      w2 += dv * dv * w;
    }
    const double a1 = w1 / w0;
    c.K = t * d.umax + std::log(w0 / dn);
    c.K1 = d.umax + a1;
    c.K2 = w2 / w0 - a1 * a1;
  }
  c.K2 = std::max(c.K2, 0.0);
  return c;
}

inline double delta_at(double t, double a, double K) {
  return std::log(t) + 0.5 * std::log(a) - stirling_remainder(a) - a * K + 0.5;
}

struct ProfilePoint {
  double t, delta, slope, shape, K, K1;
};

inline ProfilePoint profile(const LogData &d, double t, int &evaluations) {
  const Cgf c = cgf(d, t);
  int inner_iterations = 0;
  const double a = gamma_shape(c.K, inner_iterations);
  ++evaluations;
  return {t, delta_at(t, a, c.K), 1.0 / t - a * c.K1, a, c.K, c.K1};
}

// Brent's zero finder on [lo, hi], f(lo) and f(hi) of opposite signs.
template <typename F>
inline double brent_zero(F f, double lo, double hi, double flo, double fhi,
                         double tol, int maxit, bool &converged) {
  double a = lo, b = hi, fa = flo, fb = fhi, c = a, fc = fa, d = b - a, e = d;
  converged = false;
  for (int it = 0; it < maxit; ++it) {
    if ((fb > 0.0) == (fc > 0.0)) {
      c = a;
      fc = fa;
      d = e = b - a;
    }
    if (std::abs(fc) < std::abs(fb)) {
      a = b; b = c; c = a;
      fa = fb; fb = fc; fc = fa;
    }
    const double tol1 = 2.0 * 2.2e-16 * std::abs(b) + 0.5 * tol;
    const double m = 0.5 * (c - b);
    if (std::abs(m) <= tol1 || fb == 0.0) {
      converged = true;
      return b;
    }
    if (std::abs(e) >= tol1 && std::abs(fa) > std::abs(fb)) {
      double p, q, r;
      const double s = fb / fa;
      if (a == c) {
        p = 2.0 * m * s;
        q = 1.0 - s;
      } else {
        q = fa / fc;
        r = fb / fc;
        p = s * (2.0 * m * q * (q - r) - (b - a) * (r - 1.0));
        q = (q - 1.0) * (r - 1.0) * (s - 1.0);
      }
      if (p > 0.0) q = -q; else p = -p;
      if (2.0 * p < std::min(3.0 * m * q - std::abs(tol1 * q), std::abs(e * q))) {
        e = d;
        d = p / q;
      } else {
        d = m;
        e = m;
      }
    } else {
      d = m;
      e = m;
    }
    a = b;
    fa = fb;
    b += std::abs(d) > tol1 ? d : (m > 0.0 ? tol1 : -tol1);
    fb = f(b);
  }
  return b;
}

inline TrGammaResult make_result(const LogData &d, double shape1, double t, double K,
                                 double delta, int iterations, TrGammaStatus status) {
  const double log_scale = d.mean + d.sd * (K - std::log(shape1)) / t;
  const double logLik = static_cast<double>(d.n) * delta + lognormal_loglik(d);
  return {shape1, t / d.sd, std::exp(log_scale), log_scale, logLik, iterations, status};
}

// Fixed shape1 = a: Delta_a(t) = log(t) + log(a)/2 - stirling(a) - a K(t) + 1/2
// is strictly concave in b = log(t): its derivative 1 - a t K'(t) decreases
// from 1 to -inf. Safeguarded Newton on that derivative inside a bracket.
inline TrGammaResult fit_fixed_shape1(const LogData &d, double a, double tol, int maxit) {
  int evaluations = 0;
  auto score = [&](double b, double &derivative) {
    const double t = std::exp(b);
    const Cgf c = cgf(d, t);
    ++evaluations;
    derivative = -a * t * (c.K1 + t * c.K2);
    return 1.0 - a * t * c.K1;
  };
  const double bmin = std::log(kMinT), bmax = 700.0;
  double b = std::max(bmin, std::min(bmax, -0.5 * std::log(a)));
  double lo = bmin, hi = bmax;
  double unused = 0.0;
  const bool rising = score(b, unused) > 0.0;
  if (rising) lo = b; else hi = b;
  while (lo < bmax && hi > bmin && (rising ? hi == bmax : lo == bmin)) {
    const double next = rising ? std::min(bmax, lo + 2.0) : std::max(bmin, hi - 2.0);
    if (score(next, unused) > 0.0) lo = next; else hi = next;
    if ((rising && next >= bmax) || (!rising && next <= bmin)) break;
  }
  b = 0.5 * (lo + hi);
  bool converged = false;
  for (int it = 0; it < maxit; ++it) {
    double df = 0.0;
    const double f = score(b, df);
    if (f == 0.0) { converged = true; break; }
    if (f > 0.0) lo = b; else hi = b;
    double next = df < 0.0 ? b - f / df : 0.5 * (lo + hi);
    if (!(next > lo && next < hi)) next = 0.5 * (lo + hi);
    const double step = next - b;
    b = next;
    if (std::abs(step) <= tol || f == 0.0 || hi - lo <= tol) {
      converged = true;
      break;
    }
  }
  const double t = std::exp(b);
  const Cgf c = cgf(d, t);
  return make_result(d, a, t, c.K, delta_at(t, a, c.K), evaluations,
                     converged ? TrGammaStatus::Ok : TrGammaStatus::IterationFailure);
}

inline TrGammaResult face_lognormal(const LogData &d, int iterations) {
  const double inf = std::numeric_limits<double>::infinity();
  return {inf, 0.0, 0.0, -inf, lognormal_loglik(d), iterations,
          TrGammaStatus::LognormalBoundary};
}

inline TrGammaResult face_power(const LogData &d, int iterations) {
  const double inf = std::numeric_limits<double>::infinity();
  const double log_scale = d.mean + d.sd * d.umax;
  return {0.0, inf, std::exp(log_scale), log_scale,
          static_cast<double>(d.n) * power_delta(d) + lognormal_loglik(d), iterations,
          TrGammaStatus::PowerBoundary};
}

// Free fit: one-dimensional search in t with an exact inner gamma fit.
inline TrGammaResult fit_free(const LogData &d, double tol, int maxit) {
  int evaluations = 0;
  const double dn = static_cast<double>(d.n);
  const double slope0 = -d.kappa3 / 6.0;  // dDelta/dt at t = 0+
  const double t_lo = 0.05 / std::max(d.umax, -d.umin);
  // Beyond t_hi, K(t) >= t*max(u) - log(n) bounds Delta below the power face.
  const double t_hi = 4.0 * (dn + std::log(dn) + 10.0) / d.umax;
  const int per_decade = 16;
  const int cells = std::max(8, static_cast<int>(std::ceil(per_decade * std::log10(t_hi / t_lo))));

  ProfilePoint best{0.0, -std::numeric_limits<double>::infinity(), 0.0, 0.0, 0.0, 0.0};
  bool failure = false;
  auto consider = [&](const ProfilePoint &p) {
    if (std::isfinite(p.delta) && p.delta > best.delta) best = p;
  };
  auto refine_log = [&](const ProfilePoint &l, const ProfilePoint &r) {
    auto f = [&](double b) {
      const double t = std::exp(b);
      return profile(d, t, evaluations).slope * t;
    };
    bool ok = false;
    const double b = brent_zero(f, std::log(l.t), std::log(r.t), l.slope * l.t,
                                r.slope * r.t, tol, maxit, ok);
    if (!ok) failure = true;
    consider(profile(d, std::exp(b), evaluations));
  };

  ProfilePoint prev = profile(d, t_lo, evaluations);
  consider(prev);
  if (slope0 > 0.0 && prev.slope <= 0.0) {
    // A local maximum below the grid, near the lognormal face. Below
    // t = 1e-12 it cannot beat that face by the tie margin.
    ProfilePoint right = prev, left = prev;
    while (left.t > 1e-12) {
      left = profile(d, left.t / 10.0, evaluations);
      if (left.slope > 0.0) break;
      right = left;
    }
    if (left.slope > 0.0) refine_log(left, right);
  }
  // H(K) = sup_a [a log(a)-a-lgamma(a)-a K] is convex. Its
  // secant over [K(l), K(r)] is an upper bound. Consequently the
  // profile on [l,r] is bounded by log(t)-a_sec K(t)+constant,
  // a strictly concave function of t. The intersection of its endpoint
  // tangents bounds its maximum, including peaks missed by the grid.
  auto certify = [&](const ProfilePoint &left, const ProfilePoint &right) {
    struct Cell { ProfilePoint l, r; int depth; };
    std::vector<Cell> pending{{left, right, 0}};
    while (!pending.empty()) {
      const Cell cell = pending.back();
      pending.pop_back();
      const auto &l = cell.l;
      const auto &r = cell.r;
      const double da = (l.delta - r.delta + std::log(r.t / l.t)) / (r.K - l.K);
      const double sl = 1.0 / l.t - da * l.K1;
      const double sr = 1.0 / r.t - da * r.K1;
      double upper = std::max(l.delta, r.delta);
      if (sl > 0.0 && sr < 0.0) {
        const double offset = (r.delta - l.delta - sr * (r.t - l.t)) / (sl - sr);
        upper = l.delta + sl * std::max(0.0, std::min(r.t - l.t, offset));
      }
      const double incumbent = std::max(best.delta, std::max(0.0, power_delta(d)));
      if (upper <= incumbent + kTiePerObs) continue;
      if (cell.depth >= 40) { failure = true; continue; }
      const auto mid = profile(d, std::sqrt(l.t * r.t), evaluations);
      consider(mid);
      if (l.slope > 0.0 && mid.slope <= 0.0) refine_log(l, mid);
      if (mid.slope > 0.0 && r.slope <= 0.0) refine_log(mid, r);
      pending.push_back({l, mid, cell.depth + 1});
      pending.push_back({mid, r, cell.depth + 1});
    }
  };
  for (int j = 1; j <= cells; ++j) {
    const double t = t_lo * std::pow(t_hi / t_lo, static_cast<double>(j) / cells);
    const ProfilePoint cur = profile(d, t, evaluations);
    consider(cur);
    if (prev.slope > 0.0 && cur.slope <= 0.0) refine_log(prev, cur);
    certify(prev, cur);
    prev = cur;
  }

  const double face = std::max(0.0, power_delta(d));
  if (!(best.delta > face + kTiePerObs)) {
    return power_delta(d) > 0.0 ? face_power(d, evaluations) : face_lognormal(d, evaluations);
  }
  return make_result(d, best.shape, best.t, best.K, best.delta, evaluations,
                     failure ? TrGammaStatus::IterationFailure : TrGammaStatus::Ok);
}

}  // namespace trgamma_detail

// Public face-only helper for other positive-power families (e.g. GB2).
// The endpoint and shape describe the power face without the finite-sample
// offset used by mlpower; invalid input returns valid=false.
struct TrGammaFaces {
  bool valid;
  double lognormal_logLik, power_logLik, meanlog, sdlog, power_shape, log_endpoint;
};

inline TrGammaFaces trgamma_faces_log(span y) {
  trgamma_detail::LogData d;
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (!trgamma_detail::standardize(y, d)) return {false, nan, nan, nan, nan, nan, nan};
  const double ln = trgamma_detail::lognormal_loglik(d);
  return {true, ln, ln + y.size() * trgamma_detail::power_delta(d), d.mean, d.sd,
          1.0 / (d.sd * d.umax), d.mean + d.sd * d.umax};
}

// Fit on log data y = log(x). NaN shape1/shape2 are estimated, positive
// finite values are held fixed. logLik is for x = exp(y).
inline TrGammaResult trgamma_fit_log(span y, double tol, int maxit,
                                     double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                     double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  TrGammaResult bad{shape1, shape2, nan, nan, nan, 0, TrGammaStatus::InvalidData};
  for (double s : {shape1, shape2}) {
    if (!std::isnan(s) && (!(s > 0.0) || !std::isfinite(s))) {
      bad.status = TrGammaStatus::InvalidFixed;
      return bad;
    }
  }
  if (!(tol > 0.0) || !std::isfinite(tol) || maxit < 1) return bad;
  trgamma_detail::LogData d;
  const bool standardized = trgamma_detail::standardize(y, d);
  // Both shapes held: only scale is fitted, even for a singleton or
  // constant sample. Centre before exponentiating to avoid x^shape2.
  if (!standardized && !std::isnan(shape1) && !std::isnan(shape2)) {
    if (y.size() == 0) return bad;
    double ymax = -std::numeric_limits<double>::infinity();
    long double sum_y = 0.0L;
    for (double v : y) {
      if (!std::isfinite(v)) return bad;
      ymax = std::max(ymax, v);
      sum_y += v;
    }
    long double total = 0.0L;
    for (double v : y) total += std::exp(shape2 * (v - ymax));
    const double log_scale = ymax +
        (std::log(static_cast<double>(total / y.size())) - std::log(shape1)) / shape2;
    const double K = shape2 * (ymax - static_cast<double>(sum_y / y.size())) +
        std::log(static_cast<double>(total / y.size()));
    const double ll = y.size() * (std::log(shape2) + 0.5 * std::log(shape1) -
        trgamma_detail::kHalfLog2Pi - trgamma_detail::stirling_remainder(shape1) - shape1 * K) -
        static_cast<double>(sum_y);
    return {shape1, shape2, std::exp(log_scale), log_scale, ll, 0, TrGammaStatus::Ok};
  }
  if (!standardized) return bad;
  if (!std::isnan(shape2)) {
    const double t = shape2 * d.sd;
    const trgamma_detail::Cgf c = trgamma_detail::cgf(d, t);
    int iterations = 0;
    const double a = std::isnan(shape1) ? trgamma_detail::gamma_shape(c.K, iterations) : shape1;
    return trgamma_detail::make_result(d, a, t, c.K, trgamma_detail::delta_at(t, a, c.K),
                                       iterations, TrGammaStatus::Ok);
  }
  if (!std::isnan(shape1)) return trgamma_detail::fit_fixed_shape1(d, shape1, tol, maxit);
  return trgamma_detail::fit_free(d, tol, maxit);
}

// Interior fits whose scale is not a normal positive double (close to the
// lognormal face) report ScaleOutOfRange, keeping shapes and logLik.
inline TrGammaStatus trgamma_range_status(TrGammaStatus status, double log_scale) {
  if (status != TrGammaStatus::Ok) return status;
  const double lo = std::log(std::numeric_limits<double>::min());
  const double hi = std::log(std::numeric_limits<double>::max());
  return (log_scale > lo && log_scale < hi) ? status : TrGammaStatus::ScaleOutOfRange;
}

inline TrGammaResult trgamma_mle(span x, double tol = 1e-10, int maxit = 100,
                                 double shape1 = std::numeric_limits<double>::quiet_NaN(),
                                 double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  std::vector<double> y(x.size());
  for (std::size_t i = 0; i < x.size(); ++i) {
    if (!(x[i] > 0.0) || !std::isfinite(x[i])) {
      const double nan = std::numeric_limits<double>::quiet_NaN();
      return {shape1, shape2, nan, nan, nan, 0, TrGammaStatus::InvalidData};
    }
    y[i] = std::log(x[i]);
  }
  TrGammaResult r = trgamma_fit_log(span{y.data(), y.size()}, tol, maxit, shape1, shape2);
  r.status = trgamma_range_status(r.status, r.log_scale);
  return r;
}

}  // namespace cppunivariate
