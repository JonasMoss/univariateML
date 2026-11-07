// Truncated normal with known bounds (a, b): Newton in natural parameters.
//
// On (a, b) the density is exp(t1*x + t2*x^2 - A(t1, t2)), t1 = mean/sd^2,
// t2 = -1/(2*sd^2) < 0, a regular exponential family in the closure; the
// log-likelihood n*(t1*m1 + t2*m2 - A) is strictly concave. Its supremum over
// t2 < 0 is attained iff the profile derivative at t2 = 0, n*(v - v_face),
// is negative, where v is the population variance and v_face the variance of
// the t2 = 0 member with the sample mean: the exponential on (a, inf) or
// (-inf, b), or the truncated exponential on (a, b). Otherwise the
// likelihood increases towards that face as sd -> inf.
//
// The fit depends on the data through mean and variance only. It works in
// standardized coordinates u = (x - c)/s, where every evaluation of A and
// its derivatives is one Gauss-Legendre quadrature over the region in which
// the exponent is within kCut of its maximum. This is accurate in both tails
// and near t2 = 0, where differences of normal CDFs cancel.
#pragma once
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../cmb/acceptance.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {

enum class TnormStatus {
  Ok,
  InvalidInput,      // empty or non-finite data, or bad tol / maxit
  InvalidBounds,     // NaN bound, a >= b, a = inf or b = -inf
  OutsideBounds,     // an observation is outside [a, b]
  InvalidFixed,      // held mean not finite, or held sd not finite and positive
  DegenerateData,    // scale collapse at the projected mean: unbounded likelihood
  ExponentialBoundary,           // b = inf: a + exponential face
  ReflectedExponentialBoundary,  // a = -inf: b - exponential face
  TruncatedExponentialBoundary,  // finite a, b: exp(rate*x) on (a, b) face
  UniformBoundary,               // mean held, finite a, b: uniform face
  IterationFailure,
  UnboundedLocation // held sd, constant data at a finite endpoint
};

struct TnormResult {
  double mean, sd, a, b, logLik;
  // Escapes: the limit law has density proportional to exp(limit_rate * x)
  // on the support (rate 0: uniform); limit_logLik is its log-likelihood.
  double limit_rate, limit_logLik;
  int iterations;
  TnormStatus status;
 StopDetail stop_detail{}; };

namespace tnorm_detail {

constexpr int kNodes = 64;
// Integrate where the exponent is within kCut of its maximum; the neglected
// mass of a log-concave density is below exp(-kCut) relative.
constexpr double kCut = 50.0;

// 64-point Gauss-Legendre rule on [-1, 1], symmetric pairs x = +-(1 - 2h):
// kHalfGap[i] = h = (1 - |x_i|)/2, the distance of a node from its nearer
// end of [0, 1], and kWeight[i] the weight on [-1, 1]. Computed with
// 50-digit arithmetic and rounded; storing h keeps nodes next to the ends
// exact, where exponential faces put their mass.
inline constexpr double kHalfGap[kNodes / 2] = {
    0.0003474791321139303, 0.0018299416140223604, 0.00449331426162784,
    0.008331873057687022, 0.013336586105044517, 0.01949560017397314,
    0.026794312570798593, 0.035215413934030215, 0.044738931460748595,
    0.055342277002442944, 0.06700030092295359, 0.07968535187370981,
    0.09336734243860122, 0.1080138205283293, 0.12359004636973406,
    0.1400590749141946, 0.15738184347288336, 0.17551726437267132,
    0.19442232241380336, 0.21405217689868297, 0.23436026799005272,
    0.2552984271464735, 0.27681699137326793, 0.2988649210180042,
    0.32138992083116596, 0.34433856400489454, 0.3676564188956163,
    0.39128817812999644, 0.41517778978800357, 0.4392685903519397,
    0.4635034391061005, 0.48782485366828776,
};
inline constexpr double kWeight[kNodes / 2] = {
    0.001783280721696433, 0.004147033260562468, 0.006504457968978363,
    0.008846759826363947, 0.011168139460131128, 0.013463047896718643,
    0.015726030476024718, 0.017951715775697343, 0.02013482315353021,
    0.022270173808383253, 0.024352702568710874, 0.02637746971505466,
    0.028339672614259483, 0.030234657072402478, 0.03205792835485155,
    0.033805161837141606, 0.035472213256882386, 0.03705512854024005,
    0.038550153178615626, 0.03995374113272034, 0.04126256324262353,
    0.04247351512365359, 0.04358372452932345, 0.044590558163756566,
    0.04549162792741814, 0.046284796581314416, 0.04696818281621002,
    0.04754016571483031, 0.04799938859645831, 0.048344762234802954,
    0.04857546744150343, 0.048690957009139724,
};

// Mass and central moments of exp(t1*u + t2*u^2) on (lo, hi), t2 <= 0.
struct Mass {
  double log_mass, mean, var, m3, m4;
  bool ok;
};

inline Mass mass(double t1, double t2, double lo, double hi, bool moments = true) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  Mass out{nan, nan, nan, nan, nan, false};
  if (!(t2 <= 0) || !std::isfinite(t1) || !std::isfinite(t2) || !(lo < hi)) return out;
  // Mode us on [lo, hi], slope g there; exponent minus its maximum at
  // u = us + d is d*(g + t2*d).
  double us;
  if (t2 < 0) {
    us = std::min(std::max(-t1 / (2 * t2), lo), hi);
  } else if (t1 > 0) {
    if (!std::isfinite(hi)) return out;
    us = hi;
  } else if (t1 < 0) {
    if (!std::isfinite(lo)) return out;
    us = lo;
  } else {
    if (!std::isfinite(lo) || !std::isfinite(hi)) return out;
    us = lo;
  }
  const double g = t1 + 2 * t2 * us;
  double dlo, dhi;
  if (t2 < 0) {
    // Roots of t2*d^2 + g*d + kCut = 0, one on each side of zero.
    const double D = std::sqrt(g * g - 4 * t2 * kCut);
    if (!std::isfinite(D)) return out;
    if (g >= 0) {
      dhi = (-g - D) / (2 * t2);
      dlo = kCut / (t2 * dhi);
    } else {
      dlo = (-g + D) / (2 * t2);
      dhi = kCut / (t2 * dlo);
    }
  } else if (t1 > 0) {
    dlo = -kCut / t1;
    dhi = 0;
  } else if (t1 < 0) {
    dlo = 0;
    dhi = kCut / -t1;
  } else {
    dlo = 0;
    dhi = hi - lo;
  }
  const double slo = std::max(lo, us + dlo), shi = std::min(hi, us + dhi);
  const double width = shi - slo;
  if (!(width > 0) || !std::isfinite(width)) return out;
  double d[kNodes], e[kNodes];
  double s0 = 0, s1 = 0;
  for (int i = 0; i < kNodes / 2; ++i) {
    for (int side = 0; side < 2; ++side) {
      const int j = 2 * i + side;
      const double u = side == 0 ? slo + width * kHalfGap[i] : shi - width * kHalfGap[i];
      d[j] = u - us;
      e[j] = kWeight[i] * std::exp(d[j] * (g + t2 * d[j]));
      s0 += e[j];
      s1 += e[j] * d[j];
    }
  }
  out.log_mass = us * (t1 + t2 * us) + std::log(s0 * width / 2);
  // Analytic normal CDF differences where completing the square does not
  // cause cancellation. Survival probabilities handle the positive tail;
  // shifted quadrature above covers underflow, narrow intervals, and the
  // near-exponential face where the completed-square terms diverge.
  if (t2 < 0) {
    const double sigma = 1 / std::sqrt(-2 * t2);
    const double centre = -t1 / (2 * t2);
    const double alpha = (lo - centre) / sigma, beta = (hi - centre) / sigma;
    if (std::abs(centre / sigma) < 8 && beta - alpha > 1e-3) {
      const bool upper_tail = alpha >= 0;
      const double big = upper_tail ? special::pnorm(alpha, 0, 1, false, true)
                                    : special::pnorm(beta, 0, 1, true, true);
      const double small = upper_tail ? special::pnorm(beta, 0, 1, false, true)
                                      : special::pnorm(alpha, 0, 1, true, true);
      if (std::isfinite(big) && small - big < -1e-4)
        out.log_mass = -t1 * t1 / (4 * t2) + std::log(sigma) +
                       0.91893853320467274178 + big + std::log(-std::expm1(small - big));
    }
  }
  out.ok = std::isfinite(out.log_mass);
  if (!moments) return out;
  const double shift = s1 / s0;
  double c2 = 0, c3 = 0, c4 = 0;
  for (int j = 0; j < kNodes; ++j) {
    const double z = d[j] - shift, z2 = z * z;
    c2 += e[j] * z2;
    c3 += e[j] * z2 * z;
    c4 += e[j] * z2 * z2;
  }
  out.mean = us + shift;
  out.var = c2 / s0;
  out.m3 = c3 / s0;
  out.m4 = c4 / s0;
  out.ok = out.ok && std::isfinite(out.mean) && out.var > 0;
  return out;
}

// Maximize f(t2) = t2*m2 - A(0, t2) over t2 < 0 (held mean at the origin).
// Returns false on iteration failure.
inline bool newton_scale(double lo, double hi, double m2, double tol, int maxit,
                         double &t2, int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  Mass m = mass(0, t2, lo, hi);
  if (!m.ok) return false;
  double f = t2 * m2 - m.log_mass;
  for (iterations = 1; iterations <= maxit; ++iterations) {
    const double eu2 = m.var + m.mean * m.mean;
    const double g = m2 - eu2;
    const double h = m.m4 - m.var * m.var + 4 * m.mean * m.m3 + 4 * m.mean * m.mean * m.var;
    const double step = g / h;
    if (!std::isfinite(step)) return false;
    double alpha = 1;
    bool accepted = false;
    for (int k = 0; k < maxit; ++k, alpha /= 2) {
      const double next = t2 + alpha * step;
      if (!(next < 0)) continue;
      const Mass mn = mass(0, next, lo, hi);
      if (!mn.ok) continue;
      const double fn = next * m2 - mn.log_mass;
      if (fn >= f + 1e-4 * alpha * g * step ||
          (fn >= f-64*std::numeric_limits<double>::epsilon()*(1+std::abs(f)) &&
           std::abs(m2-(mn.var+mn.mean*mn.mean)) < std::abs(g))) {
        t2 = next;
        m = mn;
        f = fn;
        accepted = true;
        break;
      }
    }
    const bool small = std::abs(alpha * step) <= tol * (1 + std::abs(t2));
    if (accepted && small && std::abs(g) < 1e-7) return true;
    // Line search exhausted at machine precision: the decrement is zero.
    if (!accepted) return std::abs(g) < 1e-7;
  }
  return false;
}

// Location at fixed t2 with mean(u) = 0: minimize A(t1, t2) over t1.
inline bool newton_location(double lo, double hi, double t2, double tol, int maxit,
                            double &t1, int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  Mass m = mass(t1, t2, lo, hi);
  if (!m.ok) return false;
  double f = -m.log_mass;
  for (iterations = 1; iterations <= maxit; ++iterations) {
    const double g = -m.mean;
    const double step = g / m.var;
    if (!std::isfinite(step)) return false;
    double alpha = 1;
    bool accepted = false;
    for (int k = 0; k < maxit; ++k, alpha /= 2) {
      const double next = t1 + alpha * step;
      const Mass mn = mass(next, t2, lo, hi);
      if (!mn.ok) continue;
      const double fn = -mn.log_mass;
      if (fn >= f + 1e-4 * alpha * g * step ||
          (fn >= f-64*std::numeric_limits<double>::epsilon()*(1+std::abs(f)) &&
           std::abs(mn.mean) < std::abs(g))) {
        t1 = next;
        m = mn;
        f = fn;
        accepted = true;
        break;
      }
    }
    const bool small = std::abs(alpha * step) <= tol * (1 + std::abs(t1));
    if (accepted && small && std::abs(g) < 1e-7) return true;
    if (!accepted) return std::abs(g) < 1e-7;
  }
  return false;
}

// Maximize t1*0 + t2*1 - A(t1, t2) over t2 < 0 (standardized data).
inline bool newton_joint(double lo, double hi, double tol, int maxit,
                         double &t1, double &t2, int &iterations) {
  maxit = static_cast<int>(solver_budget(maxit));
  Mass m = mass(t1, t2, lo, hi);
  if (!m.ok) return false;
  double f = t2 - m.log_mass;
  for (iterations = 1; iterations <= maxit; ++iterations) {
    const double mu = m.mean;
    const double g1 = -mu, g2 = 1 - (m.var + mu * mu);
    // Covariance of (U, U^2) from central moments.
    const double c11 = m.var;
    const double c12 = m.m3 + 2 * mu * m.var;
    const double c22 = m.m4 - m.var * m.var + 4 * mu * m.m3 + 4 * mu * mu * m.var;
    const double det = c11 * c22 - c12 * c12;
    double s1, s2;
    if (det > 0) {
      s1 = (c22 * g1 - c12 * g2) / det;
      s2 = (c11 * g2 - c12 * g1) / det;
    } else {
      s1 = g1 / c11;
      s2 = g2 / c22;
    }
    if (!std::isfinite(s1) || !std::isfinite(s2)) return false;
    const double slope = g1 * s1 + g2 * s2;
    double alpha = 1;
    bool accepted = false;
    for (int k = 0; k < maxit; ++k, alpha /= 2) {
      const double n1 = t1 + alpha * s1, n2 = t2 + alpha * s2;
      if (!(n2 < 0)) continue;
      const Mass mn = mass(n1, n2, lo, hi);
      if (!mn.ok) continue;
      const double fn = n2 - mn.log_mass;
      if (fn >= f + 1e-4 * alpha * slope ||
          (fn >= f-64*std::numeric_limits<double>::epsilon()*(1+std::abs(f)) &&
           std::max(std::abs(mn.mean),std::abs(1-mn.var-mn.mean*mn.mean)) <
             std::max(std::abs(g1),std::abs(g2)))) {
        t1 = n1;
        t2 = n2;
        m = mn;
        f = fn;
        accepted = true;
        break;
      }
    }
    const double size = std::max(std::abs(t1), std::abs(t2));
    const double moved = alpha * std::max(std::abs(s1), std::abs(s2));
    if (accepted && moved <= tol * (1 + size) &&
        std::max(std::abs(g1), std::abs(g2)) < 1e-7) return true;
    if (!accepted) return std::max(std::abs(g1), std::abs(g2)) < 1e-7;
  }
  return false;
}

// Tilt t with mean zero of exp(t*u) on finite (lo, hi), lo < 0 < hi.
inline bool tilt_with_zero_mean(double lo, double hi, double &t, Mass &m,int maxit=200) {
  maxit = static_cast<int>(solver_budget(maxit));
  double tl = -std::numeric_limits<double>::infinity();
  double th = std::numeric_limits<double>::infinity();
  t = 0;
  for (int it = 0; it < maxit; ++it) {
    m = mass(t, 0, lo, hi);
    if (!m.ok) return false;
    if (m.mean == 0) return true;
    if (m.mean > 0) th = t; else tl = t;
    double next = t - m.mean / m.var;
    if (!(next > tl && next < th)) {
      if (std::isfinite(tl) && std::isfinite(th)) next = tl + (th - tl) / 2;
      else if (std::isfinite(tl)) next = tl + std::max(1.0, std::abs(tl));
      else next = th - std::max(1.0, std::abs(th));
    }
    if (std::abs(next - t) <= 1e-15 * (1 + std::abs(t))) {
      t = next;
      m = mass(t, 0, lo, hi);
      return m.ok;
    }
    t = next;
  }
  return false;
}

}  // namespace tnorm_detail

// Truncated normal MLE with known bounds a < b (either may be infinite).
// Observations may equal either finite endpoint. NaN bounds estimate the
// corresponding sample extreme. Pass a finite fixed_mean or
// fixed_sd to hold that parameter; NaN estimates it. tol is the relative
// Newton step tolerance in natural parameters.
inline TnormResult tnorm_mle_impl(span x,
                             double a = -std::numeric_limits<double>::infinity(),
                             double b = std::numeric_limits<double>::infinity(),
                             double tol = 1e-12,
                             int maxit = 100,
                             double fixed_mean = std::numeric_limits<double>::quiet_NaN(),
                             double fixed_sd = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double nan = std::numeric_limits<double>::quiet_NaN();
  const double inf = std::numeric_limits<double>::infinity();
  const double log_root_2pi = 0.91893853320467274178;
  TnormResult res{nan, nan, a, b, nan, nan, nan, 0, TnormStatus::InvalidInput};
  if (x.size() == 0 || !(tol > 0) || !std::isfinite(tol) || maxit < 1) return res;
  if (std::isnan(a)) {
    a = x[0];
    for (double v : x) a = std::min(a, v);
  }
  if (std::isnan(b)) {
    b = x[0];
    for (double v : x) b = std::max(b, v);
  }
  res.a = a;
  res.b = b;
  if (!(a < b) || a == inf || b == -inf) {
    res.status = TnormStatus::InvalidBounds;
    return res;
  }
  const bool hold_mean = !std::isnan(fixed_mean), hold_sd = !std::isnan(fixed_sd);
  if ((hold_mean && !std::isfinite(fixed_mean)) ||
      (hold_sd && !(std::isfinite(fixed_sd) && fixed_sd > 0))) {
    res.status = TnormStatus::InvalidFixed;
    return res;
  }
  const double n = static_cast<double>(x.size());
  double sum = 0;
  bool constant = true;
  for (double v : x) {
    if (!std::isfinite(v)) return res;
    if (!(v >= a && v <= b)) {
      res.status = TnormStatus::OutsideBounds;
      return res;
    }
    constant = constant && v == x[0];
    sum += v - x[0];
  }
  const double xbar = x[0] + sum / n;
  double ss = 0;
  for (double v : x) ss += (v - xbar) * (v - xbar);
  const double var = ss / n;
  const bool lower = std::isfinite(a), upper = std::isfinite(b);

  if (hold_mean && hold_sd) {
    const double sd = fixed_sd, mu = fixed_mean;
    const double m2 = (var + (xbar - mu) * (xbar - mu)) / (sd * sd);
    const auto m = tnorm_detail::mass(0, -0.5, (a - mu) / sd, (b - mu) / sd, false);
    if (!m.ok) { res.status = TnormStatus::IterationFailure; return res; }
    res.mean = mu;
    res.sd = sd;
    res.logLik = n * (-0.5 * m2 - m.log_mass) - n * std::log(sd);
    res.status = TnormStatus::Ok;
    return res;
  }

  // Exact data tests precede every search. For scale collapse the common
  // observation must minimize squared distance to the held parent mean.
  if (constant && !hold_sd &&
      (!hold_mean || x[0] == std::min(std::max(fixed_mean, a), b))) {
    res.status = TnormStatus::DegenerateData;
    return res;
  }
  if (constant && hold_sd && (x[0] == a || x[0] == b)) {
    res.status = TnormStatus::UnboundedLocation;
    return res;
  }

  if (hold_sd) {
    // Location only: a regular one-parameter family in mean/sd^2 whose
    // sufficient statistic mean(x) lies inside (a, b) after the endpoint test.
    const double sd = fixed_sd;
    res.sd = sd;
    const double m2 = var / (sd * sd);
    double t1 = 0;
    if (lower || upper) {
      if (!tnorm_detail::newton_location((a - xbar) / sd, (b - xbar) / sd, -0.5, tol, maxit,
                                         t1, res.iterations)) {
        res.status = TnormStatus::IterationFailure;
        return res;
      }
    }
    const auto m = tnorm_detail::mass(t1, -0.5, (a - xbar) / sd, (b - xbar) / sd, false);
    res.mean = xbar + sd * t1;
    res.logLik = n * (-0.5 * m2 - m.log_mass) - n * std::log(sd);
    res.status = m.ok ? TnormStatus::Ok : TnormStatus::IterationFailure;
    return res;
  }

  if (hold_mean) {
    // Scale only, standardized at the held mean: u = (x - mean)/s with
    // mean(u^2) = 1. Natural parameter t2 = -1/(2 sd_u^2) for u^2.
    const double mu = fixed_mean;
    res.mean = mu;
    const double s = std::sqrt(var + (xbar - mu) * (xbar - mu));
    if (!(s > 0)) {
      res.status = TnormStatus::DegenerateData;
      return res;
    }
    const double lo = (a - mu) / s, hi = (b - mu) / s;
    if (lower && upper) {
      // Uniform face at t2 = 0; E u^2 under the uniform is (lo^2+lo*hi+hi^2)/3.
      res.limit_rate = 0;
      res.limit_logLik = -n * std::log(b - a);
      if ((lo * lo + lo * hi + hi * hi) / 3 <= 1) {
        res.status = TnormStatus::UniformBoundary;
        return res;
      }
    }
    double t2 = -0.5;
    if (lower || upper) {
      if (!tnorm_detail::newton_scale(lo, hi, 1, tol, maxit, t2, res.iterations)) {
        res.status = TnormStatus::IterationFailure;
        return res;
      }
    }
    const auto m = tnorm_detail::mass(0, t2, lo, hi, false);
    res.sd = s / std::sqrt(-2 * t2);
    res.logLik = n * (t2 - m.log_mass) - n * std::log(s);
    res.status = m.ok ? TnormStatus::Ok : TnormStatus::IterationFailure;
    if (lower && upper && res.logLik - res.limit_logLik <= 1e-8 * n)
      res.status = TnormStatus::UniformBoundary;
    return res;
  }

  // Both free.
  if (!(ss > 0)) {
    res.status = TnormStatus::DegenerateData;
    return res;
  }
  const double s = std::sqrt(var);
  if (!lower && !upper) {
    res.mean = xbar;
    res.sd = s;
    res.logLik = -n * (std::log(s) + log_root_2pi + 0.5);
    res.status = TnormStatus::Ok;
    return res;
  }
  const double lo = (a - xbar) / s, hi = (b - xbar) / s;
  // Face at t2 = 0 matched to the mean (zero in u); interior iff its
  // second moment exceeds the sample's (one).
  TnormStatus face;
  double face_second;
  if (!upper) {
    // a + exponential with mean -lo: rate 1/(xbar - a).
    face = TnormStatus::ExponentialBoundary;
    face_second = lo * lo;
    res.limit_rate = -1 / (xbar - a);
    res.limit_logLik = n * (std::log(-res.limit_rate) - 1);
  } else if (!lower) {
    face = TnormStatus::ReflectedExponentialBoundary;
    face_second = hi * hi;
    res.limit_rate = 1 / (b - xbar);
    res.limit_logLik = n * (std::log(res.limit_rate) - 1);
  } else {
    face = TnormStatus::TruncatedExponentialBoundary;
    double t;
    tnorm_detail::Mass m;
    if (!tnorm_detail::tilt_with_zero_mean(lo, hi, t, m,maxit)) {
      res.status = TnormStatus::IterationFailure;
      return res;
    }
    face_second = m.var + m.mean * m.mean;
    res.limit_rate = t / s;
    res.limit_logLik = -n * m.log_mass - n * std::log(s);
  }
  if (face_second <= 1) {
    res.status = face;
    return res;
  }
  double t1 = 0, t2 = -0.5;
  if (!tnorm_detail::newton_joint(lo, hi, tol, maxit, t1, t2, res.iterations)) {
    res.status = TnormStatus::IterationFailure;
    return res;
  }
  const auto m = tnorm_detail::mass(t1, t2, lo, hi, false);
  const double sd_u = 1 / std::sqrt(-2 * t2);
  res.mean = xbar + s * (t1 * sd_u * sd_u);
  res.sd = s * sd_u;
  res.logLik = n * (t2 - m.log_mass) - n * std::log(s);
  res.status = m.ok ? TnormStatus::Ok : TnormStatus::IterationFailure;
  // Faces win ties within 1e-8 n.
  if (res.status == TnormStatus::Ok && res.logLik - res.limit_logLik <= 1e-8 * n)
    res.status = face;
  return res;
}

}  // namespace cppunivariate

namespace cppunivariate {
inline TnormResult tnorm_mle(span x,double a=-INFINITY,double b=INFINITY,double tol=1e-12,int maxit=100,double mean=NAN,double sd=NAN) {
  maxit = static_cast<int>(solver_budget(maxit));
  auto r=tnorm_mle_impl(x,a,b,tol,maxit,mean,sd);
  if(r.status==TnormStatus::DegenerateData && !x.empty()) {
    bool collapse=true;for(double value:x)collapse=collapse && value==x[0];
    if(!std::isnan(mean))collapse=collapse && x[0]==std::min(std::max(mean,r.a),r.b);
    if(!collapse)r.status=TnormStatus::IterationFailure;
  }
  if(r.status==TnormStatus::Ok && !(std::isfinite(r.mean) && std::isfinite(r.sd) &&
     r.sd>0 && std::isfinite(r.logLik))) {
    r.status=TnormStatus::IterationFailure;r.stop_detail.loglik_upper=INFINITY;return r;
  }
  if(r.status==TnormStatus::Ok && (std::isnan(mean) || std::isnan(sd)) &&
     (std::isfinite(r.a) || std::isfinite(r.b))) {
    const bool fm=!std::isnan(mean),fs=!std::isnan(sd);
    const double n=double(x.size());
    double shifted_sum=0;for(double k:x)shifted_sum+=k-x[0];
    const double origin=fm?mean:x[0]+shifted_sum/n;
    double square=0;for(double k:x)square+=(k-origin)*(k-origin)/n;
    const double scale=square>0?std::sqrt(square):r.sd;
    double u1=0,u2=0;for(double k:x){const double u=(k-origin)/scale;u1+=u/n;u2+=u*u/n;}
    const double lo=(r.a-origin)/scale,hi=(r.b-origin)/scale;
    const double theta2=-.5*(scale/r.sd)*(scale/r.sd);
    const double theta1=(r.mean-origin)/scale*(-2*theta2);
    const auto q=tnorm_detail::mass(theta1,theta2,lo,hi);
    const double cov=q.m3+2*q.mean*q.var;
    const double var2=q.m4-q.var*q.var+4*q.mean*q.m3+4*q.mean*q.mean*q.var;
    double a=0,b=0,c=0;
    if(fs)a=1/q.var;else if(fm)a=1/var2;
    else {const double det=q.var*var2-cov*cov;a=var2/det;b=-cov/det;c=q.var/det;}
    const auto evaluate=[&](double v,double w) {
      const double t1=fm?0:v,t2=fm?v:(fs?theta2:w);
      const auto z=tnorm_detail::mass(t1,t2,lo,hi);
      const double ll=t1*u1+t2*u2-z.log_mass-std::log(scale);
      return concave_accept_detail::Evaluation{ll,fm?u2-z.var-z.mean*z.mean:u1-z.mean,
        (!fm && !fs)?u2-z.var-z.mean*z.mean:0,z.ok};
    };
    const auto accurate=[&](const concave_accept_detail::Bound& z) {
      for(double v:{z.lo1,z.hi1})for(double w:{z.lo2,z.hi2}) {
        const double t1=fm?0:v,t2=fm?v:(fs?theta2:w);
        if(!(t2<0))return false;
        const double mu=origin-scale*t1/(2*t2),sigma=scale/std::sqrt(-2*t2);
        if((!fm && std::abs(mu-r.mean)>1e-8*(scale+std::abs(r.mean))) ||
           (!fs && std::abs(sigma-r.sd)>1e-8*r.sd))return false;
      }
      return true;
    };
    const auto bound=concave_accept_detail::certify(fm?theta2:theta1,theta2,r.logLik/n,
      a,b,c,(!fm && !fs)?2:1,false,maxit,evaluate,accurate);
    r.stop_detail.loglik_lower=r.logLik-1e-9*(n+std::abs(r.logLik));
    r.stop_detail.loglik_upper=n*bound.upper;
    if(!bound.ok) {
      r.status=TnormStatus::IterationFailure;
      r.stop_detail.candidates.push_back({{r.mean,r.sd,r.a,r.b},r.stop_detail.loglik_lower,
        r.logLik+1e-9*(n+std::abs(r.logLik))});
    }
  }
  if(r.status==TnormStatus::UniformBoundary)r.stop_detail=limit_detail({r.a,r.b},r.limit_logLik,StopModel::Unif);
  if(r.status==TnormStatus::ExponentialBoundary)r.stop_detail=limit_detail({r.a,-r.limit_rate},r.limit_logLik,StopModel::TranslatedExp);
  if(r.status==TnormStatus::ReflectedExponentialBoundary)r.stop_detail=limit_detail({r.b,r.limit_rate},r.limit_logLik,StopModel::ReflectedExp);
  if(r.status==TnormStatus::TruncatedExponentialBoundary)r.stop_detail=limit_detail({r.a,r.b,r.limit_rate},r.limit_logLik,StopModel::TruncatedExp);
  // A solved interior point compared closely with its face is a numerical
  // tie, not a mathematical assertion of nonexistence.
  if(r.status!=TnormStatus::Ok && std::isfinite(r.mean) && r.sd>0 && std::isfinite(r.logLik) &&
     std::isfinite(r.limit_logLik) && !r.stop_detail.limit_coef.empty()) {
    r.stop_detail.comparison_tie=true;
    const double margin=1e-9*(double(x.size())+std::abs(r.logLik));
    r.stop_detail.candidates.push_back({{r.mean,r.sd,r.a,r.b},r.logLik-margin,r.logLik+margin});
    r.stop_detail.loglik_lower=std::max(r.logLik-margin,r.limit_logLik-margin);
    r.stop_detail.loglik_upper=INFINITY;
  }
  return r;
}
}
