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
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../cmb/acceptance.hpp"
#include "tnorm_near_face.hpp"

#include "../../core/span.hpp"
#include "../norm/mle.hpp"
#include "../../special/special.hpp"
#include <algorithm>
#include <cmath>
#include <limits>

namespace cppunivariate {
namespace distributions { inline double dtnorm(double x,double mean,double sd,double a,double b,bool log); }

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
 StopDetail stop_detail{};
 // Distinguish coincident estimated bounds with proved collapse from bad design bounds.
 bool estimated_bounds_collapse=false;
 bool certified=true;
};

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

// Mean displacement from 1/2 under exp(t*u) on [0, 1]. Pairing
// the quadrature nodes makes the displacement proportional to t near zero.
inline double unit_tilt_shift(double t) {
  double mass = 0, moment = 0;
  for (int i = 0; i < kNodes / 2; ++i) {
    const double d = 0.5 - kHalfGap[i], z = t * d;
    mass += kWeight[i] * std::cosh(z);
    moment += kWeight[i] * d * std::sinh(z);
  }
  return moment / mass;
}

inline double endpoint_mean(double t) {
  const double tail = std::exp(-t);
  return 1 / t - tail / (-std::expm1(-t));
}

constexpr double face_moment_margin = 128 * std::numeric_limits<double>::epsilon();

// Compare the varying part of the squared distance on a finite window.
// For u=(x-a)/width, the constant (a-mean)^2 cancels, and we can divide
// the remaining empirical-minus-uniform difference by width*scale.
inline bool held_mean_uniform_wins(span x, double a, double width, double mean,
                                   double margin) {
  const double offset = a - mean;
  const double scale = std::max(std::abs(offset), width);
  if (!std::isfinite(scale)) return false;
  const double linear = 2 * (offset / scale), quadratic = width / scale;
  double first = 0, second = 0;
  for (double v : x) {
    const double u = (v - a) / width;
    first += u;
    second += u * u;
  }
  const double n = static_cast<double>(x.size());
  const double gap = linear * (first / n - 0.5) + quadratic * (second / n - 1. / 3);
  return gap > margin * (std::abs(linear) + quadratic);
}

struct ExponentialFace {
  double rate, loglik;
  bool accurate;
  double variance_ratio_upper = INFINITY;
  double variance_ratio_lower = 0;
};

// Detect an exact zero score when rounding margins obscure it. The
// two-word residual sum is accepted only if every low-word addition is exact.
inline bool exact_uniform_score(span x, double a, double b) {
  const double width = b - a, n = double(x.size());
  const double total = width * n;
  if (!std::isfinite(total) || norm_detail::residual_error(b, a, width) != 0 ||
      std::fma(width, n, -total) != 0) return false;
  double hi = -total, lo = 0;
  for (double v : x) {
    const double d = v - a, doubled = 2 * d;
    const double next = hi + doubled;
    if (!std::isfinite(next)) return false;
    for (double error : {2 * norm_detail::residual_error(v, a, d),
                         norm_detail::residual_error(hi, -doubled, next)}) {
      const double low = lo + error;
      if (!std::isfinite(low) || norm_detail::residual_error(lo, -error, low) != 0) return false;
      lo = low;
    }
    hi = next;
  }
  return hi == -lo;
}

inline ExponentialFace finite_exponential_face(span x, double a, double b,
                                               double xbar, double delta,
                                               double sd, double tol, int maxit) {
  const double factor = std::isfinite(b - a) ? 1 : 0.5;
  const double lower = a * factor, upper = b * factor;
  xbar *= factor;
  delta *= factor;
  const double w = upper - lower;
  const double mid = lower + w / 2;
  const double da = lower - mid, db = upper - mid;
  const double mid_low = norm_detail::residual_error(lower, mid, da) +
                         norm_detail::residual_error(upper, mid, db);
  const double mid_residual = (da + db) + mid_low;
  const double mid_error = mid_residual / 2;
  const double shift = ((xbar - mid) + (delta - mid_error)) / w;
  const double direction = shift > 0 ? 1 : -1;
  const double endpoint = shift > 0 ? upper : lower;
  double distance = 0, compensation = 0;
  for (double v : x) {
    const double term = std::abs(v*factor-endpoint)/w-compensation;
    const double next = distance+term;
    compensation = (next-distance)-term;
    distance = next;
  }
  const double d = distance/double(x.size());
  const double eps = 8 * std::numeric_limits<double>::epsilon();
  const double shift_error = eps * (std::abs(shift) +
      (std::abs(delta) + std::abs(mid_error)) / w + eps*double(x.size()));
  const double distance_error = eps*d + 4*std::numeric_limits<double>::denorm_min();
  if (!(d > 0 && d <= 0.5 + eps)) return {NAN, NAN, false};

  // The signed score decreases at a rate equal to the variance.
  // Use the distance from the mode once the tilt is large enough to
  // evaluate that moment without substantial cancellation.
  const auto score = [&](double t) {
    if (std::abs(t) <= 4) return std::abs(shift) - unit_tilt_shift(t);
    return t > 0 ? endpoint_mean(t) - d : 1 - d - endpoint_mean(-t);
  };
  const auto margin = [&](double t) {
    if (std::abs(t) <= 4) return shift_error + eps * std::abs(unit_tilt_shift(t));
    return distance_error + eps * (t > 0 ? 1 / t : 1);
  };
  double lo = 0, hi = std::nextafter(1 / d, INFINITY);
  if (std::abs(shift) <= 0.08) hi = 1;
  // The score at 1/d is negative since the truncated mean is less than 1/t.
  // Near zero use the linear term as a start, then bisect the monotone score.
  double t = std::min(hi, 12 * std::abs(shift));
  bool accurate = false;
  for (int it = 0; it < maxit; ++it) {
    const double g = score(t);
    const double noise = margin(t);
    if (std::abs(g) <= noise || hi - lo <= eps * t) break;
    if (g > 0) lo = t; else hi = t;
    t = lo + (hi - lo) / 2;
  }
  double rate = t == 0 ? 0 : direction * (t / w) * factor;
  if (std::isfinite(rate)) {
    // Check around the returned coefficient, including rounding on
    // conversion from the unit interval. The slack uses the smaller
    // possible contract scale anywhere in the verified interval.
    const double mapped = (std::abs(rate) / factor) * w;
    const double slack = std::min(tol, 0.5) / 2;
    const double radius = std::max(slack * mapped, (slack * w) / factor);
    const double left = mapped - radius, right = mapped + radius;
    accurate = score(left) > margin(left) && score(right) < -margin(right);
    t = mapped;
  }
  if (!accurate && std::abs(shift) <= shift_error && exact_uniform_score(x, a, b)) {
    rate = t = 0;
    accurate = true;
  }
  const double normalization = -std::expm1(-t);
  // In a very wide window the unit-interval variance can underflow
  // before dividing by the sample variance. Compare standard deviations.
  const double tail = t > 4 ? t * std::exp(-t / 2) / normalization : 0;
  const double face_sd = t <= 4 ? std::sqrt(mass(t, 0, 0, 1).var) :
      std::sqrt(1 - tail * tail) / t;
  const double ll = t <= 1 ? std::log(factor) - std::log(w) - t * d -
      (t == 0 ? 0 : std::log(normalization / t)) :
      std::log(std::abs(rate)) - t * d - std::log(normalization);
  const double empirical_sd = (sd * factor) / w;
  const double ratio = face_sd / empirical_sd;
  // A narrow signed bracket is needed in unit-window coordinates. An
  // accurate public rate can allow a much wider tilt on a large window.
  const double radius = face_moment_margin * (1 + t);
  const double left = t - radius, right = t + radius;
  double ratio_upper = INFINITY, ratio_lower = 0;
  if (accurate && score(left) > margin(left) && score(right) < -margin(right)) {
    // On the unit window the normalized density ratio is between
    // exp(-radius) and exp(radius) throughout the tilt bracket. Minimizing
    // the second moment over centers preserves both inequalities.
    // At large t, 1/left^2 also bounds the truncated variance above.
    double sd_upper = face_sd * std::exp(radius / 2);
    if (t > 4 && left > 0) sd_upper = std::min(sd_upper,1/left);
    const double upper = sd_upper / empirical_sd;
    ratio_upper = upper * upper;
    const double lower = ratio * std::exp(-radius / 2);
    ratio_lower = lower * lower;
  }
  return {rate, double(x.size()) * ll, accurate, ratio_upper, ratio_lower};
}

}  // namespace tnorm_detail

// Truncated normal MLE with known bounds a < b (either may be infinite).
// Observations may equal either finite endpoint. NaN bounds estimate the
// corresponding sample extreme. Pass a finite fixed_mean or
// fixed_sd to hold that parameter; NaN estimates it. tol is the relative
// Newton step tolerance in natural parameters and the relative accuracy
// requested for an exponential face rate (with unit floor near zero).
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
  // A shrinking estimated window gives f(c)/integral(f) ~ 1/width,
  // even with both parent coefficients held. Supplied equal bounds instead
  // describe an invalid design and are not this likelihood escape.
  if((std::isnan(a) || std::isnan(b)) && (std::isnan(a) || a==x[0]) &&
     (std::isnan(b) || b==x[0]) && (std::isnan(fixed_mean) || std::isfinite(fixed_mean)) &&
     (std::isnan(fixed_sd) || (fixed_sd>0 && std::isfinite(fixed_sd))) &&
     std::all_of(x.begin(),x.end(),[&](double v){return std::isfinite(v) && v==x[0];})) {
    res.status=TnormStatus::InvalidBounds;
    res.estimated_bounds_collapse=true;
    return res;
  }
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
  // The sum of nonnegative normalized squares incurs at most gamma_n
  // relative error. The remaining allowance covers scaling and moment evaluation.
  const double face_margin = tnorm_detail::face_moment_margin +
      8*n*std::numeric_limits<double>::epsilon();
  bool constant = true;
  for (double v : x) {
    if (!std::isfinite(v)) return res;
    if (!(v >= a && v <= b)) {
      res.status = TnormStatus::OutsideBounds;
      return res;
    }
    constant = constant && v == x[0];
  }
  const auto moments = norm_detail::moments(x);
  const double xbar = moments.mean;
  const bool lower = std::isfinite(a), upper = std::isfinite(b);

  if (hold_mean && hold_sd) {
    const double sd = fixed_sd, mu = fixed_mean;
    const double m2 = norm_detail::residuals(x,mu).standardized_square(sd) / n;
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
    const double m2 = norm_detail::residuals(x,xbar).standardized_square(sd) / n;
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
    const double s = norm_detail::residuals(x,mu).rms(x.size());
    if (!(s > 0)) {
      res.status = TnormStatus::DegenerateData;
      return res;
    }
    const double lo = (a - mu) / s, hi = (b - mu) / s;
    bool interior = true;
    if (lower && upper) {
      // Uniform face at t2 = 0; E u^2 under the uniform is (lo^2+lo*hi+hi^2)/3.
      res.limit_rate = 0;
      const double width = b-a;
      res.limit_logLik = -n*(std::isfinite(width) ? std::log(width) :
          std::log(b*.5-a*.5)+std::log(2.));
      const double face_second = (lo * lo + lo * hi + hi * hi) / 3;
      interior = face_second > 1 + face_margin;
      if (face_second < 1 - face_margin ||
          (!interior && tnorm_detail::held_mean_uniform_wins(x, a, width, mu, face_margin))) {
        res.status = TnormStatus::UniformBoundary;
        return res;
      }
    }
    double t2 = -0.5;
    if (lower || upper) {
      if (!tnorm_detail::newton_scale(lo, hi, 1, tol, maxit, t2, res.iterations)) {
        res.status = TnormStatus::IterationFailure;
        res.certified = false;
        return res;
      }
    }
    const auto m = tnorm_detail::mass(0, t2, lo, hi, false);
    res.sd = s / std::sqrt(-2 * t2);
    res.logLik = n * (t2 - m.log_mass) - n * std::log(s);
    res.status = m.ok ? TnormStatus::Ok : TnormStatus::IterationFailure;
    res.certified = interior;
    return res;
  }

  // Both free.
  if (constant) {
    res.status = TnormStatus::DegenerateData;
    return res;
  }
  const double s = moments.sd;
  if (!(s>0 && std::isfinite(s))) {res.status=TnormStatus::IterationFailure;return res;}
  if (!lower && !upper) {
    res.mean = xbar;
    res.sd = s;
    res.logLik = -n * (std::log(s) + log_root_2pi + 0.5);
    res.status = TnormStatus::Ok;
    return res;
  }
  const double lo = (a - xbar) / s, hi = (b - xbar) / s;
  // Match the exponential face to the sample mean before comparing variances.
  // Keep the mean correction in residual coordinates so that a narrow
  // window does not subtract two separately rounded absolute locations.
  const double correction = norm_detail::mean_rounding(x, xbar);
  // The compensated residual sum can overflow on widely separated data.
  // The normal mean already uses scaled summands in that case.
  const double delta = std::isfinite(correction) ? correction : 0;
  const double face_sd = std::isfinite(correction) ? norm_detail::exact_center_rms(x, xbar, s) : s;
  TnormStatus face;
  tnorm_detail::ExponentialFace limit;
  if (lower && upper) {
    face = TnormStatus::TruncatedExponentialBoundary;
    limit = tnorm_detail::finite_exponential_face(x, a, b, xbar, delta, face_sd, tol, maxit);
  } else {
    face = lower ? TnormStatus::ExponentialBoundary : TnormStatus::ReflectedExponentialBoundary;
    const double endpoint = lower ? a : b;
    const double factor = std::isfinite(xbar - endpoint) ? 1 : 0.5;
    const double d = std::abs((xbar * factor - endpoint * factor) + delta * factor);
    const double rate = factor / d;
    const double ratio = d / (face_sd * factor);
    limit = {lower ? -rate : rate, n * (std::log(rate) - 1),
             rate > 0 && std::abs(std::fma(rate, d, -factor)) <= tol * factor / 2 &&
             tol >= 8 * std::numeric_limits<double>::epsilon(), ratio * ratio, ratio * ratio};
  }
  res.limit_rate = limit.rate;
  res.limit_logLik = limit.loglik;
  const bool face_ok = limit.accurate && std::isfinite(limit.rate) && std::isfinite(limit.loglik);
  const bool strict_face = limit.variance_ratio_upper < 1 - face_margin;
  // For two distinct points with one at the finite endpoint, their
  // standard deviation and their mean distance from that endpoint are equal.
  const double endpoint = lower ? a : b;
  const bool endpoint_pair = lower != upper && x.size() == 2 &&
      (x[0] == endpoint || x[1] == endpoint);
  if (strict_face || endpoint_pair) {
    res.status = face_ok ? face : TnormStatus::IterationFailure;
    res.certified = strict_face;
    return res;
  }
  double t1 = 0, t2 = -0.5;
  if (!tnorm_detail::newton_joint(lo, hi, tol, maxit, t1, t2, res.iterations)) {
    res.status = TnormStatus::IterationFailure;
    res.certified = false;
    return res;
  }
  const auto m = tnorm_detail::mass(t1, t2, lo, hi, false);
  const double sd_u = 1 / std::sqrt(-2 * t2);
  res.mean = xbar + s * (t1 * sd_u * sd_u);
  res.sd = s * sd_u;
  res.logLik = n * (t2 - m.log_mass) - n * std::log(s);
  res.status = m.ok ? TnormStatus::Ok : TnormStatus::IterationFailure;
  res.certified = limit.variance_ratio_lower > 1 + face_margin;
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
    // With mean held outside the window, a squared-distance score about
    // that mean subtracts nearly equal moments. Center at the near endpoint
    // and use the varying part of the squared distance instead.
    const double origin=fm?std::clamp(mean,r.a,r.b):norm_detail::moments(x).mean;
    const double rms=norm_detail::residuals(x,origin).rms(x.size());
    const double scale=rms>0?rms:r.sd;
    double u1=0,u2=0,correction1=0,correction2=0;
    for(double k:x) {
      const double u=(k-origin)/scale;
      const double y1=u/n-correction1,next1=u1+y1;
      const double y2=u*u/n-correction2,next2=u2+y2;
      correction1=(next1-u1)-y1;u1=next1;
      correction2=(next2-u2)-y2;u2=next2;
    }
    const double lo=(r.a-origin)/scale,hi=(r.b-origin)/scale;
    const double theta2=-.5*(scale/r.sd)*(scale/r.sd);
    const double theta1=(r.mean-origin)/scale*(-2*theta2);
    const auto q=tnorm_detail::mass(theta1,theta2,lo,hi);
    const double cov=q.m3+2*q.mean*q.var;
    const double var2=q.m4-q.var*q.var+4*q.mean*q.m3+4*q.mean*q.mean*q.var;
    // Center and scale the dual enclosure so its arithmetic margin is
    // relative to each natural coordinate near the exponential face.
    const double scale2=-theta2;
    const double scale1=fm?scale2:std::max(scale2,std::abs(theta1));
    const int dim=(!fm && !fs)?2:1;
    double ia=0,ib=0,ic=0;
    if(fm) {
      ia=1/(theta1*theta1*q.var-2*theta1*scale2*cov+scale2*scale2*var2);
    } else if(fs) {
      ia=1/q.var/scale1/scale1;
    } else {
      const double det=q.var*var2-cov*cov;
      ia=var2/det/scale1/scale1;
      ib=-cov/det/scale1/scale2;
      ic=q.var/det/scale2/scale2;
    }
    const auto natural1=[&](double v){return fm?theta1*(1-v):std::fma(v,scale1,theta1);};
    const auto natural2=[&](double v,double w){return fs?theta2:std::fma(fm?v:w,scale2,theta2);};
    const auto evaluate=[&](double v,double w) {
      const double t1=natural1(v),t2=natural2(v,w);
      const auto z=tnorm_detail::mass(t1,t2,lo,hi);
      const double ll=t1*u1+t2*u2-z.log_mass-std::log(scale);
      const double g1=u1-z.mean,g2=u2-z.var-z.mean*z.mean;
      return concave_accept_detail::Evaluation{ll,fm?-theta1*g1+scale2*g2:scale1*g1,
        dim==2?scale2*g2:0,z.ok};
    };
    const auto accurate=[&](const concave_accept_detail::Bound& z) {
      double min_mean=INFINITY,max_mean=-INFINITY,min_sd=INFINITY,max_sd=0;
      for(double v:{z.lo1,z.hi1})for(double w:{z.lo2,z.hi2}) {
        const double t1=natural1(v),t2=natural2(v,w);
        if(!(t2<0))return false;
        const double mu=origin-scale*t1/(2*t2),sigma=scale/std::sqrt(-2*t2);
        min_mean=std::min(min_mean,mu);max_mean=std::max(max_mean,mu);
        min_sd=std::min(min_sd,sigma);max_sd=std::max(max_sd,sigma);
      }
      // The coordinate extrema occur at the box corners. Use the smallest
      // possible contract scale over the box, including a mean crossing zero.
      const double mean_floor=std::max({min_sd,min_mean,-max_mean});
      return (fm || std::max(r.mean-min_mean,max_mean-r.mean)<=tol*mean_floor) &&
             (fs || std::max(r.sd-min_sd,max_sd-r.sd)<=tol*min_sd);
    };
    const auto initial=evaluate(0,0);
    if(initial.ok)r.logLik=n*initial.value;
    auto bound=concave_accept_detail::certify(0,0,r.logLik/n,ia,ib,ic,dim,false,maxit,evaluate,accurate);
    bool locally_accurate = false;
    if(!bound.ok && ia>0 && (dim==1 || ia*ic>ib*ib)) {
      // Near a face a small natural-coordinate residual can still move
      // the public coefficients. Apply the information-scaled correction
      // against the compensated sample moments before replaying the box.
      double v=0,w=0,last_v=INFINITY,last_w=INFINITY;
      const double eps=4*std::numeric_limits<double>::epsilon();
      const double noise1=eps*(fm?std::abs(theta1)*(1+std::abs(u1))+scale2*(1+std::abs(u2)):
        scale1*(1+std::abs(u1)));
      const double noise2=dim==2?eps*scale2*(1+std::abs(u2)):0;
      const double noise_v=std::abs(ia)*noise1+std::abs(ib)*noise2;
      const double noise_w=std::abs(ib)*noise1+std::abs(ic)*noise2;
      auto current=evaluate(v,w);
      for(int it=0;it<maxit && current.ok;++it) {
        const double dv=ia*current.g1+ib*current.g2,dw=ib*current.g1+ic*current.g2;
        if(!std::isfinite(dv) || !std::isfinite(dw))break;
        const auto next=evaluate(v+dv,w+dw);
        if(!next.ok || std::hypot(next.g1,next.g2)>std::hypot(current.g1,current.g2)+std::hypot(noise1,noise2))break;
        v+=dv;w+=dw;current=next;
        last_v=std::abs(dv);last_w=std::abs(dw);
        if(last_v<=noise_v && last_w<=noise_w)break;
      }
      if(current.ok && natural2(v,w)<0) {
        if(!fm)r.mean=origin-scale*natural1(v)/(2*natural2(v,w));
        if(!fs)r.sd=scale/std::sqrt(-2*natural2(v,w));
        r.logLik=n*current.value;
        bound=concave_accept_detail::certify(v,w,current.value,ia,ib,ic,dim,false,maxit,evaluate,accurate);
        if(!bound.ok && std::isfinite(last_v) && std::isfinite(last_w)) {
          const double dv=std::abs(ia*current.g1+ib*current.g2);
          const double dw=std::abs(ib*current.g1+ic*current.g2);
          // Local Newton error after an applied step and contraction to
          // the score's arithmetic resolution. Map the error to public units.
          const double rv=2*dv+noise_v,rw=2*dw+noise_w;
          concave_accept_detail::Bound local;
          local.lo1=v-rv;local.hi1=v+rv;local.lo2=w-rw;local.hi2=w+rw;
          if(dv<=std::max(last_v/4,noise_v) && dw<=std::max(last_w/4,noise_w) && accurate(local))
            locally_accurate = true;
        }
      }
    }
    r.stop_detail.loglik_lower=r.logLik-1e-9*(n+std::abs(r.logLik));
    r.stop_detail.loglik_upper=n*bound.upper;
    // An interior coefficient enclosure can prove attainment independently
    // of the comparison at the face.
    r.certified = bound.ok;
    if(!bound.ok && !locally_accurate) {
      r.status=TnormStatus::IterationFailure;
      r.stop_detail.candidates.push_back({{r.mean,r.sd,r.a,r.b},r.stop_detail.loglik_lower,
        r.logLik+1e-9*(n+std::abs(r.logLik))});
    }
  }
  if(std::isnan(sd) && std::isfinite(r.limit_rate) &&
      (r.status==TnormStatus::IterationFailure || (r.status==TnormStatus::Ok && !r.certified))) {
    const auto refined=tnorm_detail::refine_near_face(x,r.a,r.b,r.mean,r.sd,mean,r.limit_rate,tol,maxit);
    if(refined.iterations>0) {
      r.mean=refined.mean;
      r.sd=refined.sd;
      r.iterations+=refined.iterations;
      r.status=TnormStatus::Ok;
      r.certified=true;
      r.stop_detail={};
    }
  }
  if(r.status==TnormStatus::UniformBoundary)r.stop_detail=limit_detail({r.a,r.b},r.limit_logLik,StopModel::Unif);
  if(r.status==TnormStatus::ExponentialBoundary)r.stop_detail=limit_detail({r.a,-r.limit_rate},r.limit_logLik,StopModel::TranslatedExp);
  if(r.status==TnormStatus::ReflectedExponentialBoundary)r.stop_detail=limit_detail({r.b,r.limit_rate},r.limit_logLik,StopModel::ReflectedExp);
  if(r.status==TnormStatus::TruncatedExponentialBoundary)r.stop_detail=limit_detail({r.a,r.b,r.limit_rate},r.limit_logLik,StopModel::TruncatedExp);
  if(r.status==TnormStatus::Ok) {
    double ll=0,correction=0;
    for(double v:x) {
      const double value=distributions::dtnorm(v,r.mean,r.sd,r.a,r.b,true)-correction;
      const double next=ll+value;correction=(next-ll)-value;ll=next;
    }
    r.logLik=ll;
    if(!std::isfinite(ll))r.status=TnormStatus::IterationFailure;
  }
  return r;
}
}

// dtnorm uses the quadrature above; both include orders remain self-contained.
#include "tnorm_dpqr.hpp"

namespace cppunivariate {
inline bool family_point(const TnormResult&, const FitCandidate& c) {
  return c.model == StopModel::None && c.coef.size() == 4 && std::isfinite(c.coef[0]) &&
         std::isfinite(c.coef[1]) && c.coef[1] > 0 && c.coef[2] < c.coef[3];
}
inline bool accurate_point(const TnormResult&, const FitCandidate&) { return false; }
inline FitOutcome outcome(const TnormResult& r) {
  if(r.estimated_bounds_collapse)return FitOutcome::nonexistence(OutcomeReason::Unbounded);
  if(r.stop_detail.accuracy_stop==AccuracyStop::Tolerance)return FitOutcome::failure(OutcomeReason::Tolerance);
  if(r.stop_detail.accuracy_stop==AccuracyStop::Conditioning)return FitOutcome::failure(OutcomeReason::Conditioning);
  switch(r.status) {
    case TnormStatus::Ok: return FitOutcome::singleton(r.certified);
    case TnormStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TnormStatus::InvalidBounds: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TnormStatus::OutsideBounds: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TnormStatus::InvalidFixed: return FitOutcome::failure(OutcomeReason::InvalidInput);
    case TnormStatus::DegenerateData: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case TnormStatus::ExponentialBoundary: return FitOutcome::limit(r.certified);
    case TnormStatus::ReflectedExponentialBoundary: return FitOutcome::limit(r.certified);
    case TnormStatus::TruncatedExponentialBoundary: return FitOutcome::limit(r.certified);
    case TnormStatus::UniformBoundary: return FitOutcome::limit(r.certified);
    case TnormStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case TnormStatus::UnboundedLocation: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
  }
  return {};
}
} // namespace cppunivariate
