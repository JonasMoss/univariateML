// Gompertz MLE.
//
// Up-front overdispersion check: mean(x) < sd(x) ⇒ data is consistent
// with an exponential and the Gompertz b MLE doesn't exist. Otherwise
// 1D Newton on b with hybrid convergence; a is the closed-form
// maximiser given b.

#pragma once
#include "../../core/maximizer_set.hpp"
#include <string_view>
#include "../../core/outcome.hpp"
#include "../../core/work_budget.hpp"
#include "../../core/stop_detail.hpp"
#include "../../core/precision.hpp"

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"

#include <cmath>
#include <algorithm>
#include <cstddef>
#include <limits>
#include <optional>
#include <vector>

namespace cppunivariate {

// NumericalFailure: the MLE of the rescaled data does not map back into
// binary64 (a coefficient overflows, or with a tolerance detail lies below
// binary64 resolution). InvalidInput: an empty sample, or an observation
// that is not finite or lies below the support.
enum class GompertzStatus { Ok = 0, Overdispersed, IterationFailure, Unbounded, NumericalFailure, InvalidInput, InitialHazardUnderflow };

// Negative log-likelihood of Gompertz(a, b) [actuar: shape=a, rate=b]:
//   log f = log(a) + b*x - (a/b) * (exp(b*x) - 1)
// Per-obs Hessian (sympy):
//   H_aa/obs = 1/a^2                                   (data-indep diagonal)
//   H_bb/obs = a*(b^2 x^2 e - 2 b x e + 2 e - 2) / b^3,  e = exp(b*x)
//   H_ab/obs = (b*x*e - e + 1) / b^2
struct GompertzResult {
  double a;
  double b;
  double logLik;
  int iterations;
  GompertzStatus status;
 StopDetail stop_detail{};
  // Overdispersed only: the sample lies inside the rounding pad of the
  // exponential boundary, so the limit is the preferred reading, not a proof.
  bool margin_decided = false;
  MaximizerSet maximizers{};
};

namespace gompertz_detail {
constexpr double unit_roundoff = 0x1p-53;

struct Moments {
  double sh = 0, sl = 0, qh = 0, ql = 0;
  int exponent = 0;
};

inline double two_sum(double a, double b, double& err) {
  const double s = a + b, bb = s - a;
  err = (a - (s - bb)) + (b - bb);
  return s;
}

// Power-of-two scaling preserves the face comparison and avoids overflow.
inline Moments scaled_moments(span x, double xmax) {
  Moments m;
  std::frexp(xmax, &m.exponent);
  for (double v : x) {
    const double z = std::ldexp(v, -m.exponent);
    double err = 0;
    m.sh = two_sum(m.sh, z, err);
    m.sl += err;
    const double p = z * z, pe = std::fma(z, z, -p);
    m.qh = two_sum(m.qh, p, err);
    m.ql += err;
    m.ql += pe;
  }
  return m;
}

struct Comparison {
  double value;
  double pad;

  int sign() const {
    return value > pad ? 1 : value < -pad ? -1 : 0;
  }
};

inline double gamma(double k) {
  return k * unit_roundoff / (1 - k * unit_roundoff);
}

// Bound (u = 2^-53, gamma_k = k u / (1 - k u), n <= 2^30, correctly rounded
// binary64 operations with no extended-precision intermediates, std::fma
// exact, z_i^2 and z_i not underflowing; the last two are covered by U):
//  1. Sum2: S = sh + sl_exact, and fl(sl) differs from sl_exact by at most
//     gamma_{n-1} sum|e_i| <= gamma_n^2 S; likewise Q = qh + ql_exact with
//     |fl(ql) - ql_exact| <= gamma_{2n} (gamma_n + u) Q <= gamma_{2n}^2 Q.
//  2. With S^ = sh + sl, Q^ = qh + ql (real numbers), T^ = n Q^ - 2 S^2 differs
//     from T by at most n gamma_{2n}^2 Q + 2 gamma_n^2 S (2 S + gamma_n^2 S)
//     <= 5 gamma_{2n}^2 nQ, using S^2 <= n Q (Cauchy-Schwarz) and n gamma_n^2 << 1.
//  3. n qh - 2 sh^2 = d + f + e1 - 2 e2 exactly (two TwoProd, one TwoSum). The
//     rest, B = n ql - 4 sh sl - 2 sl^2, and t = f + e1 - 2 e2 + B take at
//     most eight roundings, so |fl(t) - t| <= gamma_8 W with
//     W = |f| + |e1| + 2|e2| + n|ql| + 4|sh sl| + 2 sl^2, itself computed with
//     relative error below 8u. The final d + t is one rounding and preserves
//     the sign.
//  4. Underflow: each z_i or product below 2^-1022 loses at most 2^-1074, which
//     moves T by at most 8 n^2 2^-1074; U = 16 n^2 2^-1074.
// The pad P = 6 gamma_{2n}^2 N + 16 u W + U, with N >= n Q computed upward, so
// |T - T_computed| <= P; the comparison uses (1 + 10^-6) P to absorb the O(u)
// relative slack in the evaluation of P and in the sign-preserving final sum.
// Steps 1 and 3 are the cited bounds; step 2 and the constants are derived here
// and checked only by the test below, not by an independent proof assistant.
inline Comparison free_face(const Moments& m, double dn) {
  const double sh = m.sh, sl = m.sl, qh = m.qh, ql = m.ql;
  const double p1 = dn * qh, e1 = std::fma(dn, qh, -p1);
  const double p2 = sh * sh, e2 = std::fma(sh, sh, -p2);
  double f = 0.0;
  const double d = two_sum(p1, -2.0 * p2, f);
  const double b = (dn * ql - 4.0 * (sh * sl)) - 2.0 * (sl * sl);
  const double t = ((f + e1) - 2.0 * e2) + b;
  const double td = d + t;
  const double u = unit_roundoff;
  const double w = std::abs(f) + std::abs(e1) + 2.0 * std::abs(e2) + std::abs(dn * ql) +
      4.0 * std::abs(sh * sl) + 2.0 * (sl * sl);
  const double g2n = gamma(2.0 * dn);
  const double big_n = dn * (qh + std::abs(ql)) * (1.0 + 8.0 * u);
  const double underflow = 16.0 * dn * dn * std::ldexp(1.0, -1074);
  const double pad = (6.0 * g2n * g2n * big_n + 16.0 * u * w + underflow) * (1.0 + 1e-6);
  return {td, pad};
}

// The held-a score at zero, in scaled coordinates, is S - alpha Q/2.
// TwoSum and TwoProd leave only the low-word expression to evaluate in
// binary64. The pad covers moment errors, low-word rounding and subnormals.
inline Comparison held_face(const Moments& m, double n, double alpha) {
  // z_max >= 1/2 gives Q >= 1/4 and S < n.
  if (alpha >= 8 * n) return {-1, 0};
  const double half = alpha / 2;
  const double p = half * m.qh, pe = std::fma(half, m.qh, -p);
  double de = 0;
  const double d = two_sum(m.sh, -p, de);
  const double low_q = half * m.ql;
  const double t = ((de - pe) + m.sl) - low_q;
  const double gs = gamma(n), gq = gamma(2 * n);
  const double w = std::abs(de) + std::abs(pe) + std::abs(m.sl) + std::abs(low_q);
  const double pad = 2 * (gs * gs * m.sh + gq * gq * p)
      + 16 * unit_roundoff * w
      + 16 * n * (1 + alpha) * std::numeric_limits<double>::denorm_min();
  return {d + t, pad * (1 + 1e-6)};
}

// H(beta) = sum z_i * integral_0^1 exp(beta*z_i*t) dt.
// The free score has sign S*H/n - H', and the held-a score is S-alpha*H'.
// Keep the score at zero separate so that rounding H and H' cannot erase it.
inline std::optional<GompertzResult> small_rate(span x, const Moments& m,
    Comparison at_zero, double alpha, double held_a, double tol, int maxit) {
  constexpr double cutoff = 0.01;
  constexpr int order = 8;
  const double n = static_cast<double>(x.size());
  const double mean = (m.sh + m.sl) / n;
  const bool free_a = std::isnan(held_a);
  const double weight = free_a ? 1 : alpha;
  const double weighted_mean = free_a ? mean : 0;
  // At the cutoff the first-order estimate is enough to skip accumulating
  // the full Taylor polynomial on samples well away from the face:
  // H' - Q/2 <= beta*exp(beta)*Q/3 (z <= 1).
  if (at_zero.value > cutoff * 1.011 * weight * (m.qh + m.ql) / 3)
    return std::nullopt;

  // h[j] is the coefficient of beta^j in H, j>=1. Kahan summation
  // keeps the relative error independent of the number of observations here.
  double h[order + 2]{}, correction[order + 2]{};
  h[0] = m.sh + m.sl;
  h[1] = (m.qh + m.ql) / 2;
  for (double v : x) {
    const double z = std::ldexp(v, -m.exponent);
    double term = z * z / 2;
    for (int j = 2; j <= order + 1; ++j) {
      term *= z / (j + 1);
      const double y = term - correction[j];
      const double sum = h[j] + y;
      correction[j] = (sum - h[j]) - y;
      h[j] = sum;
    }
  }
  double c[order + 1]{};
  for (int j = 1; j <= order; ++j)
    c[j] = weighted_mean * h[j] - weight * (j + 1) * h[j + 1];

  const double correction_scale = weighted_mean * h[1] + 2 * weight * h[2];
  const double constant_pad = at_zero.pad + 4 * unit_roundoff * std::abs(at_zero.value);
  const auto score = [&](double beta) {
    double t = 0;
    for (int j = order; j > 0; --j) t = beta * (c[j] + t);
    // At beta <= .02, the Taylor remainder is below beta times
    // the correction scale times 1e-19. Higher moment coefficients
    // are bounded by the first ones because z <= 1. The practical
    // rounding pad covers the moment sums and the polynomial evaluation.
    const double pad = constant_pad + 128 * unit_roundoff * beta * correction_scale;
    return Comparison{at_zero.value + t, pad};
  };
  if (score(cutoff).sign() >= 0) return std::nullopt;

  double lo = 0, hi = cutoff;
  // Use the linearized zero to shorten the bracket when the root is
  // many orders of magnitude below the cutoff. Check the sign before
  // using this as an endpoint.
  if (c[1] < 0) {
    const double near = 2 * at_zero.value / -c[1];
    if (near > 0 && near < hi && score(near).sign() < 0) hi = near;
  }
  constexpr double nan = std::numeric_limits<double>::quiet_NaN();
  int i = 1;
  for (; i <= maxit; ++i) {
    const double beta = lo + (hi - lo) / 2;
    const double b = std::ldexp(beta, -m.exponent);
    if (b > 0 && std::isfinite(b)) {
      const double represented = std::ldexp(b, m.exponent);
      const double radius = 0.5 * std::min(tol, 1.0);
      const double lower = represented * (1 - radius);
      const double upper = represented * (1 + radius);
      if (lower < represented && upper > represented && upper <= 2 * cutoff &&
          score(lower).sign() > 0 && score(upper).sign() < 0) {
        double integral = h[order + 1];
        for (int j = order; j >= 0; --j) integral = h[j] + represented * integral;
        const double a = free_a ? std::ldexp(n / integral, -m.exponent) : held_a;
        const double ll = n * std::log(a) + represented * h[0] -
            (free_a ? n : alpha * integral);
        if (a > 0 && std::isfinite(a) && std::isfinite(ll) &&
            (!free_a || std::numeric_limits<double>::denorm_min() / a <= tol))
          return GompertzResult{a, b, ll, i, GompertzStatus::Ok};
        return GompertzResult{nan, nan, nan, i, GompertzStatus::NumericalFailure};
      }
    }
    if (beta == lo || beta == hi) break;
    if (score(beta).value > 0) lo = beta;
    else hi = beta;
  }
  return GompertzResult{nan, nan, nan, std::min(i, maxit), GompertzStatus::IterationFailure};
}
}  // namespace gompertz_detail

inline FitOutcome outcome(const GompertzResult& r) {
  if (r.stop_detail.accuracy_stop == AccuracyStop::Tolerance) return FitOutcome::failure(OutcomeReason::Tolerance);
  if (r.stop_detail.accuracy_stop == AccuracyStop::Conditioning) return FitOutcome::failure(OutcomeReason::Conditioning);
  switch (r.status) {
    case GompertzStatus::Ok: return FitOutcome::estimate(true,r.maximizers.size == 0);
    case GompertzStatus::Overdispersed: return FitOutcome::limit(!r.margin_decided);
    case GompertzStatus::IterationFailure: return FitOutcome::failure(OutcomeReason::NoConvergence);
    case GompertzStatus::Unbounded: return FitOutcome::nonexistence(OutcomeReason::Unbounded);
    case GompertzStatus::InitialHazardUnderflow:
    case GompertzStatus::NumericalFailure: return FitOutcome::failure(OutcomeReason::Evaluation);
    case GompertzStatus::InvalidInput: return FitOutcome::failure(OutcomeReason::InvalidInput);
  }
  return {};
}

inline GompertzResult gompertz_mle_impl(span x, double tol, int maxit, bool standardized) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN, 0, GompertzStatus::IterationFailure};

  // The face is decided once, on the original data (see gompertz_detail); the
  // standardized recursion trusts that decision and does not repeat a
  // rounded comparison.
  if (!standardized) {
    if (n > (std::size_t(1) << 30)) {
      GompertzResult limit{kNaN, kNaN, kNaN, 0, GompertzStatus::Overdispersed};
      limit.margin_decided = true;
      return limit;
    }
    const double xmax = *std::max_element(x.begin(), x.end());
    const auto moments = gompertz_detail::scaled_moments(x, xmax);
    const auto comparison = gompertz_detail::free_face(moments, n);
    const int face = comparison.sign();
    if (face >= 0) {
      GompertzResult limit{kNaN, kNaN, kNaN, 0, GompertzStatus::Overdispersed};
      limit.margin_decided = face == 0;
      return limit;
    }
    if (const auto small = gompertz_detail::small_rate(
            x, moments, {-comparison.value / (2 * n), comparison.pad / (2 * n)},
            kNaN, kNaN, tol, maxit))
      return *small;
  }

  CPPUNIV_EXTENDED_REAL total = CPPUNIV_EXTENDED_LITERAL(0.0,0.0L);
  for (auto v : x) total += v;
  double unit = static_cast<double>(total / static_cast<CPPUNIV_EXTENDED_REAL>(n));
  if (!standardized && !std::isfinite(unit)) {
    // The sum overflows binary64 (double-only build, data near DBL_MAX) while the
    // mean does not: form the mean from the maximum-scaled terms so that the
    // standardization, and with it the exponential-limit decision, matches the
    // extended-precision build.
    double xmax = 0.0;
    for (auto v : x) if (v > xmax) xmax = v;
    if (xmax > 0.0 && std::isfinite(xmax)) {
      double scaled = 0.0;
      for (auto v : x) scaled += v / xmax;
      unit = xmax * (scaled / static_cast<double>(n));
    }
  }
  if (unit > 0.0 && std::isfinite(unit) && !standardized) {
    std::vector<double> z(n);
    for (std::size_t i = 0; i < n; ++i) z[i] = x[i] / unit;
    const auto r = gompertz_mle_impl(span{z.data(), n}, tol, maxit, true);
    GompertzResult out{r.a/unit, r.b/unit, r.logLik-static_cast<double>(n)*std::log(unit), r.iterations, r.status};
    if (out.status != GompertzStatus::Ok) return out;
    // a and b scale as 1/unit. On (1, 2, 3)*denorm_min they are 1.5e322 and
    // 2.5e323 (TASK-214.3): above binary64. Below the normal range the
    // relative spacing is denorm_min/coefficient.
    const double log_tiny = std::log(std::numeric_limits<double>::denorm_min()) + std::log(unit);
    const double floor = std::exp(log_tiny - std::min(std::log(r.a), std::log(r.b)));
    if (!(std::isfinite(out.a) && std::isfinite(out.b) && out.a > 0.0 && out.b > 0.0 &&
          std::isfinite(out.logLik) && floor <= std::min(tol, 1e-10))) {
      const bool tiny = std::isfinite(out.a) && std::isfinite(out.b) && std::isfinite(out.logLik);
      GompertzResult bad{kNaN, kNaN, tiny ? out.logLik : kNaN, r.iterations, GompertzStatus::NumericalFailure};
      if (tiny) {
        bad.stop_detail.accuracy_stop = AccuracyStop::Tolerance;
        bad.stop_detail.attainable_tol = floor;
      }
      return bad;
    }
    return out;
  }

  const double dn = static_cast<double>(n);
  double sum_x = 0.0;
  for (auto v : x) {
    sum_x += v;
  }
  // Profile likelihood is linear minus log of a moment generating
  // function: strictly concave. Its right derivative at zero is
  // n*(mean - mean(x^2)/(2*mean)), so equality also selects exponential;
  // that comparison was decided above (gompertz_detail::free_face).

  auto f_over_df = [&](double b) {
    double sum_bx = 0.0, sum_bx_x = 0.0, sum_bx_x2 = 0.0;
    for (auto v : x) {
      const double e = std::exp(b * v);
      sum_bx += e;
      sum_bx_x += v * e;
      sum_bx_x2 += v * v * e;
    }
    const double s = sum_bx - dn;
    const double r = b * sum_bx_x - s;
    const double a_local = b * dn / s;
    const double da = -dn * r / (s * s);
    const double d2a =
        dn / (s * s) *
        (b * (2.0 * sum_bx_x * sum_bx_x / s - sum_bx_x2) - 2.0 * sum_bx_x);
    const double f = dn * da / a_local + sum_x;
    const double df = dn * (a_local * d2a - da * da) / (a_local * a_local);
    return f / df;
  };

  double b = 1e-1;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double step = f_over_df(b);
    b -= step;
    if (b <= 0.0 || !std::isfinite(b)) {
      return {kNaN, kNaN, kNaN, iter, GompertzStatus::IterationFailure};
    }
    // The profile is strictly concave in b (see above), so the Newton step
    // is the first-order error of the iterate before the step and the error
    // after the applied step is quadratic in it. The data are standardized
    // to mean 1, so b is the public b times the mean, and a relative bound
    // on the step is the contract's relative bound on b (b > 0 has no
    // location floor). The step has been applied when the test is made.
    if (std::abs(step) < std::abs(b) * tol) break;
  }

  double sum_exp_bx = 0.0;
  for (auto v : x) sum_exp_bx += std::exp(b * v);
  const double a = b * dn / (sum_exp_bx - dn);
  const double logLik =
      dn * std::log(a) + b * sum_x - (a / b) * (sum_exp_bx - dn);
  return {a, b, logLik, iter,
      iter <= maxit ? GompertzStatus::Ok : GompertzStatus::IterationFailure};
}

inline GompertzResult gompertz_mle(span x, double tol, int maxit) {
  maxit = static_cast<int>(solver_budget(maxit));
  const double invalid = std::numeric_limits<double>::quiet_NaN();
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 || !std::all_of(x.begin(),x.end(),[](double v){return v>=0 && std::isfinite(v);}))
    return {invalid,invalid,invalid,0,GompertzStatus::InvalidInput};
  if(std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];})) {
    const double nan=std::numeric_limits<double>::quiet_NaN();
    return {nan,nan,nan,0,GompertzStatus::Unbounded};
  }
  const auto extrema = std::minmax_element(x.begin(), x.end());
  const double minimum = *extrema.first, maximum = *extrema.second;
  // At b0 = z/minimum, b0 times the mean profile score per datum is
  // at least 1 - b0*(maximum-minimum) - b0*maximum/expm1(z).
  // A positive score puts the unique maximum above b0. There a(b) is
  // smaller than a(b0) <= b0/expm1(z), below binary64 even at denorm_min.
  constexpr double z = 2048;
  if (minimum > 0 && (maximum - minimum) / minimum < 0.5 / z)
    return {invalid,invalid,invalid,0,GompertzStatus::InitialHazardUnderflow};
  auto r=gompertz_mle_impl(x,tol,maxit,false);
  if(r.status==GompertzStatus::Overdispersed) {
    double mean=0;for(double v:x)mean+=v/x.size();
    const double rate=1/mean;
    r.stop_detail=limit_detail({rate},x.size()*(std::log(rate)-1),StopModel::Exp);
  }
  return r;
}

inline GompertzResult gompertz_mle(span x,double tol,int maxit,double a,double b) {
  maxit = static_cast<int>(solver_budget(maxit));
  if(std::isnan(a) && std::isnan(b))return gompertz_mle(x,tol,maxit);
  const double nan=std::numeric_limits<double>::quiet_NaN();
  if(x.empty() || !(tol>0 && std::isfinite(tol)) || maxit<1 || !std::all_of(x.begin(),x.end(),[](double v){return v>=0 && std::isfinite(v);}))
    return {nan,nan,nan,0,GompertzStatus::InvalidInput};
  if(!(tol>0 && std::isfinite(tol)) || maxit<=0 || (!std::isnan(a) && !(a>0 && std::isfinite(a))) ||
     (!std::isnan(b) && !(b>0 && std::isfinite(b))))return {nan,nan,nan,0,GompertzStatus::IterationFailure};
  double big=0;
  for(double v:x){if(!(v>=0 && std::isfinite(v)))return {nan,nan,nan,0,GompertzStatus::InvalidInput};big=std::max(big,v);}
  const double n=static_cast<double>(x.size());int iterations=0;
  if(std::isnan(b)) {
    if(big==0) {
      GompertzResult out{a,1.0,n*std::log(a),0,GompertzStatus::Ok};
      // At zero the density equals a for every admissible positive b.
      Box piece;
      piece.axes = {fixed(a), interval(0, maxset_inf, false, false)};
      out.maximizers.dimension = 2;
      out.maximizers.add_box(piece);
      return out;
    }
    double sumy = 0;
    for (double v : x) sumy += v / big;
    const auto exponential_limit = [&](bool margin_decided) {
      GompertzResult r{a, nan, nan, 0, GompertzStatus::Overdispersed};
      r.margin_decided = margin_decided;
      r.stop_detail = limit_detail({a}, n * std::log(a) - a * big * sumy, StopModel::Exp);
      return r;
    };
    if (x.size() <= (std::size_t(1) << 30)) {
      const auto moments = gompertz_detail::scaled_moments(x, big);
      const double alpha = std::ldexp(a, moments.exponent);
      const auto comparison = gompertz_detail::held_face(moments, n, alpha);
      const int sign = comparison.sign();
      if (sign <= 0) return exponential_limit(sign == 0);
      if (const auto small = gompertz_detail::small_rate(x, moments, comparison, alpha, a, tol, maxit))
        return *small;
    } else {
      // Beyond the moment pad's stated count range, preserve the original
      // comparison as a numerical preference.
      double sumy2 = 0;
      for (double v : x) {
        const double y = v / big;
        sumy2 += y * y;
      }
      if (!(sumy > (a * big) * (sumy2 / 2))) return exponential_limit(true);
    }
    const double logsx=std::log(big)+std::log(sumy);
    auto ratio=[&](double t) -> double {double total=0;for(double v:x)if(v>0){const double z=std::exp(t+std::log(v));if(!std::isfinite(z))return INFINITY;double lh;
      if(z<1e-3)lh=std::log(.5+z/3+z*z/8+z*z*z/30+z*z*z*z/144);
      else if(z>50)lh=z+std::log(z-1)-2*std::log(z);
      else lh=std::log((z*std::exp(z)-std::expm1(z))/(z*z));
      total+=std::exp(std::log(a)+2*std::log(v)+lh-logsx);
    }return total;};
    const double lower=std::log(std::numeric_limits<double>::denorm_min()),upper=std::log(std::numeric_limits<double>::max());
    double lo=std::max(lower,std::min(upper,-std::log(big)-2)),hi=std::min(upper,lo+4),step=32;
    for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (ratio(lo)>=1 && lo>lower); ++bracket_pass){lo=std::max(lower,lo-step);step*=2;}
    step=32;for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (ratio(hi)<=1 && hi<upper); ++bracket_pass){hi=std::min(upper,hi+step);step*=2;}
    if(!(ratio(lo)<1 && ratio(hi)>1))return {a,nan,nan,0,GompertzStatus::IterationFailure};
    for(int j=0;j<maxit && hi-lo>1e-12;++j){const double mid=lo/2+hi/2;if(ratio(mid)<1)lo=mid;else hi=mid;++iterations;}
    b=std::exp(lo/2+hi/2);
    if(!(b>0 && std::isfinite(b) && std::abs(1-ratio(std::log(b)))*std::exp(std::log(b)+logsx)<1e-3))return {a,nan,nan,iterations,GompertzStatus::IterationFailure};
  }
  // Sum integrals in log coordinates; multiply by a before exponentiating.
  double log_integral=-INFINITY;
  for(double v:x)if(v>0){const double z=b*v;const double term=z<1e-8?std::log(v)+std::log1p(z/2+z*z/6):(z>50?z:std::log(std::expm1(z)))-std::log(b);const double high=std::max(log_integral,term);log_integral=high+std::log1p(std::exp(std::min(log_integral,term)-high));}
  if(std::isnan(a) && log_integral==-INFINITY)return {nan,b,nan,iterations,GompertzStatus::Unbounded};
  if(std::isnan(a))a=std::exp(std::log(n)-log_integral);
  CPPUNIV_EXTENDED_REAL sum_bx=0;for(double v:x)sum_bx+=b*v;
  const double ll=static_cast<double>(n*std::log(a)+sum_bx)-std::exp(std::log(a)+log_integral);
  if(!(a>0 && std::isfinite(a) && std::isfinite(ll)))return {a,b,ll,iterations,GompertzStatus::IterationFailure};
  return {a,b,ll,iterations,GompertzStatus::Ok};
}

}  // namespace cppunivariate
