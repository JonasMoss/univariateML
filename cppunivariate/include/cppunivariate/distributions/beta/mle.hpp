// Beta MLE: Newton-Raphson on log(beta), with alpha solved via a small
// digamma-inversion inner loop each iteration.
//
// Convergence is relative on log(beta): |db/b| < tol. Bit-identical to
// the prior arma'd implementation.
//
// Fallback. That iteration starts from the method of moments and returned
// NaN without notice when the start is invalid (data hugging 0 and 1, where
// the rounded one-pass variance makes m(1-m)/v - 1 <= 0) or the iteration
// leaves the domain. The log-likelihood is strictly concave in (alpha,
// beta) (an exponential family), so a damped Newton iteration on it from a
// two-pass moment start then finds the unique MLE. Constant data have no
// MLE (point-mass limit) and are flagged.

#pragma once

#include "../../core/reductions.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

enum class BetaStatus { Ok = 0, Degenerate, IterationFailure };

struct BetaResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
  BetaStatus status = BetaStatus::Ok;
};

namespace beta_detail {

// Avoid subtracting nearly equal digammas in concentrated held-shape fits.
inline double digamma_increment(double z, double h) {
  if (z >= 1e5) {
    const long double a = z, b = h, c = a+b;
    return std::log1p(b/a) + .5L*(b/a)/c + b*(2*a+b)/(12*a*a*c*c);
  }
  if (h < 1e-6*z)
    return h * cppunivariate::special::trigamma(z + .5*h);
  return cppunivariate::special::digamma(z+h) - cppunivariate::special::digamma(z);
}

// Local log-beta evaluation for held fits. The shared lgamma difference
// loses the entire normalizer when one shape dwarfs the other.
inline double held_logbeta(double a, double b) {
  const long double z = std::max(a,b), h = std::min(a,b);
  if (z < 1e6) return cppunivariate::special::lbeta(a,b);
  const long double r = h/z, lp = std::log1p(r);
  if (h >= 1e6) {
    const long double total = z+h;
    const auto correction = [](long double v) {return (1-1/(30*v*v))/(12*v);};
    return .5L*std::log(2*M_PI) - .5L*std::log(total) -
      (z-.5L)*lp + (h-.5L)*(std::log(r)-lp) +
      correction(z)+correction(h)-correction(total);
  }
  const long double lm = r < 1e-4 ?
    r*r*(-.5L+r*(1.0L/3+r*(-.25L+r*(.2L-r/6)))) : lp-r;
  const long double increment = h*std::log(z)+(h-.5L)*lp+z*lm-h/(12*z*(z+h));
  return cppunivariate::special::lgamma(static_cast<double>(h))-increment;
}

inline double loglik_per_obs(double a, double b, double s, double r) {
  return (a - 1.0) * s + (b - 1.0) * r - cppunivariate::special::lbeta(a, b);
}

// Damped Newton ascent on the concave per-observation log-likelihood.
inline BetaResult concave_newton(span x, double s, double r, int maxit) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const double dn = static_cast<double>(x.size());
  double m = 0.0;
  for (auto v : x) m += v;
  m /= dn;
  double v2 = 0.0;
  for (auto v : x) v2 += (v - m) * (v - m);
  v2 /= dn;
  double nu = m * (1.0 - m) / v2 - 1.0;
  if (!(nu > 0.0) || !std::isfinite(nu)) nu = 1e-2;
  double a = m * nu, b = (1.0 - m) * nu;
  int it = 0;
  for (; it < std::max(maxit, 200); ++it) {
    const double pab = cppunivariate::special::digamma(a + b);
    const double tab = cppunivariate::special::trigamma(a + b);
    const double ga = s - cppunivariate::special::digamma(a) + pab;
    const double gb = r - cppunivariate::special::digamma(b) + pab;
    const double haa = tab - cppunivariate::special::trigamma(a);
    const double hbb = tab - cppunivariate::special::trigamma(b);
    const double det = haa * hbb - tab * tab;
    // Newton step d = -H^{-1} g; H is negative definite.
    const double da = -(hbb * ga - tab * gb) / det;
    const double db = -(haa * gb - tab * ga) / det;
    if (!std::isfinite(da) || !std::isfinite(db)) break;
    const double f0 = loglik_per_obs(a, b, s, r);
    double t = 1.0;
    // Stay inside the domain, then backtrack on the objective.
    while ((a + t * da <= 0.0 || b + t * db <= 0.0) && t > 1e-300) t *= 0.5;
    bool accepted = false;
    for (int ls = 0; ls < 80; ++ls) {
      const double f1 = loglik_per_obs(a + t * da, b + t * db, s, r);
      if (std::isfinite(f1) && f1 >= f0 - 1e-15 * (std::abs(f0) + 1.0)) {
        accepted = true;
        break;
      }
      t *= 0.5;
    }
    if (!accepted) break;
    a += t * da;
    b += t * db;
    if (t == 1.0 && std::abs(da) <= 1e-13 * a && std::abs(db) <= 1e-13 * b) {
      const double ll = dn * loglik_per_obs(a, b, s, r);
      if (std::isfinite(ll)) return {a, b, ll, it + 1};
      break;
    }
  }
  return {kNaN, kNaN, kNaN, it, BetaStatus::IterationFailure};
}

}  // namespace beta_detail

inline BetaResult beta_mle(span x, double tol, int maxit,
    double shape1 = std::numeric_limits<double>::quiet_NaN(),
    double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0)
    return {0.0, 0.0, kNaN, 0};

  const double dn = static_cast<double>(n);
  double sum_log_x = 0.0, sum_log_1mx = 0.0, sum_x = 0.0, sum_xsq = 0.0;
  for (auto v : x) {
    sum_log_x += std::log(v);
    sum_log_1mx += std::log(1.0 - v);
    sum_x += v;
    sum_xsq += v * v;
  }
  if (!std::isnan(shape1) || !std::isnan(shape2)) {
    const bool fa = !std::isnan(shape1), fb = !std::isnan(shape2);
    if ((fa && (!(shape1 > 0) || !std::isfinite(shape1))) ||
        (fb && (!(shape2 > 0) || !std::isfinite(shape2))))
      return {kNaN, kNaN, kNaN, 0, BetaStatus::IterationFailure};
    sum_log_1mx = 0;
    for (double v : x) {
      if (!(v > 0 && v < 1))
        return {kNaN, kNaN, kNaN, 0, BetaStatus::IterationFailure};
      sum_log_1mx += std::log1p(-v);
    }
    double a = shape1, b = shape2;
    int it = 0;
    if (!(fa && fb)) {
      if (maxit < 1) return {kNaN,kNaN,kNaN,0,BetaStatus::IterationFailure};
      const double held = fa ? a : b;
      const double target = (fa ? sum_log_1mx : sum_log_x) / dn;
      // Strictly decreasing score, positive at zero and negative at infinity.
      const auto score = [&](double z) {
        return target + beta_detail::digamma_increment(z, held);
      };
      double lo = 0, hi = 1;
      while (score(hi) > 0 && hi < std::numeric_limits<double>::max()/2) hi *= 2;
      if (!(score(hi) <= 0))
        return {kNaN,kNaN,kNaN,0,BetaStatus::IterationFailure};
      double z = hi;
      for (; it < maxit; ++it) {
        z = lo + (hi - lo) / 2;
        const double g = score(z);
        if (g > 0) lo = z; else hi = z;
        if (hi - lo <= 1e-13 * z) break;
      }
      if (it == maxit) return {kNaN,kNaN,kNaN,it,BetaStatus::IterationFailure};
      if (fa) b = z; else a = z;
    }
    return {a,b,(a-1)*sum_log_x+(b-1)*sum_log_1mx -
      dn*beta_detail::held_logbeta(a,b),it};
  }
  bool all_equal = true;
  for (auto v : x) all_equal = all_equal && v == x[0];
  if (all_equal) return {kNaN, kNaN, kNaN, 0, BetaStatus::Degenerate};

  const double s = sum_log_x / dn;
  const double r = sum_log_1mx / dn;
  const double m = sum_x / dn;
  const double v_var = sum_xsq / dn - m * m;

  const double nu = m * (1.0 - m) / v_var - 1.0;
  double b = std::log((1.0 - m) * nu);
  double alpha = kNaN;
  int iter = 1;
  for (; iter <= maxit; ++iter) {
    const double exp_b = std::exp(b);
    const double digamma_b = cppunivariate::special::digamma(exp_b);
    const double trigamma_b = cppunivariate::special::trigamma(exp_b);
    const double y = s + digamma_b - r;

    alpha = 1.0 / std::log1p(std::exp(-y));
    for (int k = 0; k < 3; ++k) {
      const double ra = cppunivariate::special::digamma(alpha) - y;
      alpha -= ra / cppunivariate::special::trigamma(alpha);
    }

    const double f =
        exp_b * (digamma_b - r - cppunivariate::special::digamma(alpha + exp_b));
    const double df =
        f + exp_b * exp_b *
                (trigamma_b -
                 cppunivariate::special::trigamma(alpha + exp_b) *
                     (trigamma_b / cppunivariate::special::trigamma(alpha) + 1.0));
    const double b0 = b - f / df;
    if (std::abs((b0 - b) / b0) < tol) {
      b = b0;
      break;
    }
    b = b0;
  }

  const double beta = std::exp(b);
  const double logLik =
      dn * ((alpha - 1.0) * s + (beta - 1.0) * r - std::lgamma(alpha) -
            std::lgamma(beta) + std::lgamma(alpha + beta));
  if (iter <= maxit && alpha > 0.0 && std::isfinite(alpha) && beta > 0.0 &&
      std::isfinite(beta) && std::isfinite(logLik)) {
    return {alpha, beta, logLik, iter};
  }
  auto fallback = beta_detail::concave_newton(x, s, r, maxit);
  fallback.iterations += iter;
  return fallback;
}

} // namespace cppunivariate
