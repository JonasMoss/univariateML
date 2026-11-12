// Beta MLE.
//
// The log-likelihood is strictly concave in (alpha, beta): the beta family is
// a minimal regular exponential family. The free fit works in mean-precision
// coordinates alpha = mu*t, beta = (1-mu)*t, where the leading terms of the
// scores cancel analytically. With R(z,h) = psi(z+h) - psi(z) - log1p(h/z),
// d_i = x_i - mu and KL(mu||x) the binary Kullback-Leibler divergence,
//   (1/n) dl/dt         = -mean KL(mu||x_i) + mu R(alpha,beta) + (1-mu) R(beta,alpha),
//   (1/n) dl/dmu / t    = mean[log1p(d_i/mu) - log1p(-d_i/(1-mu))]
//                         + R(alpha,beta) - R(beta,alpha).
// Every term is computed without cancellation, so nearly constant data and
// data near 0 or 1 keep their information. In (alpha, beta) the
// concentration is a second-order difference of mean logs that rounding
// destroys (TASK-148).
//
// Search. Along a ray (fixed mu) the likelihood is concave in t, so dl/dt
// strictly decreases and t_hat(mu) is a unique root. The superlevel sets of a
// concave function are convex, so the profile over mu is unimodal and its
// score crosses zero once. Both levels are bracketed, safeguarded Newton
// root-finds (log t, logit mu); starts affect speed only. The outer search
// stops when computed signs bracket the root within tol in logit(mu).
//
// The log-likelihood uses the same coordinates:
//   l = -n t mean KL - sum log x - sum log1p(-x)
//       + n [log(mu (1-mu) t / (2 pi))/2 + w(t) - w(alpha) - w(beta)],
// with w the Stirling remainder of lgamma.
//
// Held fits. With one shape held, the free shape's score is strictly
// decreasing and is solved by bracketed bisection.

#pragma once
#include "../../core/work_budget.hpp"
#include "cppunivariate/core/constants.hpp"
#include "../../core/precision.hpp"

#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>

namespace cppunivariate {

enum class BetaStatus { Ok = 0, Degenerate, IterationFailure, InvalidInput };

struct BetaResult {
  double alpha;
  double beta;
  double logLik;
  int iterations;
  BetaStatus status = BetaStatus::Ok;
};

namespace beta_detail {

// d*(1+q+...+q^(n-1)), i.e. 1-q^n with d = 1-q, without cancellation.
inline double power_difference(double q, double d, unsigned n) {
  double sum = 1, term = 1;
  for (unsigned k = 1; k < n; ++k) { term *= q; sum += term; }
  return d * sum;
}

// psi(z+h) - psi(z) for z, h > 0. Below z = 20 a small increment is moved up
// by the exact recurrence psi(z+1) = psi(z) + 1/z; above, the asymptotic
// expansion is differenced term by term through z^-8. These are asymptotic
// numerical formulas, not certified uniform error bounds.
inline double psi_increment(double z, double h) {
  if (z < 20) {
    if (h >= .01 * z) return special::digamma(z + h) - special::digamma(z);
    double correction = 0;
    while (z < 20) { correction += (h / (z + h)) / z; z += 1; }
    return correction + psi_increment(z, h);
  }
  const double r = h / z, q = 1 / (1 + r), inv = 1 / z, d = r / (1 + r), v = inv * inv;
  return std::log1p(r) + .5 * inv * d
    + v * power_difference(q, d, 2) / 12
    - v * v * power_difference(q, d, 4) / 120
    + v * v * v * power_difference(q, d, 6) / 252
    - v * v * v * v * power_difference(q, d, 8) / 240;
}

// psi'(z) - psi'(z+h) for z, h > 0, by the same construction.
inline double tri_difference(double z, double h) {
  if (z < 20) {
    if (h >= .01 * z) return special::trigamma(z) - special::trigamma(z + h);
    double correction = 0;
    while (z < 20) {
      correction += (h / (z + h)) / z * (1 / z + 1 / (z + h));
      z += 1;
    }
    return correction + tri_difference(z, h);
  }
  const double r = h / z, q = 1 / (1 + r), inv = 1 / z, d = r / (1 + r), v = inv * inv;
  return inv * d + .5 * v * power_difference(q, d, 2)
    + inv * v * power_difference(q, d, 3) / 6
    - inv * v * v * power_difference(q, d, 5) / 30
    + inv * v * v * v * power_difference(q, d, 7) / 42
    - inv * v * v * v * v * power_difference(q, d, 9) / 30;
}

// psi(w) - log(w).
inline double psi_minus_log(double w) {
  if (w < 20) return special::digamma(w) - std::log(w);
  const double v = 1 / (w * w);
  return -.5 / w - v * (1. / 12 - v * (1. / 120 - v * (1. / 252 - v * (1. / 240 - v / 132))));
}

// psi'(z) - 1/z.
inline double trigamma_minus_inv(double z) {
  if (z < 20) return special::trigamma(z) - 1 / z;
  const double v = 1 / (z * z);
  return v * (.5 + (1 / z) * (1. / 6 - v * (1. / 30 - v * (1. / 42 - v / 30))));
}

// R(z,h) = psi(z+h) - psi(z) - log1p(h/z) for z, h > 0.
inline double psi_increment_rest(double z, double h) {
  if (z >= 20) {
    const double r = h / z, q = 1 / (1 + r), inv = 1 / z, d = r / (1 + r), v = inv * inv;
    return .5 * inv * d
      + v * power_difference(q, d, 2) / 12
      - v * v * power_difference(q, d, 4) / 120
      + v * v * v * power_difference(q, d, 6) / 252
      - v * v * v * v * power_difference(q, d, 8) / 240;
  }
  if (h >= .01 * z) return psi_minus_log(z + h) - psi_minus_log(z);
  return psi_increment(z, h) - std::log1p(h / z);
}

// u - log1p(u) for u > -1, without cancellation near zero.
inline double u_minus_log1p(double u) {
  if (std::abs(u) >= .1) return u - std::log1p(u);
  double sum = 0, power = u;
  for (int k = 2; k <= 17; ++k) {
    power *= -u;
    sum += power / k;
  }
  return -sum;
}

// Stirling remainder: lgamma(z) - [(z - 1/2) log z - z + log(2 pi)/2].
inline double stirling_remainder(double z) {
  if (z < 30)
    return special::lgamma(z) - ((z - .5) * std::log(z) - z + .5 * std::log(2 * cppunivariate::pi));
  const double v = 1 / (z * z);
  return (1. / 12 - v * (1. / 360 - v * (1. / 1260 - v * (1. / 1680 - v * (1. / 1188 - v * 691. / 360360))))) / z;
}

// log B(a, b). The lgamma difference loses the whole normaliser when one
// shape dwarfs the other, so large shapes use Stirling forms.
inline double log_beta(double a, double b) {
  const CPPUNIV_EXTENDED_REAL z = std::max(a,b), h = std::min(a,b);
  if (z < 1e6) return special::lbeta(a,b);
  const CPPUNIV_EXTENDED_REAL r = h/z, lp = std::log1p(r);
  if (h >= 1e6) {
    const CPPUNIV_EXTENDED_REAL total = z+h;
    const auto correction = [](CPPUNIV_EXTENDED_REAL v) {return (1-1/(30*v*v))/(12*v);};
    return CPPUNIV_EXTENDED_LITERAL(.5,.5L)*std::log(2*cppunivariate::pi) - CPPUNIV_EXTENDED_LITERAL(.5,.5L)*std::log(total) -
      (z-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*lp + (h-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*(std::log(r)-lp) +
      correction(z)+correction(h)-correction(total);
  }
  const CPPUNIV_EXTENDED_REAL lm = r < 1e-4 ?
    r*r*(-CPPUNIV_EXTENDED_LITERAL(.5,.5L)+r*(CPPUNIV_EXTENDED_LITERAL(1.0,1.0L)/3+r*(-CPPUNIV_EXTENDED_LITERAL(.25,.25L)+r*(CPPUNIV_EXTENDED_LITERAL(.2,.2L)-r/6)))) : lp-r;
  const CPPUNIV_EXTENDED_REAL increment = h*std::log(z)+(h-CPPUNIV_EXTENDED_LITERAL(.5,.5L))*lp+z*lm-h/(12*z*(z+h));
  return special::lgamma(static_cast<double>(h))-increment;
}

// Root of a function on the real line that is positive left of its single
// zero and negative right of it, from start t. fd(t, &f, &df) evaluates f and
// a Newton guide df < 0. Brackets by doubling steps from first_step, then
// takes Newton steps that stay inside the bracket and bisects otherwise.
// Stops when computed signs bracket the root within 2*width (a final Newton
// step inside the bracket is taken when available), when f is exactly zero,
// or when a Newton step below newton_stop is confirmed by a sign probe.
// Returns ok = false when the work budget runs out or the bracket leaves
// [lower, upper].
struct RootResult { double t; bool ok; std::int64_t evaluations; };

template <class FD>
inline RootResult decreasing_root(FD fd, double t, double width, double newton_stop,
    double lower, double upper, std::int64_t budget, double first_step = 1) {
  std::int64_t evals = 0;
  double f = 0, df = 0;
  auto eval = [&](double u) { ++evals; return fd(u, f, df); };
  if (!eval(t)) return {t, false, evals};
  if (f == 0) return {t, true, evals};
  // Bracket [lo, hi] with f(lo) > 0 > f(hi); f_lo and f_hi are those values.
  double lo, hi, f_lo, f_hi;
  {
    const bool up = f > 0;
    double step = first_step, inner = t, f_inner = f;
    for (;;) {
      if (evals >= budget) return {t, false, evals};
      const double next = up ? inner + step : inner - step;
      if (!(next > lower && next < upper)) return {t, false, evals};
      if (!eval(next)) return {t, false, evals};
      if (f == 0) return {next, true, evals};
      if ((f > 0) == up) { inner = next; f_inner = f; step *= 2; continue; }
      if (up) { lo = inner; f_lo = f_inner; hi = next; f_hi = f; }
      else { lo = next; f_lo = f; hi = inner; f_hi = f_inner; }
      t = next;
      break;
    }
  }
  const auto best_end = [&]() { return f_lo < -f_hi ? lo : hi; };
  // f and df are those at t, an endpoint of the bracket.
  while (evals < budget) {
    // Tolerances never fall below a few ulps of t.
    const double ulps = 4 * std::numeric_limits<double>::epsilon() * std::max(1.0, std::abs(t));
    const double w = std::max(width, ulps), ns = std::max(newton_stop, ulps);
    double next = t - f / df;
    const bool newton = std::isfinite(next) && next > lo && next < hi;
    if (hi - lo <= 2 * w) {
      // Certified; polish with one Newton step inside the bracket, or return
      // the endpoint with the smaller residual.
      if (newton && evals < budget) {
        double fn = 0, dfn = 0;
        ++evals;
        if (fd(next, fn, dfn) && std::abs(fn) <= std::min(f_lo, -f_hi)) return {next, true, evals};
      }
      return {best_end(), true, evals};
    }
    if (!newton) next = lo + (hi - lo) / 2;
    const double step = next - t;
    if (!eval(next)) return {t, false, evals};
    t = next;
    if (f == 0) return {t, true, evals};
    if (f > 0) { lo = t; f_lo = f; } else { hi = t; f_hi = f; }
    if (newton && std::abs(step) <= ns) {
      // Probe the far side so that computed signs bracket the root.
      const double probe = f > 0 ? t + ns : t - ns;
      double fp = 0, dfp = 0;
      ++evals;
      if (!fd(probe, fp, dfp)) return {t, false, evals};
      if (fp == 0) return {probe, true, evals};
      if (fp > 0) { if (probe > lo) { lo = probe; f_lo = fp; } }
      else if (probe < hi) { hi = probe; f_hi = fp; }
      if (hi - lo <= 2 * ns) return {best_end(), true, evals};
    }
  }
  return {t, false, evals};
}

}  // namespace beta_detail

inline BetaResult beta_mle(span x, double tol, int maxit,
    double shape1 = std::numeric_limits<double>::quiet_NaN(),
    double shape2 = std::numeric_limits<double>::quiet_NaN()) {
  maxit = static_cast<int>(solver_budget(maxit));
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0)
    return {0.0, 0.0, kNaN, 0, BetaStatus::InvalidInput};

  for (double v : x) if (!(v > 0 && v < 1 && std::isfinite(v)))
    return {kNaN,kNaN,kNaN,0,BetaStatus::InvalidInput};

  const double dn = static_cast<double>(n);
  double sum_log_x = 0.0, sum_log_1mx = 0.0;
  for (double v : x) {
    sum_log_x += std::log(v);
    sum_log_1mx += std::log1p(-v);
  }
  const auto loglik = [&](double a, double b) {
    double total=0,correction=0;
    for(double v:x) {
      const double value=special::beta_log_density_positive(v,a,b)-correction;
      const double next=total+value;correction=(next-total)-value;total=next;
    }
    return total;
  };

  if (!std::isnan(shape1) || !std::isnan(shape2)) {
    const bool fa = !std::isnan(shape1), fb = !std::isnan(shape2);
    if ((fa && (!(shape1 > 0) || !std::isfinite(shape1))) ||
        (fb && (!(shape2 > 0) || !std::isfinite(shape2))))
      return {kNaN, kNaN, kNaN, 0, BetaStatus::IterationFailure};
    double a = shape1, b = shape2;
    int it = 0;
    if (!(fa && fb)) {
      if (maxit < 1) return {kNaN,kNaN,kNaN,0,BetaStatus::IterationFailure};
      const double held = fa ? a : b;
      const double target = (fa ? sum_log_1mx : sum_log_x) / dn;
      // Strictly decreasing score, positive at zero and negative at infinity.
      const auto score = [&](double z) {
        return target + beta_detail::psi_increment(z, held);
      };
      // psi'(z) <= 1/z + 1/z^2, so for z>=1 the held-shape
      // digamma increment is <=2*held/z. This brackets even tiny log
      // complements without spending hundreds of doubling iterations.
      const double ratio=held/(-target);
      const double ceiling=std::numeric_limits<double>::max();
      double lo=0,hi=std::max(1.,ratio<ceiling/2?2*ratio:ceiling);
      for(std::int64_t bracket_pass=0; bracket_pass<solver_budget(maxit) && (score(hi) > 0 && hi < std::numeric_limits<double>::max()/2); ++bracket_pass) hi *= 2;
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
    const double ll=loglik(a,b);
    return {a,b,ll,it,std::isfinite(ll)?BetaStatus::Ok:BetaStatus::IterationFailure};
  }

  bool all_equal = true;
  for (double v : x) all_equal = all_equal && v == x[0];
  if (all_equal) return {kNaN, kNaN, kNaN, 0, BetaStatus::Degenerate};

  const double lower = std::log(std::numeric_limits<double>::min());
  const double upper = std::log(std::numeric_limits<double>::max());
  const std::int64_t budget = solver_budget(maxit);
  std::int64_t work = 0;

  // Mean statistics at mu (mc = 1 - mu, passed separately for accuracy near 1).
  double kl = 0, logit_gap = 0;
  const auto statistics = [&](double mu, double mc) {
    kl = 0; logit_gap = 0;
    for (double v : x) {
      // Above one half mu is rounded but mc and 1 - v (Sterbenz) are exact.
      const double d = mu <= .5 ? v - mu : mc - (1 - v);
      const double u = d / mu, w = -d / mc;
      // log1p loses accuracy as its argument nears -1; use the ratios there.
      const double lu = u < -.5 ? std::log(v) - std::log(mu) : std::log1p(u);
      const double lw = w < -.5 ? std::log1p(-v) - std::log(mc) : std::log1p(w);
      kl += mu * (std::abs(u) < .1 ? beta_detail::u_minus_log1p(u) : u - lu) +
            mc * (std::abs(w) < .1 ? beta_detail::u_minus_log1p(w) : w - lw);
      logit_gap += lu - lw;
    }
    kl /= dn; logit_gap /= dn;
  };

  // t_hat(mu): root in tau = log t of the ray score, warm-started.
  double t = 1;
  const auto t_hat = [&](double mu, double mc) {
    const auto fd = [&](double tau, double &f, double &df) {
      const double tt = std::exp(tau), a = mu * tt, b = mc * tt;
      f = -kl + mu * beta_detail::psi_increment_rest(a, b) +
          mc * beta_detail::psi_increment_rest(b, a);
      // d/dt of the ray score; the 1/z parts of trigamma cancel exactly.
      df = tt * (beta_detail::trigamma_minus_inv(tt) -
                 mu * mu * beta_detail::trigamma_minus_inv(a) -
                 mc * mc * beta_detail::trigamma_minus_inv(b));
      if (!(df < 0)) df = -.5 / tt;  // Newton guide only; brackets decide.
      return std::isfinite(f);
    };
    const auto root = beta_detail::decreasing_root(fd, std::log(t), 0.0, 1e-15,
        lower, upper, budget);
    work += root.evaluations;
    return root;
  };

  // Profile score in eta = logit(mu); its sign is that of dl/dmu at t_hat.
  bool inner_ok = true;
  const auto split = [](double eta, double &mu, double &mc) {
    mu = eta < 0 ? std::exp(eta) / (1 + std::exp(eta)) : 1 / (1 + std::exp(-eta));
    mc = eta > 0 ? std::exp(-eta) / (1 + std::exp(-eta)) : 1 / (1 + std::exp(eta));
  };
  const auto profile = [&](double eta, double &q, double &dq) {
    double mu, mc;
    split(eta, mu, mc);
    if (!(mu > 0 && mc > 0)) return false;
    statistics(mu, mc);
    const auto root = t_hat(mu, mc);
    if (!root.ok) { inner_ok = false; return false; }
    t = std::exp(root.t);
    const double a = mu * t, b = mc * t;
    q = logit_gap + beta_detail::psi_increment_rest(a, b) - beta_detail::psi_increment_rest(b, a);
    // Total derivative, including t_hat's dependence on eta through the ray
    // score's derivative h_tau; D1 = (1-mu) psi'(beta) - mu psi'(alpha).
    const double ca = beta_detail::trigamma_minus_inv(a), cb = beta_detail::trigamma_minus_inv(b);
    const double d1 = mc * cb - mu * ca;
    const double h_tau = t * (beta_detail::trigamma_minus_inv(t) - mu * mu * ca - mc * mc * cb);
    dq = -t * mu * mc * (special::trigamma(a) + special::trigamma(b) + d1 * (q + t * d1) / h_tau);
    if (!(dq < 0) || !std::isfinite(dq))  // Newton guide only; brackets decide.
      dq = -t * mu * mc * (special::trigamma(a) + special::trigamma(b));
    if (!(dq < 0) || !std::isfinite(dq)) dq = -1;
    return std::isfinite(q);
  };

  // Start: mu at the sample mean, t from the near-constant approximation
  // dl/dt ~ -KL + 1/(2t); both only affect speed.
  double m = 0, mc0 = 0;
  for (double v : x) { m += v; mc0 += 1 - v; }
  m /= dn; mc0 /= dn;
  const double eta0 = std::log(m) - std::log(mc0);
  {
    double mu, mc;
    split(eta0, mu, mc);
    statistics(mu, mc);
  }
  t = std::min(std::max(.5 / kl, 1e-3), 1e300);
  if (!std::isfinite(t)) t = 1;
  // The tolerance on eta comes from the requested relative accuracy of the
  // shapes. Along the profile, log t moves with eta at first order (tau') and,
  // for concentrated data, at second order with curvature about t mu (1-mu).
  // Re-solve with a narrower bracket until the bracket width meets both.
  tol = std::max(tol, 1e-14);
  double width = tol, eta = eta0, mu = 0, mc = 0;
  bool solved = false;
  for (std::int64_t pass = 0; pass < budget && !solved; ++pass) {
    const auto outer = beta_detail::decreasing_root(profile, eta, width, width,
        lower, upper, budget, pass == 0 ? 1 : 4 * width);
    if (!outer.ok || !inner_ok) break;
    eta = outer.t;
    // Re-solve t at the returned eta (the last evaluation may be a probe).
    split(eta, mu, mc);
    statistics(mu, mc);
    const auto root = t_hat(mu, mc);
    if (!root.ok) break;
    t = std::exp(root.t);
    const double a = mu * t, b = mc * t;
    const double ca = beta_detail::trigamma_minus_inv(a), cb = beta_detail::trigamma_minus_inv(b);
    const double h_tau = t * (beta_detail::trigamma_minus_inv(t) - mu * mu * ca - mc * mc * cb);
    const double tau_slope = -mu * mc * t * (mc * cb - mu * ca) / h_tau;
    double needed = std::min(tol / (1 + std::abs(tau_slope)),
                             std::sqrt(tol / (1 + t * mu * mc)));
    const double floor = 4 * std::numeric_limits<double>::epsilon() * std::max(1.0, std::abs(eta));
    if (!(needed > floor)) needed = floor;
    if (needed >= width || width <= floor) solved = true;
    else width = needed;
  }
  const int iterations = static_cast<int>(std::min<std::int64_t>(work, std::numeric_limits<int>::max()));
  if (!solved)
    return {kNaN, kNaN, kNaN, iterations, BetaStatus::IterationFailure};
  double alpha = mu * t, beta = mc * t;
  const double ll = -dn * t * kl - sum_log_x - sum_log_1mx +
      dn * (.5 * std::log(mu * mc * t / (2 * cppunivariate::pi)) +
            beta_detail::stirling_remainder(t) - beta_detail::stirling_remainder(alpha) -
            beta_detail::stirling_remainder(beta));
  if (!(alpha > 0 && std::isfinite(alpha) && beta > 0 && std::isfinite(beta) && std::isfinite(ll)))
    return {kNaN, kNaN, kNaN, iterations, BetaStatus::IterationFailure};
  // Evaluate the rounded public shapes, rather than the internal mean/precision.
  double public_ll=loglik(alpha,beta);
  const double likelihood_target=1e-10*std::max(1.0,std::abs(ll));
  const bool replace_likelihood=!std::isfinite(public_ll) ||
      std::abs(ll-public_ll)>likelihood_target;
  // At large precision, rounding the two shapes changes their ratio enough
  // to matter to logLik. Enumerate the adjacent representations of the same
  // continuous solution, without changing the root solve or its controls.
  if(std::isfinite(public_ll) && ll-public_ll>likelihood_target) {
    const double a0=alpha,b0=beta;
    for(double a:{std::nextafter(a0,0.0),a0,std::nextafter(a0,INFINITY)})
      for(double b:{std::nextafter(b0,0.0),b0,std::nextafter(b0,INFINITY)}) {
        if(!(a>0 && b>0 && std::isfinite(a) && std::isfinite(b)))continue;
        const double value=loglik(a,b);
        if(value>public_ll) {alpha=a;beta=b;public_ll=value;}
      }
  }
  // The continuous value already meets the contract on ordinary samples.
  // Keep it when the public-point check passes; replace a demonstrated miss.
  const double reported_ll=replace_likelihood?public_ll:ll;
  return {alpha,beta,reported_ll,iterations,
      std::isfinite(reported_ll)?BetaStatus::Ok:BetaStatus::IterationFailure};
}

} // namespace cppunivariate
