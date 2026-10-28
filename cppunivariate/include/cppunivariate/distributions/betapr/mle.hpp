// Three-parameter beta-prime MLE, with explicit likelihood comparisons to faces.
#pragma once
#include "../../core/bfgs.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"
#include "../gamma/mle.hpp"
#include "../invgamma/mle.hpp"
#include <cmath>
#include <cstddef>
#include <limits>
namespace cppunivariate {
enum class BetaPrimeStatus { Ok, BfgsFailure, InvalidStart,
                             GammaBoundary, InvGammaBoundary };
struct BetaPrimeResult {
  double alpha, beta, scale, logLik;
  int iterations;
  BetaPrimeStatus status;
};
// Negative log-likelihood in the two shapes at a supplied scale.
// Default scale 1 also preserves the legacy validation-harness seam.
// Hessian (closed form, data-independent — same shape as Beta on the
// transformed variable):
//   H_aa = n*(trigamma(a) - trigamma(a+b))
//   H_bb = n*(trigamma(b) - trigamma(a+b))
//   H_ab = -n*trigamma(a+b)
struct BetaPrimeCost {
  span x;
  double scale = 1.0;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double a = theta[0];
    const double b = theta[1];
    if (!(a > 0.0 && b > 0.0)) return std::numeric_limits<double>::infinity();
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) {
      if (v <= 0.0) return std::numeric_limits<double>::infinity();
      S_lx += std::log(v / scale);
      S_l1px += std::log1p(v / scale);
    }
    return n * std::log(scale) - (a - 1.0) * S_lx + (a + b) * S_l1px +
           n * (cppunivariate::special::lgamma(a) + cppunivariate::special::lgamma(b) -
                cppunivariate::special::lgamma(a + b));
  }
  void gradient(const double *theta, double *g) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    double S_lx = 0.0, S_l1px = 0.0;
    for (auto v : x) { S_lx += std::log(v / scale); S_l1px += std::log1p(v / scale); }
    const double psi_ab = cppunivariate::special::digamma(a + b);
    g[0] = -S_lx + S_l1px + n * (cppunivariate::special::digamma(a) - psi_ab);
    g[1] = S_l1px + n * (cppunivariate::special::digamma(b) - psi_ab);
  }
  void hessian(const double *theta, double *H) const {
    const double a = theta[0];
    const double b = theta[1];
    const double n = static_cast<double>(x.size());
    const double psi1_ab = cppunivariate::special::trigamma(a + b);
    H[0] = n * (cppunivariate::special::trigamma(a) - psi1_ab);
    H[1] = -n * psi1_ab;
    H[2] = -n * psi1_ab;
    H[3] = n * (cppunivariate::special::trigamma(b) - psi1_ab);
  }
};


// Log coordinates; the fixed-scale shape cost is also the production objective.
struct BetaPrimeLogCost {
  span x;
  int dim() const { return 3; }
  double operator()(const double *phi) const {
    const double a = std::exp(phi[0]), b = std::exp(phi[1]);
    const double scale = std::exp(phi[2]);
    // Avoid cancellation at numerically indistinguishable infinite-shape faces.
    if (!(a > 0 && b > 0 && scale > 0) || !std::isfinite(scale) ||
        a > 1e6 || b > 1e6) return INFINITY;
    const double theta[2] = {a, b};
    return BetaPrimeCost{x, scale}(theta);
  }
  void gradient(const double *phi, double *g) const {
    const double a = std::exp(phi[0]), b = std::exp(phi[1]);
    const double scale = std::exp(phi[2]);
    const double theta[2] = {a, b};
    BetaPrimeCost{x, scale}.gradient(theta, g);
    g[0] *= a;
    g[1] *= b;
    g[2] = 0;
    for (double v : x) g[2] += a - (a + b) * (v / (scale + v));
  }
};

inline BetaPrimeResult betapr_mle(span x, double tol = 1e-6, int maxit = 1000,
                                  double shape1 = 0.0, double shape2 = 0.0,
                                  double scale = 0.0, double reltol = 1e-13,
                                  double abstol = 1e-10) {
  const double nan = std::numeric_limits<double>::quiet_NaN();
  if (shape1 < 0 || shape2 < 0 || scale < 0 ||
      !std::isfinite(shape1) || !std::isfinite(shape2) || !std::isfinite(scale) ||
      x.size() < 2) return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  double lm = 0.0;
  for (double v : x) {
    if (!(v > 0) || !std::isfinite(v))
      return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
    lm += std::log(v) / static_cast<double>(x.size());
  }
  double lv = 0;
  for (double v : x) lv += (std::log(v)-lm)*(std::log(v)-lm) / x.size();
  if (!(lv > 0) || !std::isfinite(lv))
    return {nan,nan,nan,nan,0,BetaPrimeStatus::InvalidStart};
  const double n = static_cast<double>(x.size());
  auto gamma = gamma_mle(x, 1e-10, 1000);
  auto inv = invgamma_mle(x, 1e-10, 1000);
  // The closure's faces: shape2 -> inf (gamma) and shape1 -> inf (inverse
  // gamma). Both shapes -> inf is not a lognormal: Var(log X) =
  // trigamma(shape1) + trigamma(shape2) -> 0, a point mass.
  BetaPrimeResult face{gamma.shape,INFINITY,INFINITY,
      gamma.logLik,0,BetaPrimeStatus::GammaBoundary};
  if (inv.logLik > face.logLik) face = {INFINITY,inv.shape,0,
      inv.logLik,0,BetaPrimeStatus::InvGammaBoundary};
  BetaPrimeLogCost cost{x};
  BfgsControl ctl;
  ctl.grad_tol = tol; ctl.maxit = maxit; ctl.reltol = reltol; ctl.step_tol = abstol;
  BfgsResult best;
  best.f = INFINITY;
  for (double a : {0.2, 1.0, 5.0, 20.0, 100.0}) {
    for (double b : {0.2, 1.0, 5.0, 20.0, 100.0}) {
      const double start[3] = {std::log(a),std::log(b),
          lm - special::digamma(a) + special::digamma(b)};
      auto r = bfgs_minimize(cost, start, ctl);
      if (std::isfinite(r.f) && r.f < best.f) best = r;
    }
  }
  if (shape1 > 0 || shape2 > 0 || scale > 0) {
    const double a = shape1 > 0 ? shape1 : 2.0;
    const double b = shape2 > 0 ? shape2 : 2.0;
    const double ls = scale > 0 ? std::log(scale) :
        lm - special::digamma(a) + special::digamma(b);
    const double start[3] = {std::log(a),std::log(b),ls};
    auto r = bfgs_minimize(cost, start, ctl);
    if (std::isfinite(r.f) && r.f < best.f) best = r;
  }
  if (face.logLik >= -best.f - 1e-8*n) return face;
  if (!std::isfinite(best.f)) return {nan,nan,nan,nan,0,BetaPrimeStatus::BfgsFailure};
  return {std::exp(best.theta[0]),std::exp(best.theta[1]),std::exp(best.theta[2]),
          -best.f,best.iterations,best.status == BfgsStatus::Ok ?
          BetaPrimeStatus::Ok : BetaPrimeStatus::BfgsFailure};
}
} // namespace cppunivariate
