// Zero-Inflated Poisson MLE. Closed-form via Lambert W: when the empirical
// zero-rate exceeds exp(-mean), the principal branch W_0 yields the MLE.

#pragma once

#include "../../core/lambert_w.hpp"
#include "../../core/span.hpp"
#include "../../special/special.hpp"

#include <cmath>
#include <cstddef>
#include <limits>

namespace cppunivariate {

struct ZipResult {
  double lambda;
  double pi;
  double logLik;
};

// Negative log-likelihood of ZeroInflatedPoisson(lambda, pi):
//   For x = 0: f = pi + (1-pi) * exp(-lambda)
//   For x > 0: f = (1-pi) * exp(-lambda) * lambda^x / x!
// Per-obs Hessian splits by regime (sympy, with D := pi + (1-pi)*exp(-lambda)
// for the x=0 contribution):
//   x = 0:  H_ll/obs = -pi*(1-pi)*exp(lambda) / D^2  (sign flips for nll)
//                    = pi*(1-pi)*exp(lambda) / D^2  after negation
//                  Wait: sympy gave -d2 log f / d lam d lam =
//                    pi*(pi-1)*exp(lam)/D^2 = -pi*(1-pi)*exp(lam)/D^2.
//                  So H/nll = -d2logf = +pi*(1-pi)*exp(lam)/D^2  ... no.
//                  Let me re-check below.
//   x = 0:  H_ll = pi*(pi-1)*exp(lam)/D^2  (per-obs, NEGATIVE — non-standard)
//           H_pp = (1 - exp(lam))^2 / D^2
//           H_lp = -exp(lam) / D^2
//   x >= 1: H_ll = x/lam^2
//           H_pp = 1/(1-pi)^2
//           H_lp = 0
struct ZipCost {
  ispan x;
  int dim() const { return 2; }
  double operator()(const double *theta) const {
    const double lam = theta[0];
    const double pp = theta[1];
    if (!(lam > 0.0 && pp >= 0.0 && pp < 1.0))
      return std::numeric_limits<double>::infinity();
    double sum = 0.0;
    const double e_neg_lam = std::exp(-lam);
    const double D = pp + (1.0 - pp) * e_neg_lam;
    for (auto v : x) {
      if (v == 0) {
        sum += -std::log(D);
      } else {
        const double xd = static_cast<double>(v);
        sum += -std::log(1.0 - pp) + lam - xd * std::log(lam) +
               cppunivariate::special::lgamma(xd + 1.0);
      }
    }
    return sum;
  }
  void gradient(const double *theta, double *g) const {
    const double lam = theta[0];
    const double pp = theta[1];
    const double e_neg_lam = std::exp(-lam);
    const double D = pp + (1.0 - pp) * e_neg_lam;
    double dnll_dlam = 0.0, dnll_dpi = 0.0;
    for (auto v : x) {
      if (v == 0) {
        // d log f / d lam = -(1-pi)*exp(-lam) / D
        // d log f / d pi  = (1 - exp(-lam)) / D
        dnll_dlam += (1.0 - pp) * e_neg_lam / D;
        dnll_dpi += -(1.0 - e_neg_lam) / D;
      } else {
        const double xd = static_cast<double>(v);
        // d log f / d lam = -1 + x/lam
        // d log f / d pi  = -1/(1-pi)
        dnll_dlam += 1.0 - xd / lam;
        dnll_dpi += 1.0 / (1.0 - pp);
      }
    }
    g[0] = dnll_dlam;
    g[1] = dnll_dpi;
  }
  void hessian(const double *theta, double *H) const {
    const double lam = theta[0];
    const double pp = theta[1];
    const double e_lam = std::exp(lam);
    const double D = pp * e_lam + (1.0 - pp);  // D * e_lam
    const double D2 = D * D;
    double H_ll = 0.0, H_pp = 0.0, H_lp = 0.0;
    for (auto v : x) {
      if (v == 0) {
        // From sympy: -d2 log f / d lam^2 = pi*(pi-1)*exp(lam)/D^2
        //                                  = -pi*(1-pi)*e_lam / D^2
        // (D = pi*e_lam + (1-pi))
        H_ll += pp * (pp - 1.0) * e_lam / D2;
        H_pp += (1.0 - e_lam) * (1.0 - e_lam) / D2;
        H_lp += -e_lam / D2;
      } else {
        const double xd = static_cast<double>(v);
        H_ll += xd / (lam * lam);
        H_pp += 1.0 / ((1.0 - pp) * (1.0 - pp));
        // H_lp += 0
      }
    }
    H[0] = H_ll;
    H[1] = H_lp;
    H[2] = H_lp;
    H[3] = H_pp;
  }
};

inline ZipResult zip_mle(ispan x) {
  constexpr double kNaN = std::numeric_limits<double>::quiet_NaN();
  const std::size_t n = x.size();
  if (n == 0) return {0.0, 0.0, kNaN};

  double sum_zero = 0.0, sum_x = 0.0;
  for (auto v : x) {
    if (v == 0) sum_zero += 1.0;
    sum_x += static_cast<double>(v);
  }
  const double dn = static_cast<double>(n);
  const double r0 = sum_zero / dn;
  const double x_bar = sum_x / dn;

  double lambda = 0.0;
  double pi = 0.0;
  if (r0 == 1.0) {
    lambda = 1.0;
    pi = 1.0;
  } else if (r0 > std::exp(-x_bar)) {
    const double gamma = x_bar / (1.0 - r0);
    const double z = -gamma * std::exp(-gamma);
    const double w = lambertw0(z);
    lambda = w + gamma;
    pi = (r0 - std::exp(-lambda)) / (1.0 - std::exp(-lambda));
  } else {
    pi = 0.0;
    lambda = x_bar;
  }

  double logLik = 0.0;
  for (auto v : x) {
    if (v == 0) {
      logLik += std::log(pi + (1.0 - pi) * std::exp(-lambda));
    } else {
      logLik += std::log(1.0 - pi) +
                static_cast<double>(v) * std::log(lambda) - lambda -
                std::lgamma(static_cast<double>(v) + 1.0);
    }
  }
  return {lambda, pi, logLik};
}

}  // namespace cppunivariate
