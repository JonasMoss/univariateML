// Logit-normal distribution: scalar d/p/q/r.
//
// X ~ LogitNormal(mu, sigma) iff logit(X) ~ Normal(mu, sigma) on (0, 1).
//
// PDF:    f(x) = phi(logit(x); mu, sigma) / (x * (1 - x))    for x in (0, 1)
// CDF:    F(q) = Phi(logit(q); mu, sigma)
// Quantile: F^{-1}(p) = logistic_cdf(qnorm(p; mu, sigma))
// Sample: X = logistic_cdf(N(mu, sigma))

#pragma once

#include "../special/constants.hpp"
#include "../special/special.hpp"
#include <cmath>

namespace mlcore {
namespace distributions {

using mlcore::special::kNaN;
using mlcore::special::kNegInf;

inline double dlogitnorm(double x, double mu, double sigma, bool log) {
  if (!(sigma > 0.0) || !std::isfinite(x)) {
    return kNaN;
  }
  if (x <= 0.0 || x >= 1.0) {
    return log ? kNegInf : 0.0;
  }
  const double ql = mlcore::special::qlogis(x, 0.0, 1.0, /*lower_tail=*/true,
                                            /*log_p=*/false);
  if (log) {
    return mlcore::special::dnorm(ql, mu, sigma, /*log=*/true) - std::log(x) -
           std::log1p(-x);
  }
  return mlcore::special::dnorm(ql, mu, sigma, /*log=*/false) /
         (x * (1.0 - x));
}

inline double plogitnorm(double q, double mu, double sigma, bool lower_tail,
                         bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(q)) {
    return kNaN;
  }
  const double ql = mlcore::special::qlogis(q, 0.0, 1.0, /*lower_tail=*/true,
                                            /*log_p=*/false);
  return mlcore::special::pnorm(ql, mu, sigma, lower_tail, log_p);
}

inline double qlogitnorm(double p, double mu, double sigma, bool lower_tail,
                         bool log_p) {
  if (!(sigma > 0.0) || !std::isfinite(p)) {
    return kNaN;
  }
  const double qn = mlcore::special::qnorm(p, mu, sigma, lower_tail, log_p);
  return mlcore::special::plogis(qn, 0.0, 1.0, /*lower_tail=*/true,
                                 /*log_p=*/false);
}

template <typename Rng>
inline double rlogitnorm(Rng &rng, double mu, double sigma) {
  const double z = mu + sigma * rng.rnorm();
  return mlcore::special::plogis(z, 0.0, 1.0, /*lower_tail=*/true,
                                 /*log_p=*/false);
}

} // namespace distributions
} // namespace mlcore
