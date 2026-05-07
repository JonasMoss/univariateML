// Logit-normal distribution: Rcpp::NumericVector wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/logitnorm.hpp"

namespace dist_impl {

Rcpp::NumericVector dlogitnorm(const Rcpp::NumericVector &x, const Rcpp::NumericVector &mu,
                     const Rcpp::NumericVector &sigma, bool log = false) {
  const std::size_t n = recycle_length(x.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xi = x[i % x.size()];
    const double m = mu[i % mu.size()];
    const double s = sigma[i % sigma.size()];
    const double y = mlcore::distributions::dlogitnorm(xi, m, s, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector plogitnorm(const Rcpp::NumericVector &q, const Rcpp::NumericVector &mu,
                     const Rcpp::NumericVector &sigma, bool lower_tail = true,
                     bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qi = q[i % q.size()];
    const double m = mu[i % mu.size()];
    const double s = sigma[i % sigma.size()];
    const double y =
        mlcore::distributions::plogitnorm(qi, m, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qlogitnorm(const Rcpp::NumericVector &p, const Rcpp::NumericVector &mu,
                     const Rcpp::NumericVector &sigma, bool lower_tail = true,
                     bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), mu.size(), sigma.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pi = p[i % p.size()];
    const double m = mu[i % mu.size()];
    const double s = sigma[i % sigma.size()];
    const double y =
        mlcore::distributions::qlogitnorm(pi, m, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rlogitnorm");
  }
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlogitnorm(rng, mu, sigma);
  }
  return out;
}

} // namespace dist_impl
