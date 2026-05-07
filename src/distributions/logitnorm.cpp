// Logit-normal distribution: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/logitnorm.hpp"

namespace dist_impl {

arma::vec dlogitnorm(const arma::vec &x, const arma::vec &mu,
                     const arma::vec &sigma, bool log = false) {
  const uword n = recycle_length(x.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    const double y = mlcore::distributions::dlogitnorm(xi, m, s, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec plogitnorm(const arma::vec &q, const arma::vec &mu,
                     const arma::vec &sigma, bool lower_tail = true,
                     bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    const double y =
        mlcore::distributions::plogitnorm(qi, m, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qlogitnorm(const arma::vec &p, const arma::vec &mu,
                     const arma::vec &sigma, bool lower_tail = true,
                     bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    const double y =
        mlcore::distributions::qlogitnorm(pi, m, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rlogitnorm");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlogitnorm(rng, mu, sigma);
  }
  return out;
}

} // namespace dist_impl
