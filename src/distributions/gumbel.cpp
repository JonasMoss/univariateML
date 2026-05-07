// Gumbel distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/gumbel.hpp"

namespace dist_impl {

arma::vec dgumbel(const arma::vec &x, const arma::vec &mu,
                  const arma::vec &sigma, bool log = false) {
  const uword n = recycle_length(x.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dgumbel(
        x[i % x.n_elem], mu[i % mu.n_elem], sigma[i % sigma.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pgumbel(const arma::vec &q, const arma::vec &mu,
                  const arma::vec &sigma, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pgumbel(
        q[i % q.n_elem], mu[i % mu.n_elem], sigma[i % sigma.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qgumbel(const arma::vec &p, const arma::vec &mu,
                  const arma::vec &sigma, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qgumbel(
        p[i % p.n_elem], mu[i % mu.n_elem], sigma[i % sigma.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rgumbel(int n, double mu = 0.0, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rgumbel");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rgumbel(rng, mu, sigma);
  }
  return out;
}

} // namespace dist_impl
