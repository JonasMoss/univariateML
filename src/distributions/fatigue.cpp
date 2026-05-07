// Birnbaum-Saunders (fatigue) distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/fatigue.hpp"

namespace dist_impl {

arma::vec dfatigue(const arma::vec &x, const arma::vec &alpha,
                   const arma::vec &beta, const arma::vec &mu,
                   bool log = false) {
  const uword n = recycle_length(x.n_elem, alpha.n_elem,
                                 std::max(beta.n_elem, mu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dfatigue(
        x[i % x.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem],
        mu[i % mu.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pfatigue(const arma::vec &q, const arma::vec &alpha,
                   const arma::vec &beta, const arma::vec &mu,
                   bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, alpha.n_elem,
                                 std::max(beta.n_elem, mu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pfatigue(
        q[i % q.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem],
        mu[i % mu.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qfatigue(const arma::vec &p, const arma::vec &alpha,
                   const arma::vec &beta, const arma::vec &mu,
                   bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, alpha.n_elem,
                                 std::max(beta.n_elem, mu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qfatigue(
        p[i % p.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem],
        mu[i % mu.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rfatigue(int n, double alpha = 1.0, double beta = 1.0,
                   double mu = 0.0) {
  if (n < 0 || !(alpha > 0.0 && beta > 0.0)) {
    Rcpp::stop("Invalid parameters for rfatigue");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rfatigue(rng, alpha, beta, mu);
  }
  return out;
}

} // namespace dist_impl
