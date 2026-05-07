// Lomax distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lomax.hpp"

namespace dist_impl {

arma::vec dlomax(const arma::vec &x, const arma::vec &lambda,
                 const arma::vec &kappa, bool log = false) {
  const uword n = recycle_length(x.n_elem, lambda.n_elem, kappa.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dlomax(
        x[i % x.n_elem], lambda[i % lambda.n_elem], kappa[i % kappa.n_elem],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec plomax(const arma::vec &q, const arma::vec &lambda,
                 const arma::vec &kappa, bool lower_tail = true,
                 bool log_p = false) {
  const uword n = recycle_length(q.n_elem, lambda.n_elem, kappa.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::plomax(
        q[i % q.n_elem], lambda[i % lambda.n_elem], kappa[i % kappa.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qlomax(const arma::vec &p, const arma::vec &lambda,
                 const arma::vec &kappa, bool lower_tail = true,
                 bool log_p = false) {
  const uword n = recycle_length(p.n_elem, lambda.n_elem, kappa.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qlomax(
        p[i % p.n_elem], lambda[i % lambda.n_elem], kappa[i % kappa.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rlomax(int n, double lambda = 1.0, double kappa = 1.0) {
  if (n < 0 || !(lambda > 0.0 && kappa > 0.0)) {
    Rcpp::stop("Invalid parameters for rlomax");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlomax(rng, lambda, kappa);
  }
  return out;
}

} // namespace dist_impl
