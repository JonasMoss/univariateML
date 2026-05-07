// Zero-inflated Poisson distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/zip.hpp"

namespace dist_impl {

arma::vec dzip(const arma::vec &x, const arma::vec &lambda,
               const arma::vec &pi, bool log = false) {
  const uword n = recycle_length(x.n_elem, lambda.n_elem, pi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dzip(
        x[i % x.n_elem], lambda[i % lambda.n_elem], pi[i % pi.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pzip(const arma::vec &q, const arma::vec &lambda,
               const arma::vec &pi, bool lower_tail = true,
               bool log_p = false) {
  const uword n = recycle_length(q.n_elem, lambda.n_elem, pi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pzip(
        q[i % q.n_elem], lambda[i % lambda.n_elem], pi[i % pi.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qzip(const arma::vec &p, const arma::vec &lambda,
               const arma::vec &pi, bool lower_tail = true,
               bool log_p = false) {
  const uword n = recycle_length(p.n_elem, lambda.n_elem, pi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qzip(
        p[i % p.n_elem], lambda[i % lambda.n_elem], pi[i % pi.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rzip(int n, double lambda, double pi) {
  if (n < 0 || !(lambda > 0.0) || !(pi >= 0.0 && pi <= 1.0)) {
    Rcpp::stop("Invalid parameters for rzip");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rzip(rng, lambda, pi);
  }
  return out;
}

} // namespace dist_impl
