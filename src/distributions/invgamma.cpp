// Inverse-Gamma distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invgamma.hpp"

namespace dist_impl {

arma::vec dinvgamma(const arma::vec &x, const arma::vec &alpha,
                    const arma::vec &beta, bool log = false) {
  const uword n = recycle_length(x.n_elem, alpha.n_elem, beta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dinvgamma(
        x[i % x.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pinvgamma(const arma::vec &q, const arma::vec &alpha,
                    const arma::vec &beta, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(q.n_elem, alpha.n_elem, beta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pinvgamma(
        q[i % q.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qinvgamma(const arma::vec &p, const arma::vec &alpha,
                    const arma::vec &beta, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(p.n_elem, alpha.n_elem, beta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qinvgamma(
        p[i % p.n_elem], alpha[i % alpha.n_elem], beta[i % beta.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rinvgamma(int n, double alpha = 1.0, double beta = 1.0) {
  if (n < 0 || !(alpha > 0.0 && beta > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvgamma");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rinvgamma(rng, alpha, beta);
  }
  return out;
}

} // namespace dist_impl
