// Inverse-Gaussian distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invgauss.hpp"

namespace dist_impl {

arma::vec dinvgauss(const arma::vec &x, const arma::vec &mu,
                    const arma::vec &phi, bool log = false) {
  const uword n = recycle_length(x.n_elem, mu.n_elem, phi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dinvgauss(
        x[i % x.n_elem], mu[i % mu.n_elem], phi[i % phi.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pinvgauss(const arma::vec &q, const arma::vec &mu,
                    const arma::vec &phi, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mu.n_elem, phi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pinvgauss(
        q[i % q.n_elem], mu[i % mu.n_elem], phi[i % phi.n_elem], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qinvgauss(const arma::vec &p, const arma::vec &mu,
                    const arma::vec &phi, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mu.n_elem, phi.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qinvgauss(
        p[i % p.n_elem], mu[i % mu.n_elem], phi[i % phi.n_elem], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rinvgauss(int n, double mu = 1.0, double phi = 1.0) {
  if (n < 0 || !(mu > 0.0 && phi > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvgauss");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rinvgauss(rng, mu, phi);
  }
  return out;
}

} // namespace dist_impl
