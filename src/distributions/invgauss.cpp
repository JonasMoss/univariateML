// Inverse-Gaussian distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invgauss.hpp"

namespace dist_impl {

arma::vec dinvgauss(const arma::vec &x, const arma::vec &mean,
                    const arma::vec &shape, bool log = false) {
  const uword n = recycle_length(x.n_elem, mean.n_elem, shape.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dinvgauss(
        x[i % x.n_elem], mean[i % mean.n_elem], shape[i % shape.n_elem],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pinvgauss(const arma::vec &q, const arma::vec &mean,
                    const arma::vec &shape, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mean.n_elem, shape.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pinvgauss(
        q[i % q.n_elem], mean[i % mean.n_elem], shape[i % shape.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qinvgauss(const arma::vec &p, const arma::vec &mean,
                    const arma::vec &shape, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mean.n_elem, shape.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qinvgauss(
        p[i % p.n_elem], mean[i % mean.n_elem], shape[i % shape.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rinvgauss(int n, double mean = 1.0, double shape = 1.0) {
  if (n < 0 || !(mean > 0.0 && shape > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvgauss");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rinvgauss(rng, mean, shape);
  }
  return out;
}

} // namespace dist_impl
