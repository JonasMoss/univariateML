// Inverse-Weibull (Frechet) distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invweibull.hpp"

namespace dist_impl {

arma::vec dinvweibull(const arma::vec &x, const arma::vec &shape,
                      const arma::vec &scale, bool log = false) {
  const uword n = recycle_length(x.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dinvweibull(
        x[i % x.n_elem], shape[i % shape.n_elem], scale[i % scale.n_elem],
        log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pinvweibull(const arma::vec &q, const arma::vec &shape,
                      const arma::vec &scale, bool lower_tail = true,
                      bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pinvweibull(
        q[i % q.n_elem], shape[i % shape.n_elem], scale[i % scale.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qinvweibull(const arma::vec &p, const arma::vec &shape,
                      const arma::vec &scale, bool lower_tail = true,
                      bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qinvweibull(
        p[i % p.n_elem], shape[i % shape.n_elem], scale[i % scale.n_elem],
        lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rinvweibull(int n, double shape, double scale) {
  if (n < 0 || !(shape > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvweibull");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rinvweibull(rng, shape, scale);
  }
  return out;
}

} // namespace dist_impl
