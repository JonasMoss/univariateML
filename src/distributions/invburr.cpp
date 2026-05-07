// Inverse Burr (Dagum) distribution: arma::vec wrappers. mlinvburr fits
// only (shape1, shape2); scale defaults to 1.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/invburr.hpp"

namespace dist_impl {

arma::vec dinvburr(const arma::vec &x, const arma::vec &shape1,
                   const arma::vec &shape2, double scale = 1.0,
                   bool log = false) {
  const uword n = recycle_length(x.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dinvburr(
        x[i % x.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pinvburr(const arma::vec &q, const arma::vec &shape1,
                   const arma::vec &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pinvburr(
        q[i % q.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qinvburr(const arma::vec &p, const arma::vec &shape1,
                   const arma::vec &shape2, double scale = 1.0,
                   bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qinvburr(
        p[i % p.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rinvburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                   double scale = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rinvburr");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rinvburr(rng, shape1, shape2, scale);
  }
  return out;
}

} // namespace dist_impl
