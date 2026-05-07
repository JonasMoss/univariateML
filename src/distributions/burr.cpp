// Burr (Type XII) distribution: arma::vec wrappers. mlburr fits only
// (shape1, shape2); the scale defaults to 1 (matches actuar::dburr's
// rate=1 -> scale=1 default path).
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/burr.hpp"

namespace dist_impl {

arma::vec dburr(const arma::vec &x, const arma::vec &shape1,
                const arma::vec &shape2, double scale = 1.0,
                bool log = false) {
  const uword n = recycle_length(x.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dburr(
        x[i % x.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pburr(const arma::vec &q, const arma::vec &shape1,
                const arma::vec &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pburr(
        q[i % q.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qburr(const arma::vec &p, const arma::vec &shape1,
                const arma::vec &shape2, double scale = 1.0,
                bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qburr(
        p[i % p.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], scale, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rburr(int n, double shape1 = 1.0, double shape2 = 1.0,
                double scale = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rburr");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rburr(rng, shape1, shape2, scale);
  }
  return out;
}

} // namespace dist_impl
