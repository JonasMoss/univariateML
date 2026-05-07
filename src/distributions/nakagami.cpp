// Nakagami distribution: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/nakagami.hpp"

namespace dist_impl {

arma::vec dnaka(const arma::vec &x, const arma::vec &shape,
                const arma::vec &scale, bool log = false) {
  const uword n = recycle_length(x.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    const double y = mlcore::distributions::dnaka(xi, k, s, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pnaka(const arma::vec &q, const arma::vec &shape,
                const arma::vec &scale, bool lower_tail = true,
                bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    const double y =
        mlcore::distributions::pnaka(qi, k, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qnaka(const arma::vec &p, const arma::vec &shape,
                const arma::vec &scale, bool lower_tail = true,
                bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    const double y =
        mlcore::distributions::qnaka(pi, k, s, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rnaka(int n, double shape, double scale) {
  if (n < 0 || !(shape > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rnaka");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rnaka(rng, shape, scale);
  }
  return out;
}

} // namespace dist_impl
