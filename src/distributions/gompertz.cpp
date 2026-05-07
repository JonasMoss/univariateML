// Gompertz distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/gompertz.hpp"

namespace dist_impl {

arma::vec dgompertz(const arma::vec &x, const arma::vec &a,
                    const arma::vec &b, bool log = false) {
  const uword n = recycle_length(x.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dgompertz(
        x[i % x.n_elem], a[i % a.n_elem], b[i % b.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pgompertz(const arma::vec &q, const arma::vec &a,
                    const arma::vec &b, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(q.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pgompertz(
        q[i % q.n_elem], a[i % a.n_elem], b[i % b.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qgompertz(const arma::vec &p, const arma::vec &a,
                    const arma::vec &b, bool lower_tail = true,
                    bool log_p = false) {
  const uword n = recycle_length(p.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qgompertz(
        p[i % p.n_elem], a[i % a.n_elem], b[i % b.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rgompertz(int n, double a = 1.0, double b = 1.0) {
  if (n < 0 || !(a > 0.0 && b > 0.0)) {
    Rcpp::stop("Invalid parameters for rgompertz");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rgompertz(rng, a, b);
  }
  return out;
}

} // namespace dist_impl
