// Kumaraswamy distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/kumar.hpp"

namespace dist_impl {

arma::vec dkumar(const arma::vec &x, const arma::vec &a, const arma::vec &b,
                 bool log = false) {
  const uword n = recycle_length(x.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dkumar(
        x[i % x.n_elem], a[i % a.n_elem], b[i % b.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pkumar(const arma::vec &q, const arma::vec &a, const arma::vec &b,
                 bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pkumar(
        q[i % q.n_elem], a[i % a.n_elem], b[i % b.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qkumar(const arma::vec &p, const arma::vec &a, const arma::vec &b,
                 bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, a.n_elem, b.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qkumar(
        p[i % p.n_elem], a[i % a.n_elem], b[i % b.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rkumar(int n, double a = 1.0, double b = 1.0) {
  if (n < 0 || !(a > 0.0 && b > 0.0)) {
    Rcpp::stop("Invalid parameters for rkumar");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rkumar(rng, a, b);
  }
  return out;
}

} // namespace dist_impl
