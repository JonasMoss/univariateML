// Logarithmic-series distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lgser.hpp"

namespace dist_impl {

arma::vec dlgser(const arma::vec &x, const arma::vec &theta,
                 bool log = false) {
  const uword n = recycle_length(x.n_elem, theta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dlgser(
        x[i % x.n_elem], theta[i % theta.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec plgser(const arma::vec &q, const arma::vec &theta,
                 bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, theta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::plgser(
        q[i % q.n_elem], theta[i % theta.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qlgser(const arma::vec &p, const arma::vec &theta,
                 bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, theta.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qlgser(
        p[i % p.n_elem], theta[i % theta.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rlgser(int n, double theta) {
  if (n < 0 || !(theta > 0.0 && theta < 1.0)) {
    Rcpp::stop("Invalid parameters for rlgser");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlgser(rng, theta);
  }
  return out;
}

} // namespace dist_impl
