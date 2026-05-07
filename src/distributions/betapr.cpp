// Beta-prime distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/betapr.hpp"

namespace dist_impl {

arma::vec dbetapr(const arma::vec &x, const arma::vec &shape1,
                  const arma::vec &shape2, bool log = false) {
  const uword n = recycle_length(x.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dbetapr(
        x[i % x.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pbetapr(const arma::vec &q, const arma::vec &shape1,
                  const arma::vec &shape2, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pbetapr(
        q[i % q.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qbetapr(const arma::vec &p, const arma::vec &shape1,
                  const arma::vec &shape2, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape1.n_elem, shape2.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qbetapr(
        p[i % p.n_elem], shape1[i % shape1.n_elem],
        shape2[i % shape2.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rbetapr(int n, double shape1 = 1.0, double shape2 = 1.0) {
  if (n < 0 || !(shape1 > 0.0 && shape2 > 0.0)) {
    Rcpp::stop("Invalid parameters for rbetapr");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rbetapr(rng, shape1, shape2);
  }
  return out;
}

} // namespace dist_impl
