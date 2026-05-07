// Standardised Student-t: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/std.hpp"

namespace dist_impl {

arma::vec dstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd,
               const arma::vec &nu, bool log = false) {
  const uword n = recycle_length(x.n_elem, mean.n_elem,
                                 std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double y = mlcore::distributions::dstd(xi, m, s, v, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd,
               const arma::vec &nu, bool lower_tail = true,
               bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mean.n_elem,
                                 std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double y =
        mlcore::distributions::pstd(qi, m, s, v, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd,
               const arma::vec &nu, bool lower_tail = true,
               bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mean.n_elem,
                                 std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double y =
        mlcore::distributions::qstd(pi, m, s, v, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  if (n < 0 || !(sd > 0.0 && nu > 2.0)) {
    Rcpp::stop("Invalid parameters for rstd");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rstd(rng, mean, sd, nu);
  }
  return out;
}

} // namespace dist_impl
