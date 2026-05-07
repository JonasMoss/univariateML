// Skew GED: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/sged.hpp"

namespace dist_impl {

arma::vec dsged(const arma::vec &x, const arma::vec &mean,
                const arma::vec &sd, const arma::vec &nu, const arma::vec &xi,
                bool log = false) {
  const uword n = recycle_length(
      x.n_elem, mean.n_elem,
      std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xv = x[i % x.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double xi_val = xi[i % xi.n_elem];
    const double y = mlcore::distributions::dsged(xv, m, s, v, xi_val, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec psged(const arma::vec &q, const arma::vec &mean,
                const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  const uword n = recycle_length(
      q.n_elem, mean.n_elem,
      std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qv = q[i % q.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double xi_val = xi[i % xi.n_elem];
    const double y = mlcore::distributions::psged(qv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qsged(const arma::vec &p, const arma::vec &mean,
                const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  const uword n = recycle_length(
      p.n_elem, mean.n_elem,
      std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pv = p[i % p.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double xi_val = xi[i % xi.n_elem];
    const double y = mlcore::distributions::qsged(pv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rsged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0,
                double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && nu > 0.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsged");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rsged(rng, mean, sd, nu, xi);
  }
  return out;
}

} // namespace dist_impl
