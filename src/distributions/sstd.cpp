// Skew Student-t: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/sstd.hpp"

namespace dist_impl {

arma::vec dsstd(const arma::vec &x, const arma::vec &mean,
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
    const double y = mlcore::distributions::dsstd(xv, m, s, v, xi_val, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec psstd(const arma::vec &q, const arma::vec &mean,
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
    const double y = mlcore::distributions::psstd(qv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qsstd(const arma::vec &p, const arma::vec &mean,
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
    const double y = mlcore::distributions::qsstd(pv, m, s, v, xi_val);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rsstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0,
                double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && nu > 2.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsstd");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rsstd(rng, mean, sd, nu, xi);
  }
  return out;
}

} // namespace dist_impl
