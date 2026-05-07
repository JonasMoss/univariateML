// Rayleigh distribution: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/rayleigh.hpp"

namespace dist_impl {

arma::vec drayleigh(const arma::vec &x, const arma::vec &sigma,
                    bool log = false) {
  const uword n = recycle_length(x.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::drayleigh(
        x[i % x.n_elem], sigma[i % sigma.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec prayleigh(const arma::vec &q, const arma::vec &sigma,
                    bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::prayleigh(
        q[i % q.n_elem], sigma[i % sigma.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qrayleigh(const arma::vec &p, const arma::vec &sigma,
                    bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qrayleigh(
        p[i % p.n_elem], sigma[i % sigma.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rrayleigh(int n, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rrayleigh");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rrayleigh(rng, sigma);
  }
  return out;
}

} // namespace dist_impl
