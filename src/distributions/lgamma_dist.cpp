// Loggamma distribution (the *distribution*, not the function): arma::vec
// wrappers. Exposed as dlgamma/plgamma/qlgamma/rlgamma to match actuar.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/lgamma_dist.hpp"

namespace dist_impl {

arma::vec dlgamma(const arma::vec &x, const arma::vec &shapelog,
                  const arma::vec &ratelog, bool log = false) {
  const uword n = recycle_length(x.n_elem, shapelog.n_elem, ratelog.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::dlgamma(
        x[i % x.n_elem], shapelog[i % shapelog.n_elem],
        ratelog[i % ratelog.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec plgamma(const arma::vec &q, const arma::vec &shapelog,
                  const arma::vec &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shapelog.n_elem, ratelog.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::plgamma(
        q[i % q.n_elem], shapelog[i % shapelog.n_elem],
        ratelog[i % ratelog.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qlgamma(const arma::vec &p, const arma::vec &shapelog,
                  const arma::vec &ratelog, bool lower_tail = true,
                  bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shapelog.n_elem, ratelog.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qlgamma(
        p[i % p.n_elem], shapelog[i % shapelog.n_elem],
        ratelog[i % ratelog.n_elem], lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rlgamma(int n, double shapelog, double ratelog) {
  if (n < 0 || !(shapelog > 0.0 && ratelog > 0.0)) {
    Rcpp::stop("Invalid parameters for rlgamma");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rlgamma(rng, shapelog, ratelog);
  }
  return out;
}

} // namespace dist_impl
