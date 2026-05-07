// Discrete uniform distribution: arma::vec wrappers.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/dunif.hpp"

namespace dist_impl {

arma::vec ddunif(const arma::vec &x, const arma::vec &min,
                 const arma::vec &max, bool log = false) {
  const uword n = recycle_length(x.n_elem, min.n_elem, max.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::ddunif(
        x[i % x.n_elem], min[i % min.n_elem], max[i % max.n_elem], log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pdunif(const arma::vec &q, const arma::vec &min,
                 const arma::vec &max, bool lower_tail = true,
                 bool log_p = false) {
  const uword n = recycle_length(q.n_elem, min.n_elem, max.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::pdunif(
        q[i % q.n_elem], min[i % min.n_elem], max[i % max.n_elem], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qdunif(const arma::vec &p, const arma::vec &min,
                 const arma::vec &max, bool lower_tail = true,
                 bool log_p = false) {
  const uword n = recycle_length(p.n_elem, min.n_elem, max.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double y = mlcore::distributions::qdunif(
        p[i % p.n_elem], min[i % min.n_elem], max[i % max.n_elem], lower_tail,
        log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rdunif(int n, double min, double max) {
  if (n < 0 || min > max || min != std::floor(min) ||
      max != std::floor(max)) {
    Rcpp::stop("Invalid parameters for rdunif");
  }
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] = mlcore::distributions::rdunif(rng, min, max);
  }
  return out;
}

} // namespace dist_impl
