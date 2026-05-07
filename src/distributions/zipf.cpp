// Zipf distribution: arma::vec wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/zipf.hpp"

namespace dist_impl {

arma::vec dzipf(const arma::vec &x, const arma::vec &N, const arma::vec &s,
                bool log = false) {
  const uword n = recycle_length(x.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    const double y = mlcore::distributions::dzipf(xi, Ni, si, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec pzipf(const arma::vec &q, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    const double y =
        mlcore::distributions::pzipf(qi, Ni, si, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec qzipf(const arma::vec &p, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, N.n_elem, s.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double Ni = N[i % N.n_elem];
    const double si = s[i % s.n_elem];
    const double y =
        mlcore::distributions::qzipf(pi, Ni, si, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

arma::vec rzipf(int n, double N, double s) {
  if (n < 0 || !(N > 0.0 && s > 0.0) || N != std::floor(N)) {
    Rcpp::stop("Invalid parameters for rzipf");
  }
  const int Nin = static_cast<int>(N);
  const auto cum = mlcore::distributions::zipf_cdf(Nin, s);
  arma::vec out(static_cast<uword>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] =
        static_cast<double>(mlcore::distributions::rzipf_from_cdf(rng, cum));
  }
  return out;
}

} // namespace dist_impl
