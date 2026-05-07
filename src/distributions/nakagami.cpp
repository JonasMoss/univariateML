// Nakagami distribution (d/p/q/r)
#include "common.h"

namespace dist_impl {

arma::vec dnaka(const arma::vec &x, const arma::vec &shape, const arma::vec &scale, bool log = false) {
  const uword n = recycle_length(x.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    if (!(k > 0.0 && s > 0.0) || xi < 0.0 || !std::isfinite(xi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double rate = k / s;
    const double gamma_scale = 1.0 / rate;
    const double x2 = xi * xi;
    if (log) {
      out[i] = std::log(xi) + std::log(2.0) +
               mlcore::special::dgamma(x2, k, gamma_scale, /*log=*/true);
    } else {
      out[i] =
          2.0 * xi * mlcore::special::dgamma(x2, k, gamma_scale, /*log=*/false);
    }
  }
  return out;
}

arma::vec pnaka(const arma::vec &q, const arma::vec &shape, const arma::vec &scale,
          bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    if (!(k > 0.0 && s > 0.0) || qi < 0.0 || !std::isfinite(qi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double rate = k / s;
    const double gamma_scale = 1.0 / rate;
    out[i] = mlcore::special::pgamma(qi * qi, k, gamma_scale, lower_tail, log_p);
  }
  return out;
}

arma::vec qnaka(const arma::vec &p, const arma::vec &shape, const arma::vec &scale,
          bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, shape.n_elem, scale.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double k = shape[i % shape.n_elem];
    const double s = scale[i % scale.n_elem];
    if (!(k > 0.0 && s > 0.0) || !std::isfinite(pi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double rate = k / s;
    const double gamma_scale = 1.0 / rate;
    const double gq =
        mlcore::special::qgamma(pi, k, gamma_scale, lower_tail, log_p);
    out[i] = std::sqrt(gq);
  }
  return out;
}

arma::vec rnaka(int n, double shape, double scale) {
  if (n < 0 || !(shape > 0.0 && scale > 0.0)) {
    Rcpp::stop("Invalid parameters for rnaka");
  }
  arma::vec out(static_cast<uword>(n));
  const double rate = shape / scale;
  const double gamma_scale = 1.0 / rate;
  for (int i = 0; i < n; ++i) {
    out[i] = std::sqrt(R::rgamma(shape, gamma_scale));
  }
  return out;
}
} // namespace dist_impl
