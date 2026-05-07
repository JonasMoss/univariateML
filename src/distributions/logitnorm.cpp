// Logit-normal distribution (d/p/q/r)
#include "common.h"

namespace dist_impl {

arma::vec dlogitnorm(const arma::vec &x, const arma::vec &mu, const arma::vec &sigma, bool log = false) {
  const uword n = recycle_length(x.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    if (!(s > 0.0) || !std::isfinite(xi)) {
      out[i] = NA_REAL;
      continue;
    }
    if (xi <= 0.0 || xi >= 1.0) {
      out[i] = log ? 0.0 : 0.0;
      continue;
    }
    const double ql = mlcore::special::qlogis(xi, 0.0, 1.0, true, false);
    if (log) {
      out[i] = mlcore::special::dnorm(ql, m, s, true) - std::log(xi) - std::log1p(-xi);
    } else {
      out[i] = mlcore::special::dnorm(ql, m, s, false) / (xi * (1.0 - xi));
    }
  }
  return out;
}

arma::vec plogitnorm(const arma::vec &q, const arma::vec &mu, const arma::vec &sigma,
               bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    if (!(s > 0.0) || !std::isfinite(qi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double ql = mlcore::special::qlogis(qi, 0.0, 1.0, true, false);
    out[i] = mlcore::special::pnorm(ql, m, s, lower_tail, log_p);
  }
  return out;
}

arma::vec qlogitnorm(const arma::vec &p, const arma::vec &mu, const arma::vec &sigma,
               bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mu.n_elem, sigma.n_elem);
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double pi = p[i % p.n_elem];
    const double m = mu[i % mu.n_elem];
    const double s = sigma[i % sigma.n_elem];
    if (!(s > 0.0) || !std::isfinite(pi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double qn = mlcore::special::qnorm(pi, m, s, lower_tail, log_p);
    out[i] = mlcore::special::plogis(qn, 0.0, 1.0, true, false);
  }
  return out;
}

arma::vec rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  if (n < 0 || !(sigma > 0.0)) {
    Rcpp::stop("Invalid parameters for rlogitnorm");
  }
  arma::vec out(static_cast<uword>(n));
  for (int i = 0; i < n; ++i) {
    const double z = R::rnorm(mu, sigma);
    out[i] = mlcore::special::plogis(z, 0.0, 1.0, true, false);
  }
  return out;
}
} // namespace dist_impl
