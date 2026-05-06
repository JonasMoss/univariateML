// GED (symmetric) distribution dpqr
#include "common.h"

namespace dist_impl {

arma::vec dged(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool log = false) {
  const uword n = recycle_length(x.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi = x[i % x.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    if (!(s > 0.0 && v > 0.0) || !std::isfinite(xi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double z = (xi - m) / s;
    const double lambda = ged_lambda(v);
    const double g = ged_g(v, lambda);
    const double expo = -0.5 * std::pow(std::fabs(z / lambda), v);
    if (log) {
      out[i] = std::log(g) + expo - std::log(s);
    } else {
      out[i] = g * std::exp(expo) / s;
    }
  }
  return out;
}

arma::vec pged(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double qi = q[i % q.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    if (!(s > 0.0 && v > 0.0) || !std::isfinite(qi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double z = (qi - m) / s;
    const double lambda = ged_lambda(v);
    const double g = ged_g(v, lambda);
    const double h =
        std::pow(2.0, 1.0 / v) * lambda * g * std::tgamma(1.0 / v) / v;
    const double s_term = 0.5 * std::pow(std::fabs(z) / lambda, v);
    double res = 0.5 + sign1(z) * h *
                           Rf_pgamma(s_term, 1.0 / v, 1.0, /*lower_tail=*/1,
                                     /*log_p=*/0);
    if (!lower_tail)
      res = 1.0 - res;
    out[i] = log_p ? std::log(res) : res;
  }
  return out;
}

arma::vec qged(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    double pi = p[i % p.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    if (!(s > 0.0 && v > 0.0) || !std::isfinite(pi)) {
      out[i] = NA_REAL;
      continue;
    }
    if (log_p)
      pi = std::exp(pi);
    if (!lower_tail)
      pi = 1.0 - pi;
    const double lambda = ged_lambda(v);
    const double qg =
        Rf_qgamma(std::fabs(2.0 * pi - 1.0), 1.0 / v, 1.0, /*lower_tail=*/1,
                  /*log_p=*/0);
    const double q =
        lambda * std::pow(2.0 * qg, 1.0 / v) * sign1(2.0 * pi - 1.0);
    out[i] = q * s + m;
  }
  return out;
}

arma::vec rged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0) {
  if (n < 0 || !(sd > 0.0 && nu > 0.0)) {
    Rcpp::stop("Invalid parameters for rged");
  }
  arma::vec out(static_cast<uword>(n));
  const double lambda = ged_lambda(nu);
  for (int i = 0; i < n; ++i) {
    const double r = R::rgamma(1.0 / nu, 1.0);
    const double z =
        lambda * std::pow(2.0 * r, 1.0 / nu) * sign1(R::runif(0.0, 1.0) - 0.5);
    out[i] = z * sd + mean;
  }
  return out;
}
} // namespace dist_impl
