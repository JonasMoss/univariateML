// Skew Student-t dpqr
#include "common.h"

namespace dist_impl {

static inline double sstd_sigma(double nu, double xi, double &mu_out) {
  const double m1 =
      std::tgamma(0.5 * (nu - 1.0)) / std::tgamma(0.5 * nu) * std::sqrt(nu / M_PI);
  mu_out = m1 * (xi - 1.0 / xi);
  return std::sqrt((1.0 - m1 * m1) * (xi * xi + 1.0 / (xi * xi)) +
                   2.0 * m1 * m1 - 1.0);
}

static inline double dsstd_core(double z, double nu, double xi) {
  const double Xi = std::pow(xi, sign1(z));
  const double g = 2.0 / (xi + 1.0 / xi);
  return g * dstd(scalar_vec(z / Xi), scalar_vec(0.0), scalar_vec(1.0),
                  scalar_vec(nu), false)[0];
}

arma::vec dsstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
          const arma::vec &xi, bool log = false) {
  const uword n =
      recycle_length(x.n_elem, mean.n_elem,
                     std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double nu_val = nu[i % nu.n_elem];
    const double m = mean[i % mean.n_elem];
    const double xv = x[i % x.n_elem];
    if (!(sd_val > 0.0 && nu_val > 2.0 && xi_val > 0.0) ||
        !std::isfinite(xv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = sstd_sigma(nu_val, xi_val, mu);
    const double z = (xv - m) / sd_val * sigma + mu;
    double dens = dsstd_core(z, nu_val, xi_val) * sigma / sd_val;
    out[i] = log ? std::log(dens) : dens;
  }
  return out;
}

arma::vec psstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
          const arma::vec &xi) {
  const uword n =
      recycle_length(q.n_elem, mean.n_elem,
                     std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double nu_val = nu[i % nu.n_elem];
    const double m = mean[i % mean.n_elem];
    const double qv = q[i % q.n_elem];
    if (!(sd_val > 0.0 && nu_val > 2.0 && xi_val > 0.0) ||
        !std::isfinite(qv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = sstd_sigma(nu_val, xi_val, mu);
    const double z = (qv - m) / sd_val * sigma + mu;
    const double sig = (z >= 0.0) ? 1.0 : -1.0;
    const double Xi = std::pow(xi_val, sig);
    const double g = 2.0 / (xi_val + 1.0 / xi_val);
    const double base =
        pstd(scalar_vec(-std::fabs(z) / Xi), scalar_vec(0.0),
             scalar_vec(1.0), scalar_vec(nu_val), true, false)[0];
    out[i] = (z >= 0.0 ? 1.0 : 0.0) - sig * g * Xi * base;
  }
  return out;
}

arma::vec qsstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
          const arma::vec &xi) {
  const uword n =
      recycle_length(p.n_elem, mean.n_elem,
                     std::max(sd.n_elem, std::max(nu.n_elem, xi.n_elem)));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double nu_val = nu[i % nu.n_elem];
    const double m = mean[i % mean.n_elem];
    double pv = p[i % p.n_elem];
    if (!(sd_val > 0.0 && nu_val > 2.0 && xi_val > 0.0) ||
        !std::isfinite(pv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = sstd_sigma(nu_val, xi_val, mu);
    const double g = 2.0 / (xi_val + 1.0 / xi_val);
    const double pxi = pv - (1.0 / (1.0 + xi_val * xi_val));
    const double sig = sign1(pxi);
    const double Xi = std::pow(xi_val, sig);
    const double adj_p = (heaviside(pxi) - sig * pv) / (g * Xi);
    const double q =
        qstd(scalar_vec(adj_p), scalar_vec(0.0), scalar_vec(1.0),
             scalar_vec(nu_val), true, false)[0];
    out[i] = (-sig * q - mu) * sd_val / sigma + m;
  }
  return out;
}

arma::vec rsstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0,
          double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && nu > 2.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsstd");
  }
  arma::vec out(static_cast<uword>(n));
  double mu = 0.0;
  const double sigma = sstd_sigma(nu, xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  for (int i = 0; i < n; ++i) {
    const double z = R::runif(-weight, 1.0 - weight);
    const double Xi = std::pow(xi, sign1(z));
    const double base = -std::fabs(rstd(1, 0.0, 1.0, nu)[0]) / Xi * sign1(z);
    const double scaled = (base - mu) / sigma;
    out[i] = scaled * sd + mean;
  }
  return out;
}
} // namespace dist_impl
