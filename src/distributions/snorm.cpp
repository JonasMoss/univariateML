// Skew normal dpqr
#include "common.h"

namespace dist_impl {

using mlcore::dens::snorm_sigma;

static inline double dsnorm_core(double z, double xi) {
  const double Xi = std::pow(xi, sign1(z));
  const double g = 2.0 / (xi + 1.0 / xi);
  return g * Rf_dnorm4(z / Xi, 0.0, 1.0, 0);
}

arma::vec dsnorm(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi,
           bool log = false) {
  const uword n =
      recycle_length(x.n_elem, mean.n_elem, std::max(sd.n_elem, xi.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double m = mean[i % mean.n_elem];
    const double xv = x[i % x.n_elem];
    if (!(sd_val > 0.0 && xi_val > 0.0) || !std::isfinite(xv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = snorm_sigma(xi_val, mu);
    const double z = (xv - m) / sd_val * sigma + mu;
    double dens = dsnorm_core(z, xi_val) * sigma / sd_val;
    out[i] = log ? std::log(dens) : dens;
  }
  return out;
}

arma::vec psnorm(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  const uword n =
      recycle_length(q.n_elem, mean.n_elem, std::max(sd.n_elem, xi.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double m = mean[i % mean.n_elem];
    const double qv = q[i % q.n_elem];
    if (!(sd_val > 0.0 && xi_val > 0.0) || !std::isfinite(qv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = snorm_sigma(xi_val, mu);
    const double z = (qv - m) / sd_val * sigma + mu;
    const double sig = (z >= 0.0) ? 1.0 : -1.0;
    const double Xi = std::pow(xi_val, sig);
    const double g = 2.0 / (xi_val + 1.0 / xi_val);
    const double base = Rf_pnorm5(-std::fabs(z) / Xi, 0.0, 1.0, 1, 0);
    out[i] = (z >= 0.0 ? 1.0 : 0.0) - sig * g * Xi * base;
  }
  return out;
}

arma::vec qsnorm(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  const uword n =
      recycle_length(p.n_elem, mean.n_elem, std::max(sd.n_elem, xi.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double xi_val = xi[i % xi.n_elem];
    const double sd_val = sd[i % sd.n_elem];
    const double m = mean[i % mean.n_elem];
    double pv = p[i % p.n_elem];
    if (!(sd_val > 0.0 && xi_val > 0.0) || !std::isfinite(pv)) {
      out[i] = NA_REAL;
      continue;
    }
    double mu = 0.0;
    const double sigma = snorm_sigma(xi_val, mu);
    const double g = 2.0 / (xi_val + 1.0 / xi_val);
    const double pxi = pv - (1.0 / (1.0 + xi_val * xi_val));
    const double sig = sign1(pxi);
    const double Xi = std::pow(xi_val, sig);
    const double adj_p = (heaviside(pxi) - sig * pv) / (g * Xi);
    const double q = Rf_qnorm5(adj_p, 0.0, Xi, 1, 0);
    out[i] = (-sig * q - mu) * sd_val / sigma + m;
  }
  return out;
}

arma::vec rsnorm(int n, double mean = 0.0, double sd = 1.0, double xi = 1.5) {
  if (n < 0 || !(sd > 0.0 && xi > 0.0)) {
    Rcpp::stop("Invalid parameters for rsnorm");
  }
  arma::vec out(static_cast<uword>(n));
  double mu = 0.0;
  const double sigma = snorm_sigma(xi, mu);
  const double weight = xi / (xi + 1.0 / xi);
  for (int i = 0; i < n; ++i) {
    const double z = R::runif(-weight, 1.0 - weight);
    const double Xi = std::pow(xi, sign1(z));
    const double base = -std::fabs(R::rnorm(0.0, 1.0)) / Xi * sign1(z);
    const double scaled = (base - mu) / sigma;
    out[i] = scaled * sd + mean;
  }
  return out;
}
} // namespace dist_impl
