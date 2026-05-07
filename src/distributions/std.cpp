// Student-t dpqr
#include "common.h"

namespace dist_impl {

//' @rdname distributions_cpp
//' @export
// [[Rcpp::export]]
arma::vec dstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool log = false) {
  const uword n = recycle_length(x.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double xi = x[i % x.n_elem];
    if (!(s > 0.0 && v > 2.0) || !std::isfinite(xi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double scale = std::sqrt(v / (v - 2.0));
    const double z = (xi - m) / s;
    const double logdens = mlcore::special::dt(z * scale, v, /*log=*/true) +
                           std::log(scale / s);
    out[i] = log ? logdens : std::exp(logdens);
  }
  return out;
}

//' @rdname distributions_cpp
//' @export
// [[Rcpp::export]]
arma::vec pstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(q.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    const double qi = q[i % q.n_elem];
    if (!(s > 0.0 && v > 2.0) || !std::isfinite(qi)) {
      out[i] = NA_REAL;
      continue;
    }
    const double scale = std::sqrt(v / (v - 2.0));
    const double z = (qi - m) / s;
    out[i] = mlcore::special::pt(z * scale, v, lower_tail, log_p);
  }
  return out;
}

//' @rdname distributions_cpp
//' @export
// [[Rcpp::export]]
arma::vec qstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
         bool lower_tail = true, bool log_p = false) {
  const uword n = recycle_length(p.n_elem, mean.n_elem, std::max(sd.n_elem, nu.n_elem));
  arma::vec out(n);
  for (uword i = 0; i < n; ++i) {
    double pi = p[i % p.n_elem];
    const double m = mean[i % mean.n_elem];
    const double s = sd[i % sd.n_elem];
    const double v = nu[i % nu.n_elem];
    if (!(s > 0.0 && v > 2.0) || !std::isfinite(pi)) {
      out[i] = NA_REAL;
      continue;
    }
    if (log_p)
      pi = std::exp(pi);
    if (!lower_tail)
      pi = 1.0 - pi;
    const double scale = std::sqrt(v / (v - 2.0));
    const double z = mlcore::special::qt(pi, v, /*lower_tail=*/true,
                                         /*log_p=*/false);
    out[i] = z / scale * s + m;
  }
  return out;
}

//' @rdname distributions_cpp
//' @export
// [[Rcpp::export]]
arma::vec rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  if (n < 0 || !(sd > 0.0 && nu > 2.0)) {
    Rcpp::stop("Invalid parameters for rstd");
  }
  arma::vec out(static_cast<uword>(n));
  for (int i = 0; i < n; ++i) {
    const double z = R::rt(nu);
    const double scale = std::sqrt(nu / (nu - 2.0));
    out[i] = z / scale * sd + mean;
  }
  return out;
}
} // namespace dist_impl
