// Zipf distribution: Rcpp::NumericVector wrappers around mlcore::distributions.
#include "common.h"
#include "rng_r.h"
#include "../ml/distributions/zipf.hpp"

namespace dist_impl {

Rcpp::NumericVector dzipf(const Rcpp::NumericVector &x, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s,
                bool log = false) {
  const std::size_t n = recycle_length(x.size(), N.size(), s.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double xi = x[i % x.size()];
    const double Ni = N[i % N.size()];
    const double si = s[i % s.size()];
    const double y = mlcore::distributions::dzipf(xi, Ni, si, log);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector pzipf(const Rcpp::NumericVector &q, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s,
                bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(q.size(), N.size(), s.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double qi = q[i % q.size()];
    const double Ni = N[i % N.size()];
    const double si = s[i % s.size()];
    const double y =
        mlcore::distributions::pzipf(qi, Ni, si, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector qzipf(const Rcpp::NumericVector &p, const Rcpp::NumericVector &N, const Rcpp::NumericVector &s,
                bool lower_tail = true, bool log_p = false) {
  const std::size_t n = recycle_length(p.size(), N.size(), s.size());
  Rcpp::NumericVector out(n);
  for (std::size_t i = 0; i < n; ++i) {
    const double pi = p[i % p.size()];
    const double Ni = N[i % N.size()];
    const double si = s[i % s.size()];
    const double y =
        mlcore::distributions::qzipf(pi, Ni, si, lower_tail, log_p);
    out[i] = std::isnan(y) ? NA_REAL : y;
  }
  return out;
}

Rcpp::NumericVector rzipf(int n, double N, double s) {
  if (n < 0 || !(N > 0.0 && s > 0.0) || N != std::floor(N)) {
    Rcpp::stop("Invalid parameters for rzipf");
  }
  const int Nin = static_cast<int>(N);
  const auto cum = mlcore::distributions::zipf_cdf(Nin, s);
  Rcpp::NumericVector out(static_cast<std::size_t>(n));
  RRng rng;
  for (int i = 0; i < n; ++i) {
    out[i] =
        static_cast<double>(mlcore::distributions::rzipf_from_cdf(rng, cum));
  }
  return out;
}

} // namespace dist_impl
