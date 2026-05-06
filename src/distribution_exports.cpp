// Aggregator for distribution exports; pulls in one file per density.
// [[Rcpp::depends(RcppArmadillo)]]

#include "distributions/common.h"

// Pull in the actual definitions.
#include "distributions/nakagami.cpp"
#include "distributions/logitnorm.cpp"
#include "distributions/zipf.cpp"
#include "distributions/ged.cpp"
#include "distributions/sged.cpp"
#include "distributions/snorm.cpp"
#include "distributions/std.cpp"
#include "distributions/sstd.cpp"

// Exported wrappers forwarding to dist_impl.
// [[Rcpp::export]]
arma::vec dnaka(const arma::vec &x, const arma::vec &shape, const arma::vec &scale, bool log = false) {
  return dist_impl::dnaka(x, shape, scale, log);
}
// [[Rcpp::export]]
arma::vec pnaka(const arma::vec &q, const arma::vec &shape, const arma::vec &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pnaka(q, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qnaka(const arma::vec &p, const arma::vec &shape, const arma::vec &scale,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qnaka(p, shape, scale, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rnaka(int n, double shape, double scale) {
  return dist_impl::rnaka(n, shape, scale);
}

// [[Rcpp::export]]
arma::vec dlogitnorm(const arma::vec &x, const arma::vec &mu, const arma::vec &sigma, bool log = false) {
  return dist_impl::dlogitnorm(x, mu, sigma, log);
}
// [[Rcpp::export]]
arma::vec plogitnorm(const arma::vec &q, const arma::vec &mu, const arma::vec &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::plogitnorm(q, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qlogitnorm(const arma::vec &p, const arma::vec &mu, const arma::vec &sigma,
                     bool lower_tail = true, bool log_p = false) {
  return dist_impl::qlogitnorm(p, mu, sigma, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rlogitnorm(int n, double mu = 0.0, double sigma = 1.0) {
  return dist_impl::rlogitnorm(n, mu, sigma);
}

// [[Rcpp::export]]
arma::vec dzipf(const arma::vec &x, const arma::vec &N, const arma::vec &s, bool log = false) {
  return dist_impl::dzipf(x, N, s, log);
}
// [[Rcpp::export]]
arma::vec pzipf(const arma::vec &q, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::pzipf(q, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qzipf(const arma::vec &p, const arma::vec &N, const arma::vec &s,
                bool lower_tail = true, bool log_p = false) {
  return dist_impl::qzipf(p, N, s, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rzipf(int n, double N, double s) { return dist_impl::rzipf(n, N, s); }

// [[Rcpp::export]]
arma::vec dged(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool log = false) {
  return dist_impl::dged(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
arma::vec pged(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pged(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qged(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qged(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0) {
  return dist_impl::rged(n, mean, sd, nu);
}

// [[Rcpp::export]]
arma::vec dsged(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi, bool log = false) {
  return dist_impl::dsged(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
arma::vec psged(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::psged(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec qsged(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::qsged(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec rsged(int n, double mean = 0.0, double sd = 1.0, double nu = 2.0, double xi = 1.5) {
  return dist_impl::rsged(n, mean, sd, nu, xi);
}

// [[Rcpp::export]]
arma::vec dsnorm(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi,
                 bool log = false) {
  return dist_impl::dsnorm(x, mean, sd, xi, log);
}
// [[Rcpp::export]]
arma::vec psnorm(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  return dist_impl::psnorm(q, mean, sd, xi);
}
// [[Rcpp::export]]
arma::vec qsnorm(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &xi) {
  return dist_impl::qsnorm(p, mean, sd, xi);
}
// [[Rcpp::export]]
arma::vec rsnorm(int n, double mean = 0.0, double sd = 1.0, double xi = 1.5) {
  return dist_impl::rsnorm(n, mean, sd, xi);
}

// [[Rcpp::export]]
arma::vec dstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool log = false) {
  return dist_impl::dstd(x, mean, sd, nu, log);
}
// [[Rcpp::export]]
arma::vec pstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::pstd(q, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec qstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
               bool lower_tail = true, bool log_p = false) {
  return dist_impl::qstd(p, mean, sd, nu, lower_tail, log_p);
}
// [[Rcpp::export]]
arma::vec rstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0) {
  return dist_impl::rstd(n, mean, sd, nu);
}

// [[Rcpp::export]]
arma::vec dsstd(const arma::vec &x, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi, bool log = false) {
  return dist_impl::dsstd(x, mean, sd, nu, xi, log);
}
// [[Rcpp::export]]
arma::vec psstd(const arma::vec &q, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::psstd(q, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec qsstd(const arma::vec &p, const arma::vec &mean, const arma::vec &sd, const arma::vec &nu,
                const arma::vec &xi) {
  return dist_impl::qsstd(p, mean, sd, nu, xi);
}
// [[Rcpp::export]]
arma::vec rsstd(int n, double mean = 0.0, double sd = 1.0, double nu = 5.0, double xi = 1.5) {
  return dist_impl::rsstd(n, mean, sd, nu, xi);
}
