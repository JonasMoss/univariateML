#include <Rcpp.h>
#include "cppunivariate/special/special.hpp"

using namespace Rcpp;

// Expose special functions for testthat comparisons against Rmath.

// [[Rcpp::export]]
NumericVector special_digamma_cpp(const NumericVector &x) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::digamma(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_trigamma_cpp(const NumericVector &x) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::trigamma(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_tetragamma_cpp(const NumericVector &x) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::tetragamma(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_lgamma_cpp(const NumericVector &x) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::lgamma(x[i]);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_lbeta_cpp(const NumericVector &a,
                                const NumericVector &b) {
  const R_xlen_t n = std::max(a.size(), b.size());
  NumericVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const double ai = a[i % a.size()];
    const double bi = b[i % b.size()];
    out[i] = cppunivariate::special::lbeta(ai, bi);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_dnorm_cpp(const NumericVector &x, double mean = 0.0,
                                double sd = 1.0, bool log = false) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::dnorm(x[i], mean, sd, log);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_dpois_cpp(const IntegerVector &x, double lambda,
                                bool log = false) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::dpois(static_cast<double>(x[i]), lambda, log);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_dgeom_cpp(const IntegerVector &x, double prob,
                                bool log = false) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::dgeom(static_cast<double>(x[i]), prob, log);
  }
  return out;
}

// [[Rcpp::export]]
NumericVector special_psigamma_cpp(const NumericVector &x, int deriv) {
  NumericVector out(x.size());
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    out[i] = cppunivariate::special::psigamma(x[i], deriv);
  }
  return out;
}
