// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/invgauss/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlinvgauss_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::invgauss_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  if (!(res.lambda > 0.0) || !std::isfinite(res.lambda) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Inverse Gaussian MLE failed: constant data have no attained maximum; other data may exceed double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["shape"] = res.lambda),
      Rcpp::_["logLik"] = res.logLik);
}
