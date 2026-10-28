// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mllnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::lognormal_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["meanlog"] = res.meanlog, Rcpp::_["sdlog"] = res.sdlog),
      Rcpp::_["logLik"] = res.logLik);
}
