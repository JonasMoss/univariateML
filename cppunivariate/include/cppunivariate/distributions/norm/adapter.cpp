// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/norm/mle.hpp"
#include <Rcpp.h>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mlnorm_(const Rcpp::NumericVector &x) {
  auto res =
      cppunivariate::norm_mle(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mean, Rcpp::_["sd"] = res.sd),
      Rcpp::_["logLik"] = res.logLik);
}
