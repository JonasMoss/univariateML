#include "as_span.h"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List mlhnorm_(const Rcpp::NumericVector &x) {
  const auto res = cppunivariate::hnorm_mle(dist_impl::as_span(x));
  if (res.status == cppunivariate::HnormStatus::DegenerateData)
    Rcpp::stop("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE.");
  if (res.status != cppunivariate::HnormStatus::Ok)
    Rcpp::stop("Translated half-normal MLE requires finite data with a representable range.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = res.mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik);
}
