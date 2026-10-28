// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/zip/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlzip_(const Rcpp::IntegerVector &x) {
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }
  auto res = cppunivariate::zip_mle(dist_impl::as_ispan(x));
  if (res.status == cppunivariate::ZipStatus::InvalidInput)
    Rcpp::stop("Zero-inflated Poisson MLE requires nonempty nonnegative count data.");
  if (res.status != cppunivariate::ZipStatus::Ok)
    Rcpp::stop("Zero-inflated Poisson MLE failed to produce a finite valid fit.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["pi"] = res.pi),
      Rcpp::_["logLik"] = res.logLik);
}
