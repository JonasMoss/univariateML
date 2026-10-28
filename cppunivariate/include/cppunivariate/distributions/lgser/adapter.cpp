// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/lgser/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mllgser_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::lgser_mle(dist_impl::as_ispan(x));
  if (res.all_ones) {
    Rcpp::stop("All observations equals 1; the maximum likelihood estimator "
               "does not exist.");
  }
  if (!std::isfinite(res.theta)) {
    Rcpp::stop("Logarithmic-series MLE failed");
  }
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["theta"] = res.theta),
                            Rcpp::_["logLik"] = res.logLik);
}
