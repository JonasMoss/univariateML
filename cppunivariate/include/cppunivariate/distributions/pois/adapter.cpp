// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/pois/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlpois_(const Rcpp::IntegerVector &x) {
  auto res = cppunivariate::pois_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())});
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["lambda"] = res.lambda),
                            Rcpp::_["logLik"] = res.logLik);
}
