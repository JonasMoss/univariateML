// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/pois/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlpois_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue) {
  if(lambda.isNotNull() && !std::isfinite(Rcpp::as<double>(lambda)))Rcpp::stop("`lambda` must be finite and in its parameter domain.");

  auto res = cppunivariate::pois_mle(
      cppunivariate::ispan{x.begin(), static_cast<std::size_t>(x.size())}, lambda.isNotNull()?Rcpp::as<double>(lambda):std::numeric_limits<double>::quiet_NaN());
  if(!std::isfinite(res.logLik))Rcpp::stop("Held lambda is outside its domain or incompatible with the observations.");
  Rcpp::CharacterVector fixed(0);if(lambda.isNotNull())fixed.push_back("lambda");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["lambda"] = res.lambda),
                            Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"]=fixed);
}
