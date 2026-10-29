// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/lgser/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mllgser_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> theta = R_NilValue) {
  if(theta.isNotNull() && !std::isfinite(Rcpp::as<double>(theta)))Rcpp::stop("`theta` must be finite and in its parameter domain.");

  auto res = cppunivariate::lgser_mle(dist_impl::as_ispan(x),theta.isNotNull()?Rcpp::as<double>(theta):std::numeric_limits<double>::quiet_NaN());
  if (res.all_ones) {
    Rcpp::stop("All observations equals 1; the maximum likelihood estimator "
               "does not exist.");
  }
  if (!std::isfinite(res.theta)) {
    Rcpp::stop("Logarithmic-series MLE failed");
  }
  if(!std::isfinite(res.logLik))Rcpp::stop("Held theta is outside its domain or incompatible with the observations.");
  Rcpp::CharacterVector fixed(0);if(theta.isNotNull())fixed.push_back("theta");
  return Rcpp::List::create(Rcpp::_["estimates"] = Rcpp::NumericVector::create(
                                Rcpp::_["theta"] = res.theta),
                            Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"]=fixed);
}
