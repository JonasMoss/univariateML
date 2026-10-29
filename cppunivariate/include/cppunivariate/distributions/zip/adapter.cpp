// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/zip/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlzip_(const Rcpp::IntegerVector &x, Rcpp::Nullable<double> lambda = R_NilValue, Rcpp::Nullable<double> pi = R_NilValue) {
  if(lambda.isNotNull() && !std::isfinite(Rcpp::as<double>(lambda)))Rcpp::stop("`lambda` must be finite and in its parameter domain.");
  if(pi.isNotNull() && !std::isfinite(Rcpp::as<double>(pi)))Rcpp::stop("`pi` must be finite and in its parameter domain.");

  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }
  auto res = cppunivariate::zip_mle(dist_impl::as_ispan(x),lambda.isNotNull()?Rcpp::as<double>(lambda):std::numeric_limits<double>::quiet_NaN(),pi.isNotNull()?Rcpp::as<double>(pi):std::numeric_limits<double>::quiet_NaN());
  if(res.status==cppunivariate::ZipStatus::ZeroLimit)Rcpp::stop("With pi held below one and all-zero data, lambda tends to zero, undefined by the reference; use `mlpois`.");
  if (res.status == cppunivariate::ZipStatus::InvalidInput)
    Rcpp::stop("Zero-inflated Poisson MLE requires nonempty nonnegative count data, lambda positive and finite, pi in [0,1], and compatible holds.");
  if (res.status != cppunivariate::ZipStatus::Ok)
    Rcpp::stop("Zero-inflated Poisson MLE failed to produce a finite valid fit.");
  Rcpp::CharacterVector fixed(0);
  if(lambda.isNotNull())fixed.push_back("lambda");
  if(pi.isNotNull())fixed.push_back("pi");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["lambda"] = res.lambda, Rcpp::_["pi"] = res.pi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"]=fixed);
}
