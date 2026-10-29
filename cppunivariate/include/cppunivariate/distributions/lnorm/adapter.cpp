// Rcpp adapter; copied into the assembled R package.

#include "as_span.h"
#include "cppunivariate/distributions/lnorm/mle.hpp"
#include <Rcpp.h>
#include <limits>
#include <cmath>

// [[Rcpp::export]]
Rcpp::List mllnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> meanlog = R_NilValue,
    Rcpp::Nullable<double> sdlog = R_NilValue) {
  Rcpp::CharacterVector fixed(0);
  const double meanlog_held = meanlog.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(meanlog);
  if (meanlog.isNotNull()) {
    if (!(std::isfinite(meanlog_held))) Rcpp::stop("Held `meanlog` must be finite.");
    fixed.push_back("meanlog");
  }
  const double sdlog_held = sdlog.isNull() ? std::numeric_limits<double>::quiet_NaN() : Rcpp::as<double>(sdlog);
  if (sdlog.isNotNull()) {
    if (!(std::isfinite(sdlog_held) && sdlog_held > 0.0)) Rcpp::stop("Held `sdlog` must be positive and finite.");
    fixed.push_back("sdlog");
  }

  auto res = cppunivariate::lognormal_mle(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, meanlog_held, sdlog_held);
  if (!(res.sdlog > 0.0) || !std::isfinite(res.sdlog)) {
    Rcpp::stop("The lnorm maximum likelihood estimator does not exist for constant data: the likelihood is unbounded as scale shrinks to zero.");
  }
  if (!std::isfinite(res.meanlog) || !std::isfinite(res.logLik)) Rcpp::stop("Lognormal likelihood exceeds double precision.");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["meanlog"] = res.meanlog, Rcpp::_["sdlog"] = res.sdlog),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["fixed"] = fixed);
}
