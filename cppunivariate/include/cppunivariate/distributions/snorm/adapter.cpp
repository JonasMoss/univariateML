// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/snorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlsnorm_(const Rcpp::NumericVector &x) {
  auto res = cppunivariate::snorm_mle(dist_impl::as_span(x), 1.220703e-4, 200);
  if (res.status == cppunivariate::SnormStatus::RightHalfNormalBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-normal (mode at the sample minimum); the skew-normal MLE does not exist. Use `mlhnorm` to fit a translated half-normal.");
  }
  if (res.status == cppunivariate::SnormStatus::LeftHalfNormalBoundary) {
    Rcpp::stop("The likelihood is maximised by a translated half-normal (mode at the sample maximum); the skew-normal MLE does not exist. Use `mlhnorm` on `-x` (a half-normal reflected at the sample maximum).");
  }
  if (res.status == cppunivariate::SnormStatus::InvalidStart) {
    Rcpp::stop("Skew-normal MLE requires at least 2 finite observations with "
               "positive variance.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.xi)) {
    Rcpp::stop("Skew-normal MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["xi"] = res.xi),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
