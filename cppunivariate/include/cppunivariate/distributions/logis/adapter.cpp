// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/logis/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mllogis_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                    int iterlim = 100) {
  cppunivariate::LogisticResult res =
      cppunivariate::logistic_mle(dist_impl::as_span(x), abstol, iterlim);
  if (!std::isfinite(res.mu) || !std::isfinite(res.scale) || !(res.scale > 0.0))
    Rcpp::stop("Logistic MLE failed: non-finite or invalid parameters");
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["location"] = res.mu, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
