// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invburr/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlinvburr_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                      int iterlim = 100) {
  auto res = cppunivariate::invburr_mle(dist_impl::as_span(x), abstol, iterlim);
  if (res.status == cppunivariate::InvBurrStatus::AllLeOne) {
    Rcpp::stop(
        "The inverse Burr maximum likelihood estimator does not exist when all "
        "observations <=1. Use `mlpower` to fit a power distribution.");
  }
  if (res.status != cppunivariate::InvBurrStatus::Ok) {
    Rcpp::stop("Inverse Burr MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
