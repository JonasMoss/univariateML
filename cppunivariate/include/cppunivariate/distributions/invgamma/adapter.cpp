// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = cppunivariate::invgamma_mle(dist_impl::as_span(x), abstol, iterlim);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.shape, Rcpp::_["beta"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
