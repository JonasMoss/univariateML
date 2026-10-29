// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/naka/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlnaka_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                   int iterlim = 100) {
  auto res = cppunivariate::naka_mle(dist_impl::as_span(x), abstol, iterlim);
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("naka MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The Nakagami maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale)) {
    Rcpp::stop("Nakagami MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["scale"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
