// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/beta/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlbeta_(const Rcpp::NumericVector &x, double reltol = 1.490116e-08,
                   int iterlim = 100) {
  auto res = cppunivariate::beta_mle(dist_impl::as_span(x), reltol, iterlim);
  if (res.status == cppunivariate::BetaStatus::Degenerate) {
    Rcpp::stop("The beta maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as both shapes grow.");
  }
  if (!std::isfinite(res.alpha) || !std::isfinite(res.beta) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Beta MLE failed: the data are too close to a point mass to resolve the shapes in double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.alpha, Rcpp::_["shape2"] = res.beta),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
