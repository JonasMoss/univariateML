// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                    int iterlim = 100) {
  auto res = cppunivariate::gamma_mle(dist_impl::as_span(x), abstol, iterlim);
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    Rcpp::stop("The gamma maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.rate) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Gamma MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
