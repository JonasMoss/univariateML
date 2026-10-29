// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/invgamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlinvgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                       int iterlim = 100) {
  auto res = cppunivariate::invgamma_mle(dist_impl::as_span(x), abstol, iterlim);
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("invgamma MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The inverse gamma maximum likelihood estimator does not exist for constant "
               "data: the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.scale) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Inverse gamma MLE failed");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["alpha"] = res.shape, Rcpp::_["beta"] = res.scale),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
