// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/llogis/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mlllogis_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100) {
  cppunivariate::LogLogisticResult res =
      cppunivariate::loglogistic_mle(dist_impl::as_span(x), abstol, iterlim);
  if (!(res.shape>0.0) || !(res.rate>0.0) || !std::isfinite(res.shape) || !std::isfinite(res.rate) || !std::isfinite(res.logLik)) {
    if (std::all_of(x.begin(),x.end(),[&](double v){return v==x[0];}))
      Rcpp::stop("The log-logistic maximum likelihood estimator does not exist for constant data: the likelihood is unbounded.");
    Rcpp::stop("Log-logistic MLE failed: data exceed double precision.");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape"] = res.shape, Rcpp::_["rate"] = res.rate),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
