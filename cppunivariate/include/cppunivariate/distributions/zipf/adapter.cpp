// Rcpp adapter; assembled into the R package, outside the header-only core.
#include "as_span.h"
#include "cppunivariate/distributions/zipf/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlzipf_(const Rcpp::IntegerVector &x, double reltol = 1.220703e-4,
                   int iterlim = 100) {
  if (x.size() == 0) {
    Rcpp::stop("x must have positive length");
  }
  if (x.size() > 0 &&
      std::all_of(x.begin(), x.end(), [](int v) { return v == 0; })) {
    Rcpp::warning("all observations are zero");
  }

  auto res = cppunivariate::zipf_mle(dist_impl::as_ispan(x), reltol, iterlim);
  if (res.status == cppunivariate::ZipfStatus::AllOnes) {
    Rcpp::warning("All observations are equal to 1. The maximum likelihood "
                  "estimator is not unique.");
  }
  if (res.status == cppunivariate::ZipfStatus::ShapeNegative) {
    Rcpp::stop("Optimal shape parameter is less than 0. The maximum likelihood "
               "estimator does not exist.");
  }
  if (res.status != cppunivariate::ZipfStatus::Ok &&
      res.status != cppunivariate::ZipfStatus::AllOnes) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  if (!std::isfinite(res.N) || !std::isfinite(res.shape)) {
    Rcpp::stop("Zipf MLE failed to converge (check data)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["N"] = res.N, Rcpp::_["s"] = res.shape),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
