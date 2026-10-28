#include "as_span.h"
#include "cppunivariate/distributions/lst/mle.hpp"
#include <Rcpp.h>
#include <cmath>
// [[Rcpp::export]]
Rcpp::List mllst_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200) {
  auto r = cppunivariate::lst_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (r.status == cppunivariate::LstStatus::NoMLE)
    Rcpp::stop("Location-scale t MLE on df >= 1 requires at least 3 observations and no value repeated n/2 or more times; scale collapse otherwise prevents a regular MLE.");
  if (r.status == cppunivariate::LstStatus::NormalBoundary)
    Rcpp::stop("The location-scale t likelihood is maximised as df -> Inf. Use `mlnorm` to fit the normal distribution.");
  if (!std::isfinite(r.mu) || !std::isfinite(r.sigma) || !std::isfinite(r.df))
    Rcpp::stop("Location-scale t MLE requires finite data with positive spread.");
  return Rcpp::List::create(
    Rcpp::_["estimates"] = Rcpp::NumericVector::create(
      Rcpp::_["mu"] = r.mu, Rcpp::_["sigma"] = r.sigma, Rcpp::_["df"] = r.df),
    Rcpp::_["logLik"] = r.logLik, Rcpp::_["iterations"] = r.iterations);
}
