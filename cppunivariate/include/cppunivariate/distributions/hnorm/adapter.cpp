#include "as_span.h"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlhnorm_(const Rcpp::NumericVector &x) {
  const auto res = cppunivariate::hnorm_mle(dist_impl::as_span(x));
  if (res.status == cppunivariate::HnormStatus::DegenerateData)
    Rcpp::stop("Translated half-normal MLE requires at least two observations with distinct values; degenerate data have no positive-scale MLE.");
  if (res.status != cppunivariate::HnormStatus::Ok)
    Rcpp::stop("Translated half-normal MLE requires finite data with a representable range.");
  // The R reference density, extraDistr::dtnorm(x, mean = mu, sd = sigma,
  // a = mu), excludes its lower endpoint, so the boundary MLE mu = min(x)
  // would give the smallest observation density 0. Report the largest
  // double below min(x); sigma and logLik are unchanged to rounding.
  const double mu = std::nextafter(res.mu, -std::numeric_limits<double>::infinity());
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = mu, Rcpp::_["sigma"] = res.sigma),
      Rcpp::_["logLik"] = res.logLik);
}
