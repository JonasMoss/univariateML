#include "as_span.h"
#include "cppunivariate/distributions/hnorm/mle.hpp"
#include <Rcpp.h>
#include <algorithm>
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
  // double below min(x). sigma and logLik are re-profiled at that mu: one
  // ulp of a large min(x) (0.125 at 1e15) is not negligible against sigma,
  // and the reported logLik must be the reference log-likelihood of the
  // reported parameters.
  const double mu = std::nextafter(res.mu, -std::numeric_limits<double>::infinity());
  double largest = 0.0;
  for (double v : x) largest = std::max(largest, v - mu);
  double ss = 0.0;
  for (double v : x) {
    const double z = (v - mu) / largest;
    ss += z * z;
  }
  const double dn = static_cast<double>(x.size());
  const double sigma = largest * std::sqrt(ss / dn);
  const double logLik = dn * (0.5 * std::log(2.0 / std::acos(-1.0)) - std::log(sigma) - 0.5);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mu"] = mu, Rcpp::_["sigma"] = sigma),
      Rcpp::_["logLik"] = logLik);
}
