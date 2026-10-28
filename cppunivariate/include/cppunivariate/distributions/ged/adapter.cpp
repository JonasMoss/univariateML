// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/ged/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlged_(const Rcpp::NumericVector &x, double gradtol = 1.220703e-4,
                  int iterlim = 200) {
  auto res = cppunivariate::ged_mle(dist_impl::as_span(x), gradtol, iterlim);
  if (res.status == cppunivariate::GedStatus::InvalidStart) {
    Rcpp::stop("GED MLE requires at least 2 finite observations with positive "
               "variance.");
  }
  if (res.status == cppunivariate::GedStatus::UniformBoundary) {
    Rcpp::stop("GED likelihood is maximised as nu -> Inf, i.e. by the uniform distribution. Use `mlunif` instead.");
  }
  if (!std::isfinite(res.mu) || !std::isfinite(res.sigma) ||
      !std::isfinite(res.nu)) {
    Rcpp::stop("GED MLE failed to converge (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["mean"] = res.mu, Rcpp::_["sd"] = res.sigma,
          Rcpp::_["nu"] = res.nu),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
