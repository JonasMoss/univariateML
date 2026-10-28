// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/burr/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>

// [[Rcpp::export]]
Rcpp::List mlburr_(const Rcpp::NumericVector &x,
                   Rcpp::Nullable<Rcpp::NumericVector> init = R_NilValue,
                   double abstol = 1.220703e-4, int iterlim = 100) {
  const std::size_t n = static_cast<std::size_t>(x.size());

  // Preserve the actionable Pareto diagnostic before computing a start.
  bool all_ge_one = n > 0;
  for (std::size_t i = 0; i < n; ++i) {
    if (x[i] < 1.0) { all_ge_one = false; break; }
  }
  if (all_ge_one) {
    Rcpp::stop(
        "The Burr maximum likelihood estimator does not exist when all "
        "observations >=1. Use `mlpareto` to fit a Pareto distribution.");
  }

  double c0;
  if (init.isNotNull()) {
    Rcpp::NumericVector iv(init);
    if (iv.size() < 1 || Rcpp::NumericVector::is_na(iv[0]) || iv[0] <= 0) {
      Rcpp::stop("`init` must be a length-1 numeric vector with a positive c");
    }
    c0 = iv[0];
  } else {
    c0 = cppunivariate::burr_mle_start(dist_impl::as_span(x));
    if (!std::isfinite(c0) || c0 <= 0.0) {
      Rcpp::stop("Failed to get a valid initial c for Burr MLE");
    }
  }

  cppunivariate::BurrMLE res =
      cppunivariate::burr_mle_core(dist_impl::as_span(x), c0, abstol, iterlim);
  if (res.status == cppunivariate::BurrStatus::AllGeOne) {
    Rcpp::stop(
        "The Burr maximum likelihood estimator does not exist when all "
        "observations >=1. Use `mlpareto` to fit a Pareto distribution.");
  }
  if (res.status != cppunivariate::BurrStatus::Ok) {
    Rcpp::stop("Burr MLE failed (check inputs)");
  }
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shape1"] = res.shape1, Rcpp::_["shape2"] = res.shape2),
      Rcpp::_["logLik"] = res.logLik, Rcpp::_["iterations"] = res.iterations);
}
