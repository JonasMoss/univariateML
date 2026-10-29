// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <algorithm>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mllgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100) {
  const std::size_t n = static_cast<std::size_t>(x.size());
  std::vector<double> y(n);
  double sum_y = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = std::log(x[i]);
    sum_y += y[i];
  }
  auto res =
      cppunivariate::gamma_mle(cppunivariate::span{y.data(), n}, abstol, iterlim);
  if (res.status == cppunivariate::GammaStatus::Degenerate) {
    if (!std::all_of(x.begin(), x.end(), [&](double v) { return v == x[0]; }))
      Rcpp::stop("lgamma MLE failed: transformed observations exceed double precision.");
    Rcpp::stop("The loggamma maximum likelihood estimator does not exist for constant data: "
               "the likelihood is unbounded as the shape grows.");
  }
  if (!std::isfinite(res.shape) || !std::isfinite(res.rate) || !std::isfinite(res.logLik)) {
    Rcpp::stop("Loggamma MLE failed");
  }
  const double shapelog = res.shape;
  const double ratelog = res.rate;
  // log(X) is gamma: log f(x) = log f_gamma(log x) - log x.
  const double logLik = res.logLik - sum_y;
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shapelog"] = shapelog, Rcpp::_["ratelog"] = ratelog),
      Rcpp::_["logLik"] = logLik, Rcpp::_["iterations"] = res.iterations);
}
