// Rcpp adapter; copied into the assembled R package.
#include "as_span.h"
#include "cppunivariate/distributions/gamma/mle.hpp"
#include <Rcpp.h>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::export]]
Rcpp::List mllgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100) {
  const std::size_t n = static_cast<std::size_t>(x.size());
  std::vector<double> y(n);
  double sum_y = 0.0, sum_log_y = 0.0;
  for (std::size_t i = 0; i < n; ++i) {
    y[i] = std::log(x[i]);
    sum_y += y[i];
    sum_log_y += std::log(y[i]);
  }
  auto res =
      cppunivariate::gamma_mle(cppunivariate::span{y.data(), n}, abstol, iterlim);
  const double shapelog = res.shape;
  const double ratelog = res.rate;
  const double dn = static_cast<double>(n);
  const double L = sum_y / dn;
  const double K = sum_log_y / dn;
  const double logLik =
      dn * (shapelog * std::log(ratelog) - std::lgamma(shapelog) +
            (shapelog - 1.0) * K - (ratelog + 1.0) * L);
  return Rcpp::List::create(
      Rcpp::_["estimates"] = Rcpp::NumericVector::create(
          Rcpp::_["shapelog"] = shapelog, Rcpp::_["ratelog"] = ratelog),
      Rcpp::_["logLik"] = logLik, Rcpp::_["iterations"] = res.iterations);
}
