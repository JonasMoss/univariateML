#include "common/distributions/lgamma/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllgamma_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100,
    Rcpp::Nullable<double> shapelog = R_NilValue,
    Rcpp::Nullable<double> ratelog = R_NilValue) {
  return common_result(umvl_binding::fit_lgamma(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shapelog), common_held(ratelog), abstol, iterlim));
}
