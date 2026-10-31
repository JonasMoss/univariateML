#include "common/distributions/power/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpower_(const Rcpp::NumericVector &x, double epsilon = 1.490116e-08,
    Rcpp::Nullable<double> alpha = R_NilValue,
    Rcpp::Nullable<double> beta = R_NilValue) {
  return common_result(umvl_binding::fit_power(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(alpha), common_held(beta), epsilon));
}
