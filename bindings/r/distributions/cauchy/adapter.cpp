#include "common/distributions/cauchy/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlcauchy_(const Rcpp::NumericVector &x, double tol = 1e-10,
                     int iterlim = 100, Rcpp::Nullable<double> location = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_cauchy(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(location), common_held(scale), tol, iterlim));
}
