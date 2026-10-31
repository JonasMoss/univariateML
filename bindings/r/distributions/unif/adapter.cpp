#include "common/distributions/unif/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlunif_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> min = R_NilValue,
    Rcpp::Nullable<double> max = R_NilValue) {
  return common_result(umvl_binding::fit_unif(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(min), common_held(max)));
}
