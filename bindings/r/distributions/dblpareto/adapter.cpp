#include "common/distributions/dblpareto/fit.hpp"
#include "common_fit.h"

// [[Rcpp::export]]
Rcpp::List mldblpareto_(const Rcpp::NumericVector& x,
    Rcpp::Nullable<double> scale = R_NilValue,
    Rcpp::Nullable<double> left_rate = R_NilValue,
    Rcpp::Nullable<double> right_rate = R_NilValue) {
  return common_result(umvl_binding::fit_dblpareto(
      cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())},
      common_held(scale), common_held(left_rate), common_held(right_rate)));
}
