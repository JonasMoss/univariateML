#include "common/distributions/pareto/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpareto_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> a = R_NilValue,
    Rcpp::Nullable<double> b = R_NilValue) {
  return common_result(umvl_binding::fit_pareto(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(a), common_held(b)));
}
