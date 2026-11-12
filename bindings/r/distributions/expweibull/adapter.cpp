#include "common/distributions/expweibull/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlexpweibull_(const Rcpp::NumericVector& x,Rcpp::Nullable<double> scale=R_NilValue,
    Rcpp::Nullable<double> shape=R_NilValue,Rcpp::Nullable<double> exponent=R_NilValue,
    double tol=1e-10,int iterlim=100) {
  return common_result(umvl_binding::fit_expweibull(
    cppunivariate::span{x.begin(),static_cast<std::size_t>(x.size())},
    common_held(scale),common_held(shape),common_held(exponent),tol,iterlim));
}
