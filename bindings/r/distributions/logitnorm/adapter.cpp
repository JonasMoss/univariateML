#include "common/distributions/logitnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllogitnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mu = R_NilValue,
    Rcpp::Nullable<double> sigma = R_NilValue) {
  return common_result(umvl_binding::fit_logitnorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mu), common_held(sigma)));
}
