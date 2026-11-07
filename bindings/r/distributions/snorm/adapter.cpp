#include "common/distributions/snorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlsnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> sd = R_NilValue,
    Rcpp::Nullable<double> xi = R_NilValue,
    int iterlim = 200) {
  return common_result(umvl_binding::fit_snorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd), common_held(xi), iterlim));
}
