#include "common/distributions/norm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlnorm_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> mean = R_NilValue, Rcpp::Nullable<double> sd = R_NilValue) {
  return common_result(umvl_binding::fit_norm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(sd)));
}
