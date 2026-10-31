#include "common/distributions/lnorm/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllnorm_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> meanlog = R_NilValue,
    Rcpp::Nullable<double> sdlog = R_NilValue) {
  return common_result(umvl_binding::fit_lnorm(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(meanlog), common_held(sdlog)));
}
