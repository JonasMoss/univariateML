#include "common/distributions/invgauss/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlinvgauss_(const Rcpp::NumericVector &x,
    Rcpp::Nullable<double> mean = R_NilValue,
    Rcpp::Nullable<double> shape = R_NilValue) {
  return common_result(umvl_binding::fit_invgauss(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(mean), common_held(shape)));
}
