#include "common/distributions/dweibull/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mldweibull_(const Rcpp::NumericVector &x,
                       Rcpp::Nullable<double> shape1 = R_NilValue,
                       Rcpp::Nullable<double> shape2 = R_NilValue,
                       double reltol = 1e-12, int iterlim = 200) {
  return common_result(umvl_binding::fit_dweibull(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), reltol, iterlim));
}
