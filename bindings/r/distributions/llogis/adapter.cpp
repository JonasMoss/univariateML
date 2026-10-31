#include "common/distributions/llogis/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlllogis_(const Rcpp::NumericVector &x, double abstol = 1.220703e-4,
                     int iterlim = 100,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_llogis(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(scale), abstol, iterlim));
}
