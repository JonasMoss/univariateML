#include "common/distributions/weibull/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlweibull_(const Rcpp::NumericVector &x, Rcpp::Nullable<double> shape0 = R_NilValue,
                         double reltol = 1.220703e-4,
                      int iterlim = 100,
    Rcpp::Nullable<double> shape = R_NilValue,
    Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_weibull(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(scale), common_held(shape0), reltol, iterlim));
}
