#include "common/distributions/invtrgamma/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlinvtrgamma_(const Rcpp::NumericVector &x,
                         Rcpp::Nullable<double> shape1 = R_NilValue,
                         Rcpp::Nullable<double> shape2 = R_NilValue,
                         double reltol = 1e-10, int iterlim = 100, Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_invtrgamma(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), common_held(scale), reltol, iterlim));
}
