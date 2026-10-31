#include "common/distributions/pareto3/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpareto3_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> shape = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_pareto3(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(scale), common_held(min), reltol, iterlim));
}
