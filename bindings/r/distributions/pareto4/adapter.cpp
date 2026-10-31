#include "common/distributions/pareto4/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlpareto4_(const Rcpp::NumericVector &x, double min,
 double reltol = 1e-10, int iterlim = 1000, Rcpp::Nullable<double> scale = R_NilValue, Rcpp::Nullable<double> shape1 = R_NilValue, Rcpp::Nullable<double> shape2 = R_NilValue) {
  return common_result(umvl_binding::fit_pareto4(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), common_held(scale), common_held(min), reltol, iterlim));
}
