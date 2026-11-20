#include "common/distributions/paralogis/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlparalogis_(const Rcpp::NumericVector &x,
                        double tol = 1e-10, int iterlim = 100,
                        Rcpp::Nullable<double> scale = R_NilValue,
                        Rcpp::Nullable<double> shape = R_NilValue) {
  return common_result(umvl_binding::fit_paralogis(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape), common_held(scale), tol, iterlim));
}
