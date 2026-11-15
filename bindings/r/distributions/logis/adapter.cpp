#include "common/distributions/logis/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mllogis_(const Rcpp::NumericVector &x, double tol = 1e-10,
                    int iterlim = 100, Rcpp::Nullable<double> location = R_NilValue, Rcpp::Nullable<double> scale = R_NilValue) {
  return common_result(umvl_binding::fit_logis(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(location), common_held(scale), tol, iterlim));
}
