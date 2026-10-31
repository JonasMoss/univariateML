#include "common/distributions/beta/fit.hpp"
#include "common_fit.h"
// [[Rcpp::export]]
Rcpp::List mlbeta_(const Rcpp::NumericVector &x, double reltol = 1.490116e-08,
                   int iterlim = 100,
    Rcpp::Nullable<double> shape1 = R_NilValue,
    Rcpp::Nullable<double> shape2 = R_NilValue) {
  return common_result(umvl_binding::fit_beta(cppunivariate::span{x.begin(), static_cast<std::size_t>(x.size())}, common_held(shape1), common_held(shape2), reltol, iterlim));
}
